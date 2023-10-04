type alias = string * Tokens.typ list
type union = Tokens.typ list
type keyword = VAR | CONST | LET | FUNCTION | TYPE
type field = string * union

let key_as_str = function
  | VAR -> "var"
  | CONST -> "const"
  | LET -> "let"
  | FUNCTION -> "function"
  | TYPE -> "type"

type blocks =
  | KEYWORD of keyword
  | IDENT of string
  | ALIAS of alias
  | UNION of union
  | FIELD of field
  | PAREN of blocks list
  | BRACKET of blocks list
  | BRACE of blocks list
  | I of Tokens.tokens
  | NONE

type inners = B of blocks | T of Tokens.tokens

let rec flatten_into list = function
  | [] -> list
  | h :: t -> flatten_into t (h :: list)

let map lst func =
  let rec aux l f acc =
    match l with
    | [] -> Base.List.rev acc
    | h :: t -> aux t f (f h :: acc)
  in
  aux lst func []

let squash func arg =
  let a, b = func arg in
  Base.Printf.sprintf "%s %s" a b

let print_block = function
  | KEYWORD k -> ("Keyword:\n", key_as_str k)
  | IDENT i -> ("Ident:", i)
  | ALIAS a ->
      let c, tlist = a in
      (Base.Printf.sprintf "alias %s" c, Tokens.typ_as_str (Tokens.UNION tlist))
  | UNION u -> ("Union:\n", Tokens.typ_as_str (Tokens.UNION u))
  | FIELD f ->
      let str, un = f in
      (Base.Printf.sprintf "field %s" str, Tokens.typ_as_str (Tokens.UNION un))
  | PAREN _ -> ("PAREN", "")
  | BRACKET _ -> ("BRACKET", "")
  | BRACE _ -> ("BRACE", "")
  | I t ->
      let a, b = Tokens.token_as_str t in
      (Base.Printf.sprintf "I:", Base.Printf.sprintf "%s %s" a b)
  | NONE -> ("NONE", "")

type line =
  | FUNC of string * field list * alias
  | STRUCT of string * field list
  | ALIAS of alias
  | VALUE of keyword * string * alias * string

let is_alias tokens =
  let rec aux tokens =
    match tokens with
    | [] -> false
    | h :: _ when h = Tokens.PIPE -> true
    | _ :: t -> aux t
  in
  aux tokens

let is_type tokens = not (is_alias tokens)

let rec parenblock tkns =
  let rec aux (blocks : Tokens.tokens list) (out : blocks list) =
    match blocks with
    | [] when out != [] -> ([], PAREN out)
    | [] -> ([], NONE)
    | h :: t -> (
        match h with
        | Tokens.LEFTPAREN ->
            let rest, next = parenblock (h :: t) in
            aux rest (next :: out)
        | Tokens.RIGHTPAREN -> (t, PAREN out)
        | Tokens.LEFTBRACE ->
            let rest, next = braceblock (h :: t) in
            aux rest (next :: out)
        | Tokens.LEFTBRACKET ->
            let rest, next = bracketblock (h :: t) in
            aux rest (next :: out)
        | h -> aux t (I h :: out))
  in
  match tkns with
  | Tokens.LEFTPAREN :: t -> aux t []
  | _ :: _ -> (tkns, NONE)
  | [] -> ([], NONE)

and braceblock tkns =
  let rec aux (blocks : Tokens.tokens list) (out : blocks list) =
    match blocks with
    | [] when out != [] -> ([], BRACE out)
    | [] -> ([], NONE)
    | h :: t -> (
        match h with
        | Tokens.LEFTBRACE ->
            let rest, next = braceblock (h :: t) in
            aux rest (next :: out)
        | Tokens.RIGHTBRACE -> (t, BRACE out)
        | Tokens.LEFTPAREN ->
            let rest, next = parenblock (h :: t) in
            aux rest (next :: out)
        | Tokens.LEFTBRACKET ->
            let rest, next = bracketblock (h :: t) in
            aux rest (next :: out)
        | h -> aux t (I h :: out))
  in
  match tkns with
  | Tokens.LEFTBRACE :: t -> aux t []
  | _ :: _ -> (tkns, NONE)
  | [] -> ([], NONE)

and bracketblock tkns =
  let rec aux (blocks : Tokens.tokens list) (out : blocks list) =
    match blocks with
    | [] when out != [] -> ([], BRACKET out)
    | [] -> ([], NONE)
    | h :: t -> (
        match h with
        | Tokens.LEFTBRACKET ->
            let rest, next = bracketblock (h :: t) in
            aux rest (next :: out)
        | Tokens.RIGHTBRACKET -> (t, BRACKET out)
        | Tokens.LEFTPAREN ->
            let rest, next = parenblock (h :: t) in
            aux rest (next :: out)
        | Tokens.LEFTBRACE ->
            let rest, next = braceblock (h :: t) in
            aux rest (next :: out)
        | h -> aux t (I h :: out))
  in
  match tkns with
  | Tokens.LEFTBRACKET :: t -> aux t []
  | _ :: _ -> (tkns, NONE)
  | [] -> ([], NONE)

let is_block_start = function
  | t
    when t = Tokens.LEFTBRACKET || t = Tokens.LEFTBRACE || t = Tokens.LEFTPAREN
    ->
      true
  | _ -> false

let should_end_alias (t : Tokens.tokens) =
  Tokens.token_symbol t && t != Tokens.PIPE

let unionize tks =
  let rec aux (tokens : Tokens.tokens list) out =
    match tokens with
    | [] -> (tokens, out)
    | h :: t -> (
        match h with
        | tkn when should_end_alias tkn -> (h :: t, out)
        | IDENT a -> aux t (Tokens.typeify a :: out)
        | _ -> aux t out)
  in
  let toks, al = aux tks [] in
  (toks, al)

let should_end_fields (t : Tokens.tokens) =
  t = Tokens.RIGHTBRACKET || t = Tokens.RIGHTPAREN

let fields tks =
  let rec aux (tokens : Tokens.tokens list) (out : blocks option) (temp : field)
      =
    match tokens with
    | [] -> (tokens, out)
    | h :: t -> (
        match h with
        | tkn when should_end_fields tkn -> (h :: t, out)
        | IDENT a -> aux t out (a, [])
        | COLON ->
            let rest, l = unionize t in
            let a, _ = temp in
            aux rest (Some (FIELD (a, Base.List.rev l))) ("", [])
        | SEMICOLON -> (t, out)
        | _ -> aux t out ("", []))
  in
  let remaining, f = aux tks None ("", []) in
  (remaining, f)

let is_empty = function
  | [] -> true
  | _ -> false

let next_block t lst =
  match t with
  | k when Tokens.is_keyword k ->
      let a =
        match k with
        | Tokens.VAR -> VAR
        | Tokens.CONST -> CONST
        | Tokens.LET -> LET
        | Tokens.FUNCTION -> FUNCTION
        | Tokens.TYPE -> TYPE
        | a ->
            let arg, _ = Tokens.token_as_str a in
            raise (Invalid_argument arg)
      in
      (Base.List.tl lst, KEYWORD a)
  | Tokens.LEFTPAREN ->
      let rest, block = parenblock lst in
      (Some rest, block)
  | Tokens.LEFTBRACE ->
      let rest, block = braceblock lst in
      (Some rest, block)
  | Tokens.LEFTBRACKET ->
      let rest, block = bracketblock lst in
      (Some rest, block)
  | Tokens.IDENT a -> (
      match lst with
      | [] ->
          raise
            (Invalid_argument
               "Passed empty list and token to function next_block")
      | head :: _ when head = Tokens.COLON -> (
          let rest, block = fields lst in
          match block with
          | None -> (Some rest, NONE)
          | Some b -> (Some rest, b))
      | _ :: _ -> (Base.List.tl lst, IDENT a))
  | other -> (Base.List.tl lst, I other)

let get_blocks lst =
  let rec aux current out =
    match current with
    | [] -> Base.List.rev out
    | h :: _ -> (
        let rest, block = next_block h current in
        match rest with
        | None -> Base.List.rev (block :: out)
        | Some l -> aux l (block :: out))
  in
  aux lst []

let rec print = function
  | [] -> ()
  | h :: t ->
      let a, _ = Tokens.token_as_str h in
      print_endline a;
      print t

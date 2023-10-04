type typ =
  | NUMBER
  | STRING
  | ANY
  | BOOL
  | ARRAY
  | TUPLE
  | UNKNOWN
  | NULL
  | UNDEFINED
  | VOID
  | NEVER
  | USERDEFINED of string
  | UNION of typ list

type tokens =
  | VAR
  | LET
  | CONST
  | TYPE
  | CLASS
  | ENUM
  | FUNCTION
  | IDENT of string
  | TYP of typ
  | LEFTBRACKET
  | RIGHTBRACKET
  | LEFTBRACE
  | RIGHTBRACE
  | LEFTPAREN
  | RIGHTPAREN
  | PIPE
  | EQUAL
  | COLON
  | SEMICOLON
  | COMMA
  | ILLEGAL of string
  | VAL of tokens list

let is_keyword = function
  | VAR -> true
  | LET -> true
  | CONST -> true
  | TYPE -> true
  | CLASS -> true
  | ENUM -> true
  | FUNCTION -> true
  | _ -> false

let char_symbol = function
  | '{' -> true
  | '}' -> true
  | '[' -> true
  | ']' -> true
  | '(' -> true
  | ')' -> true
  | '|' -> true
  | '=' -> true
  | ':' -> true
  | ';' -> true
  | ',' -> true
  | _ -> false

let token_symbol = function
  | LEFTBRACKET -> true
  | RIGHTBRACKET -> true
  | LEFTBRACE -> true
  | RIGHTBRACE -> true
  | LEFTPAREN -> true
  | RIGHTPAREN -> true
  | PIPE -> true
  | EQUAL -> true
  | COLON -> true
  | SEMICOLON -> true
  | COMMA -> true
  | _ -> false

let is_number = function
  | '0' -> true
  | '1' -> true
  | '2' -> true
  | '3' -> true
  | '4' -> true
  | '5' -> true
  | '6' -> true
  | '7' -> true
  | '8' -> true
  | '9' -> true
  | _ -> false

let is_letter = function
  | 'a' -> true
  | 'b' -> true
  | 'c' -> true
  | 'd' -> true
  | 'e' -> true
  | 'f' -> true
  | 'g' -> true
  | 'h' -> true
  | 'i' -> true
  | 'j' -> true
  | 'k' -> true
  | 'l' -> true
  | 'm' -> true
  | 'n' -> true
  | 'o' -> true
  | 'p' -> true
  | 'q' -> true
  | 'r' -> true
  | 's' -> true
  | 't' -> true
  | 'u' -> true
  | 'v' -> true
  | 'w' -> true
  | 'x' -> true
  | 'y' -> true
  | 'z' -> true
  | 'A' -> true
  | 'B' -> true
  | 'C' -> true
  | 'D' -> true
  | 'E' -> true
  | 'F' -> true
  | 'G' -> true
  | 'H' -> true
  | 'I' -> true
  | 'J' -> true
  | 'K' -> true
  | 'L' -> true
  | 'M' -> true
  | 'N' -> true
  | 'O' -> true
  | 'P' -> true
  | 'Q' -> true
  | 'R' -> true
  | 'S' -> true
  | 'T' -> true
  | 'U' -> true
  | 'V' -> true
  | 'W' -> true
  | 'X' -> true
  | 'Y' -> true
  | 'Z' -> true
  | _ -> false

let is_valid_ident = function
  | '_' -> true
  | ch -> is_letter ch

type raw = STRING of string | CHAR of char

let tokenize = function
  | STRING s -> (
      match s with
      | "var" -> VAR
      | "let" -> LET
      | "const" -> CONST
      | "function" -> FUNCTION
      | "class" -> CLASS
      | "type" -> TYPE
      | "enum" -> ENUM
      | s -> IDENT (Base.String.rstrip s))
  | CHAR c -> (
      match c with
      | '{' -> LEFTBRACKET
      | '}' -> RIGHTBRACKET
      | '[' -> LEFTBRACE
      | ']' -> RIGHTBRACE
      | '(' -> LEFTPAREN
      | ')' -> RIGHTPAREN
      | '|' -> PIPE
      | '=' -> EQUAL
      | ':' -> COLON
      | ';' -> SEMICOLON
      | ',' -> COMMA
      | _ ->
          raise
            (Invalid_argument
               (Base.String.concat
                  (Base.Char.to_string c :: [ " as a symbol" ]))))

let is_ident = function
  | IDENT _ -> true
  | _ -> false

let typeify = function
  | "number" -> NUMBER
  | "string" -> STRING
  | "any" -> ANY
  | "bool" -> BOOL
  | "array" -> ARRAY
  | "tuple" -> TUPLE
  | "unknown" -> UNKNOWN
  | "null" -> NULL
  | "undefined" -> UNDEFINED
  | "void" -> VOID
  | "never" -> NEVER
  | s -> USERDEFINED s

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

let rec typ_as_str = function
  | NUMBER -> "number"
  | STRING -> "string"
  | ANY -> "any"
  | BOOL -> "bool"
  | ARRAY -> "array"
  | TUPLE -> "tuple"
  | UNKNOWN -> "unknown"
  | NULL -> "null"
  | UNDEFINED -> "undefined"
  | VOID -> "void"
  | NEVER -> "never"
  | UNION a ->
      let buf = Base.Buffer.create 64 in
      let b = map a typ_as_str in
      let _ = map b (Base.Buffer.add_string buf) in
      Base.Buffer.contents buf
  | USERDEFINED s -> s

let token_as_str = function
  | IDENT s -> ("IDENT", s)
  | ILLEGAL s -> ("ILLEGAL", s)
  | VAR -> ("VAR", "")
  | LET -> ("LET", "")
  | CONST -> ("CONST", "")
  | TYPE -> ("TYPE", "")
  | CLASS -> ("CLASS", "")
  | FUNCTION -> ("FUNCTION", "")
  | ENUM -> ("ENUM", "")
  | LEFTBRACKET -> ("LEFTBRACKET", "")
  | RIGHTBRACKET -> ("RIGHTBRACKET", "")
  | LEFTBRACE -> ("LEFTBRACE", "")
  | RIGHTBRACE -> ("RIGHTBRACE", "")
  | LEFTPAREN -> ("LEFTPAREN", "")
  | RIGHTPAREN -> ("RIGHTPAREN", "")
  | PIPE -> ("PIPE", "")
  | EQUAL -> ("EQUAL", "")
  | COLON -> ("COLON", "")
  | SEMICOLON -> ("SEMICOLON", "")
  | COMMA -> ("COMMA", "")
  | TYP _ -> ("Type", "")
  | VAL _ -> ("Value", "")

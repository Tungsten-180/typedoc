type ident = string
type alias = ident * Tokens.typ list
type keyword = string

let is_alias tokens =
  let rec aux tokens =
    match tokens with
    | [] -> false
    | h :: _ when h = Tokens.PIPE -> true
    | _ :: t -> aux t
  in
  aux tokens

let is_type tokens = not (is_alias tokens)

let parenblock t =
  let rec aux tokens count out =
    match tokens with
    | [] when count = 0 -> Some (Base.List.rev out)
    | _ :: _ when count = 0 -> Some (Base.List.rev out)
    | h :: t when h = '(' -> aux t (count + 1) (h :: out)
    | h :: t when h = ')' -> aux t (count - 1) (h :: out)
    | h :: t -> aux t count (h :: out)
    | [] -> None
  in
  aux t 1 []

let braceblock t =
  let rec aux tokens count out =
    match tokens with
    | [] when count = 0 -> Some (Base.List.rev out)
    | _ :: _ when count = 0 -> Some (Base.List.rev out)
    | h :: t when h = '[' -> aux t (count + 1) (h :: out)
    | h :: t when h = ']' -> aux t (count - 1) (h :: out)
    | h :: t -> aux t count (h :: out)
    | [] -> None
  in
  aux t 1 []

let bracketblock t =
  let rec aux tokens count out =
    match tokens with
    | [] when count = 0 -> Some (Base.List.rev out)
    | _ :: _ when count = 0 -> Some (Base.List.rev out)
    | h :: t when h = '{' -> aux t (count + 1) (h :: out)
    | h :: t when h = '}' -> aux t (count - 1) (h :: out)
    | h :: t -> aux t count (h :: out)
    | [] -> None
  in
  aux t 1 []

type field = ident * alias
type stct = ident * field list
type func = ident * field list * alias
type value = keyword * ident * alias * string

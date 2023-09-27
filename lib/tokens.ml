type typ =
  | STRING
  | NUMBER
  | BOOLEAN
  | USERDEFINED of string
  | ANY
  | UNION of typ list

type tokens =
  | VAR
  | LET
  | CONST
  | TYPE
  | CLASS
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
  | ILLEGAL of string
  | VAL of tokens list

let is_symbol = function
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
      | _ ->
          raise
            (Invalid_argument
               (Base.String.concat
                  (Base.Char.to_string c :: [ " as a symbol" ]))))

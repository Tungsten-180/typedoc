let lex str =
  let k = Typedoc.Lexer.create_lexer str in
  let l = Typedoc.Lexer.advance k in
  Typedoc.Lexer.get_tokens l

let test = function
  | Typedoc.Tokens.IDENT s ->
      print_endline "IDENT";
      print_endline s
  | Typedoc.Tokens.ILLEGAL s ->
      print_endline "ILLEGAL";
      print_endline s
  | Typedoc.Tokens.VAR -> print_endline "VAR"
  | Typedoc.Tokens.LET -> print_endline "LET"
  | Typedoc.Tokens.CONST -> print_endline "CONST"
  | Typedoc.Tokens.TYPE -> print_endline "TYPE"
  | Typedoc.Tokens.CLASS -> print_endline "CLASS"
  | Typedoc.Tokens.FUNCTION -> print_endline "FUNCTION"
  | Typedoc.Tokens.LEFTBRACKET -> print_endline "LEFTBRACKET"
  | Typedoc.Tokens.RIGHTBRACKET -> print_endline "RIGHTBRACKET"
  | Typedoc.Tokens.LEFTBRACE -> print_endline "LEFTBRACE"
  | Typedoc.Tokens.RIGHTBRACE -> print_endline "RIGHTBRACE"
  | Typedoc.Tokens.LEFTPAREN -> print_endline "LEFTPAREN"
  | Typedoc.Tokens.RIGHTPAREN -> print_endline "RIGHTPAREN"
  | Typedoc.Tokens.PIPE -> print_endline "PIPE"
  | Typedoc.Tokens.EQUAL -> print_endline "EQUAL"
  | Typedoc.Tokens.COLON -> print_endline "COLON"
  | Typedoc.Tokens.SEMICOLON -> print_endline "SEMICOLON"
  | Typedoc.Tokens.TYP _ -> print_endline "Type"
  | Typedoc.Tokens.VAL _ -> print_endline "Value"

let rec print = function
  | [] -> ()
  | h :: t ->
      let _ = test h in
      print t

let () = print (lex "type cool = number | string;")
let () = print_newline ()
let () = print_endline "----------------"
let () = print_newline ()
let () = print (lex "function cool(question:number):number{\n}")
(*
let () =
  print
    (match w with
    | _, None -> []
    | _, Some t -> t :: [])
*)

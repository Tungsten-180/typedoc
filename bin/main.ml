let lex str =
  let k = Typedoc.Lexer.create_lexer str in
  let l = Typedoc.Lexer.advance k in
  Typedoc.Lexer.get_tokens l

let rec print = function
  | [] -> ()
  | h :: t ->
      let a, _ = Typedoc.Tokens.token_as_str h in
      print_endline a;
      print t

let rec go lst =
  match lst with
  | [] -> ()
  | h :: t -> (
      match h with
      | Typedoc.Analyzer.PAREN l
      | Typedoc.Analyzer.BRACE l
      | Typedoc.Analyzer.BRACKET l ->
          let () = go l in
          go t
      | _ ->
          let a, b = Typedoc.Analyzer.print_block h in
          print_endline (Base.Printf.sprintf "%s %s" a b);
          go t)

let cool = lex "type cool = number | string;"
let () = print cool
let () = print_newline ()
let () = print_endline "----------------"
let () = print_newline ()
let f = lex "function cool(question:number):number{\n}"
let () = print f
let () = print_newline ()
let () = print_endline "----------------"
let () = print_newline ()
let blcks = Typedoc.Analyzer.get_blocks cool
let () = go blcks
(*
let () = print_newline ()
let () = print_endline "----------------"
let () = print_newline ()
let blks = Typedoc.Analyzer.get_blocks f
let () = go blks
*)

(*
let () =
  print
    (match w with
    | _, None -> []
    | _, Some t -> t :: [])
*)

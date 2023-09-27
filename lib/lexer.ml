open Base

type t = { input : string; position : int; ch : char option }

let create_lexer str = { input = str; position = 0; ch = None }

let advance lexer =
  match lexer.ch with
  | None when lexer.position = 0 ->
      {
        input = lexer.input;
        position = 0;
        ch =
          (try Some (Base.String.get lexer.input 0)
           with Invalid_argument _ -> None);
      }
  | None -> lexer
  | Some _ ->
      {
        input = lexer.input;
        position = lexer.position + 1;
        ch =
          (try Some (Base.String.get lexer.input (lexer.position + 1))
           with Invalid_argument _ -> None);
      }

let is_whitespace = function
  | ' ' | '\n' -> true
  | _ -> false

let rec skip_whitespace lexer =
  match lexer.ch with
  | Some a when is_whitespace a -> skip_whitespace (advance lexer)
  | _ -> lexer

type b = BUILDING | SINGLE

(*
            ( advance lex,
              Some
                (Tokens.tokenize
                   (Tokens.STRING
                      (Base.String.of_list (Base.List.rev temporary)))) )
          *)
let rec next_token lexer =
  let rec aux lex temporary b =
    match b with
    | SINGLE -> (
        match lex.ch with
        | None -> (lex, None)
        | Some ch when is_whitespace ch -> next_token (skip_whitespace lex)
        | Some ch when Tokens.is_symbol ch ->
            (advance lex, Some (Tokens.tokenize (Tokens.CHAR ch)))
        | Some ch -> aux (advance lex) (ch :: temporary) BUILDING)
    | BUILDING -> (
        match lex.ch with
        | None -> (lex, None)
        | Some ch when is_whitespace ch || Tokens.is_symbol ch ->
            ( skip_whitespace lex,
              Some
                (Tokens.tokenize
                   (Tokens.STRING
                      (Base.String.of_list (Base.List.rev temporary)))) )
        | Some ch -> aux (advance lex) (ch :: temporary) BUILDING)
  in
  aux lexer [] SINGLE

let get_tokens l =
  let rec aux lexer tokens =
    let lex, token = next_token lexer in
    match token with
    | None -> Base.List.rev tokens
    | Some t -> aux lex (t :: tokens)
  in
  aux l []

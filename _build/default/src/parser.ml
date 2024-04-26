open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with
  | Some(Tok_Let) -> parse_let_expr toks
  | Some(Tok_If) -> parse_if_expr toks
  | Some(Tok_Fun) -> parse_fun_expr toks
  | _ -> parse_or_expr toks

and parse_let_expr toks = 
  (match lookahead toks with
  | Some(Tok_Let) -> 
    (match lookahead_many toks 1 with
    | Some(Tok_Rec) ->
      (match lookahead_many toks 2 with
      | Some(Tok_ID id) -> 
        (match lookahead_many toks 3 with
        | Some(Tok_Equal) ->
          let t = match_many toks [Tok_Let; Tok_Rec; Tok_ID id; Tok_Equal] in
          let (t', ast) = parse_expr t in
          (match lookahead t' with
          | Some(Tok_In) ->
            let t'' = match_token t' Tok_In in
            let (t''', ast') = parse_expr t'' in
            (t''', Let(id, true, ast, ast'))
          | _ -> raise (InvalidInputException "no in")
          )
        | _ -> raise (InvalidInputException "no equal")
        )
      | _ -> raise (InvalidInputException "no variable")
      )
    | Some(Tok_ID id) -> 
      (match lookahead_many toks 2 with
      | Some(Tok_Equal) ->
        let t = match_many toks [Tok_Let; Tok_ID id; Tok_Equal] in
        let (t', ast) = parse_expr t in
        (match lookahead t' with
          | Some(Tok_In) ->
          let t'' = match_token t' Tok_In in
          let (t''', ast') = parse_expr t'' in
          (t''', Let(id, false, ast, ast'))
          | _ -> raise (InvalidInputException "no in")
        )
      | _ -> raise (InvalidInputException "no equal")
      )
    | _ -> raise (InvalidInputException "invalid let")
    )
  | _ -> raise (InvalidInputException "no let")
  )

and parse_if_expr toks = 
    let t = match_token toks Tok_If in
    let (t', ast) = parse_expr t in
    let t'' = match_token t' Tok_Then in
    let (t''', ast') = parse_expr t'' in
    let t'''' = match_token t''' Tok_Else in
    let (t''''', ast'') = parse_expr t'''' in
    (t''''', If(ast,ast',ast''))

and parse_fun_expr toks = 
  (match lookahead toks with 
  | Some(Tok_Fun) ->
    (match lookahead_many toks 1 with
    | Some(Tok_ID id) ->
      (match lookahead_many toks 2 with
      | Some(Tok_Arrow) ->
        let t = match_many toks [Tok_Fun; Tok_ID id; Tok_Arrow] in 
        let (t', ast) = parse_expr t in
        (t', Fun(id, ast))
      | _ -> raise (InvalidInputException "no arrow")
      )
    | _ -> raise (InvalidInputException "invalid variable")
    )
  | _ -> raise (InvalidInputException "invalid fun")
  )

and parse_or_expr toks = 
  let (t, ast) = parse_and_expr toks in
  match lookahead t with
  | Some(Tok_Or) ->
    let t' = match_token t Tok_Or in
    let (t'', ast') = parse_or_expr t' in
    (t'', Binop(Or,ast,ast'))
  | _ -> (t, ast)

and parse_and_expr toks = 
  let (t, ast) = parse_eq_expr toks in
  match lookahead t with
  | Some(Tok_And) ->
    let t' = match_token t Tok_And in
    let (t'', ast') = parse_and_expr t' in
    (t'', Binop(And,ast,ast'))
  | _ -> (t, ast)

and parse_eq_expr toks =
  let (t, ast) = parse_rel_expr toks in
  match lookahead t with
  | Some(Tok_Equal) ->
    let t' = match_token t Tok_Equal in
    let (t'', ast') = parse_eq_expr t' in
    (t'', Binop(Equal,ast,ast'))
  | Some(Tok_NotEqual) ->
    let t' = match_token t Tok_NotEqual in
    let (t'', ast') = parse_eq_expr t' in
    (t'', Binop(NotEqual,ast,ast'))
  | _ -> (t, ast)

and parse_rel_expr toks = 
  let (t, ast) = parse_add_expr toks in
  match lookahead t with
  | Some(Tok_Less) ->
    let t' = match_token t Tok_Less in
    let (t'', ast') = parse_rel_expr t' in
    (t'', Binop(Less,ast,ast'))
  | Some(Tok_Greater) ->
    let t' = match_token t Tok_Greater in
    let (t'', ast') = parse_rel_expr t' in
    (t'', Binop(Greater,ast,ast'))
  | Some(Tok_LessEqual) ->
    let t' = match_token t Tok_LessEqual in
    let (t'', ast') = parse_rel_expr t' in
    (t'', Binop(LessEqual,ast,ast'))
  | Some(Tok_GreaterEqual) ->
    let t' = match_token t Tok_GreaterEqual in
    let (t'', ast') = parse_rel_expr t' in
    (t'', Binop(GreaterEqual,ast,ast'))
  | _ -> (t, ast)

and parse_add_expr toks =
  let (t, ast) = parse_mult_expr toks in
  match lookahead t with
  | Some(Tok_Add) -> 
    let t' = match_token t Tok_Add in
    let (t'', ast') = parse_add_expr t' in
    (t'', Binop(Add,ast,ast'))
  | Some(Tok_Sub) -> 
    let t' = match_token t Tok_Sub in
    let (t'', ast') = parse_add_expr t' in
    (t'', Binop(Sub,ast,ast'))
  | _ -> (t, ast)

and parse_mult_expr toks =
  let (t, ast) = parse_concat_expr toks in
  match lookahead t with
  | Some(Tok_Mult) -> 
    let t' = match_token t Tok_Mult in
    let (t'', ast') = parse_mult_expr t' in
    (t'', Binop(Mult,ast,ast'))
  | Some(Tok_Div) -> 
    let t' = match_token t Tok_Div in
    let (t'', ast') = parse_mult_expr t' in
    (t'', Binop(Div,ast,ast'))
  | _ -> (t, ast)

and parse_concat_expr toks =
  let (t, ast) = parse_unary_expr toks in
  match lookahead t with
  | Some(Tok_Concat) -> 
    let t' = match_token t Tok_Concat in 
    let (t'', ast') = parse_concat_expr t' in 
    (t'', Binop(Concat,ast,ast'))
  | _ -> (t, ast)

and parse_unary_expr toks =
  match lookahead toks with
  | Some(Tok_Not) -> 
    let t = match_token toks Tok_Not in 
    let (t', ast) = parse_unary_expr t in 
    (t', Not(ast))
  | _ -> parse_function_call toks

and parse_function_call toks = (* return FunctionCall(PrimaryExpr, PrimaryExpr) or return PrimaryExpr *)
  let (t, ast) = parse_primary_expr toks in
  match lookahead t with
  (* if (t', ast') = parse_primary_expr does not error then return FuncCall(ast, ast') *)
  | Some(Tok_Int i) -> let (t', ast') = parse_primary_expr t in (t', FunctionCall(ast, ast'))
  | Some(Tok_Bool b) -> let (t', ast') = parse_primary_expr t in (t', FunctionCall(ast, ast'))
  | Some(Tok_String s) -> let (t', ast') = parse_primary_expr t in (t', FunctionCall(ast, ast'))
  | Some(Tok_ID id) -> let (t', ast') = parse_primary_expr t in (t', FunctionCall(ast, ast'))
  | Some(Tok_LParen) -> let (t', ast') = parse_primary_expr t in (t', FunctionCall(ast, ast'))
  (* else just return (t, ast) *)
  | _ -> (t, ast)
  

and parse_primary_expr toks = 
  match lookahead toks with
  | Some(Tok_Int i) -> let t = match_token toks (Tok_Int i) in (t, Value(Int(i)))
  | Some(Tok_Bool b) -> let t = match_token toks (Tok_Bool b) in (t, Value(Bool(b)))
  | Some(Tok_String s) -> let t = match_token toks (Tok_String s) in (t, Value(String(s)))
  | Some(Tok_ID id) -> let t = match_token toks (Tok_ID id) in (t, ID(id))
  | Some(Tok_LParen) -> 
    let t = match_token toks Tok_LParen in 
    let (t', ast) = parse_expr t in 
    let t'' = match_token t' Tok_RParen in 
    (t'', ast)
  | _ -> raise (InvalidInputException "invalid line")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some(Tok_DoubleSemi) -> if List.length toks = 1 then ([], NoOp) else raise (InvalidInputException "invalid ending")
  | Some(Tok_Def) -> parse_def_mutop toks
  | _ -> parse_expr_mutop toks

and parse_def_mutop toks =
  (match lookahead toks with
  | Some(Tok_Def) -> 
    (match lookahead_many toks 1 with
    | Some(Tok_ID id) -> 
      (match lookahead_many toks 2 with
      | Some(Tok_Equal) ->
        let t = match_many toks [Tok_Def; Tok_ID id; Tok_Equal] in
        let (t', ast) = parse_expr t in
        (match lookahead t' with
        | Some(Tok_DoubleSemi) -> if List.length t' = 1 then ([], Def(id,ast)) else raise (InvalidInputException "invalid ending")
        | _ -> raise (InvalidInputException "invalid ending")
        )
      | _ -> raise (InvalidInputException "no equal")
      )
    | _ -> raise (InvalidInputException "no var")
    )
  | _ -> raise (InvalidInputException "no def")
  )

and parse_expr_mutop toks = 
  let (t, ast) = parse_expr toks in
  if List.length t = 1 then ([], Expr(ast)) else raise (InvalidInputException "invalid expr")
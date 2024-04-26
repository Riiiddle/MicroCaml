(* #load "str.cma";; *)

open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize (input: string) : token list = 
  let length = String.length input in

  let rec tok pos =
    if pos >= length then []

    else if Str.string_match (Str.regexp " ") input pos then
      tok (pos + 1)

      (* Tok_Int(i)  *) 
      (* this is put up here for short circuiting, 
         to not get confused with paranthesis and subtraction*)
    else if Str.string_match (Str.regexp "\\((-[0-9]+)\\)") input pos then
      let value = Str.matched_string input in
      let integer = String.sub value 1 (String.length value - 2) in
      Tok_Int(int_of_string integer)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "->") input pos then
      Tok_Arrow::(tok (pos + 2))

    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen::(tok (pos + 1))

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(tok (pos + 1))

    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Equal::(tok (pos + 1))

    else if Str.string_match (Str.regexp "<>") input pos then
      Tok_NotEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater::(tok (pos + 1))

    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less::(tok (pos + 1))

    else if Str.string_match (Str.regexp "[|][|]") input pos then
      Tok_Or::(tok (pos + 2))

    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And::(tok (pos + 2))

    else if Str.string_match (Str.regexp "\\bnot\\b") input pos then
      Tok_Not::(tok (pos + 3))

    else if Str.string_match (Str.regexp "\\bif\\b") input pos then
      Tok_If::(tok (pos + 2))

    else if Str.string_match (Str.regexp "\\bthen\\b") input pos then
      Tok_Then::(tok (pos + 4))

    else if Str.string_match (Str.regexp "\\belse\\b") input pos then
      Tok_Else::(tok (pos + 4))

    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add::(tok (pos + 1))

    else if Str.string_match (Str.regexp "-") input pos then
      Tok_Sub::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult::(tok (pos + 1))

    else if Str.string_match (Str.regexp "/") input pos then
      Tok_Div::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Concat::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\blet\\b") input pos then
      Tok_Let::(tok (pos + 3))

    else if Str.string_match (Str.regexp "\\brec\\b") input pos then
      Tok_Rec::(tok (pos + 3))

    else if Str.string_match (Str.regexp "\\bin\\b") input pos then
      Tok_In::(tok (pos + 2))

    else if Str.string_match (Str.regexp "\\bdef\\b") input pos then
      Tok_Def::(tok (pos + 3))

    else if Str.string_match (Str.regexp "\\bfun\\b") input pos then
      Tok_Fun::(tok (pos + 3))

    (* Tok_Int(i) positive *)
    else if Str.string_match (Str.regexp "[0-9]+") input pos then
      let value = Str.matched_string input in
      Tok_Int(int_of_string value)::(tok (pos + String.length value))

    (* Tok_Bool(b) false*) 
    else if Str.string_match (Str.regexp "\\bfalse\\b") input pos then
      Tok_Bool(false)::(tok (pos + 5))

    (* Tok_Bool(b) true *)
    else if Str.string_match (Str.regexp "\\btrue\\b") input pos then
      Tok_Bool(true)::(tok (pos + 4))

    (* Tok_String(s) *)
    else if Str.string_match (Str.regexp "\"\\([^\"]*\\)\"") input pos then
      let value = Str.matched_group 1 input in
      Tok_String(value)::(tok (pos + 2 + String.length value))

    (* Tok_ID(s) *)
    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let value = Str.matched_string input in
      Tok_ID(value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp ";;") input pos then
      Tok_DoubleSemi::(tok (pos + 2))

    else raise (InvalidInputException "Invalid line")

  in tok 0;;
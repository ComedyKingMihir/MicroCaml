open Types
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lcurly = Str.regexp "{"
let re_rcurly = Str.regexp "}"
let re_dot = Str.regexp "\\."
let re_equal = Str.regexp "="
let re_not_equal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greater_equal = Str.regexp ">="
let re_less_equal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_def = Str.regexp "def"
let re_in = Str.regexp "in"
let re_rec = Str.regexp "rec"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_double_semi = Str.regexp ";;"
let re_semi = Str.regexp ";"
let re_num = Str.regexp "[0-9]+"
let re_neg_num = Str.regexp "(-[0-9]+)"
let re_true = Str.regexp "true"
let re_false = Str.regexp "false"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_whitespace = Str.regexp "[ \t\n]"

let tokenize input = 
  let rec tokenize_helper pos str =
    if pos >= String.length str then
      []
    else if Str.string_match re_arrow str pos then
      Tok_Arrow :: tokenize_helper (pos + 2) str
    else if Str.string_match re_rparen str pos then
      Tok_RParen :: tokenize_helper (pos + 1) str
    else if Str.string_match re_lcurly str pos then
      Tok_LCurly :: tokenize_helper (pos + 1) str
    else if Str.string_match re_rcurly str pos then
      Tok_RCurly :: tokenize_helper (pos + 1) str
    else if Str.string_match re_dot str pos then
      Tok_Dot :: tokenize_helper (pos + 1) str
    else if Str.string_match re_equal str pos then
      Tok_Equal :: tokenize_helper (pos + 1) str
    else if Str.string_match re_not_equal str pos then
      Tok_NotEqual :: tokenize_helper (pos + 2) str
    else if Str.string_match re_greater str pos then
      Tok_Greater :: tokenize_helper (pos + 1) str
    else if Str.string_match re_greater_equal str pos then
      Tok_GreaterEqual :: tokenize_helper (pos + 2) str
    else if Str.string_match re_less_equal str pos then
      Tok_LessEqual :: tokenize_helper (pos + 2) str
    else if Str.string_match re_or str pos then
      Tok_Or :: tokenize_helper (pos + 2) str
    else if Str.string_match re_and str pos then
      Tok_And :: tokenize_helper (pos + 2) str
    else if Str.string_match re_not str pos then
      Tok_Not :: tokenize_helper (pos + 3) str
    else if Str.string_match re_if str pos then
      Tok_If :: tokenize_helper (pos + 2) str
    else if Str.string_match re_then str pos then
      Tok_Then :: tokenize_helper (pos + 4) str
    else if Str.string_match re_else str pos then
      Tok_Else :: tokenize_helper (pos + 4) str
    else if Str.string_match re_neg_num str pos then
      let tok = Str.matched_string str in
      let len = String.length tok in
      let sanitized = String.sub tok 1 (len - 2) in
      Tok_Int (int_of_string sanitized) :: tokenize_helper (pos + len) str
    else if Str.string_match re_lparen str pos then
      Tok_LParen :: tokenize_helper (pos + 1) str
    else if Str.string_match re_add str pos then
      Tok_Add :: tokenize_helper (pos + 1) str
    else if Str.string_match re_sub str pos then
      Tok_Sub :: tokenize_helper (pos + 1) str
    else if Str.string_match re_div str pos then
      Tok_Div :: tokenize_helper (pos + 1) str
    else if Str.string_match re_mult str pos then
      Tok_Mult :: tokenize_helper (pos + 1) str
    else if Str.string_match re_concat str pos then
      Tok_Concat :: tokenize_helper (pos + 1) str
    else if Str.string_match re_let str pos then
      Tok_Let :: tokenize_helper (pos + 3) str
    else if Str.string_match re_def str pos then
      Tok_Def :: tokenize_helper (pos + 3) str
    else if Str.string_match re_in str pos then
      Tok_In :: tokenize_helper (pos + 2) str
    else if Str.string_match re_rec str pos then
      Tok_Rec :: tokenize_helper (pos + 3) str
    else if Str.string_match re_fun str pos then
      Tok_Fun :: tokenize_helper (pos + 3) str
    else if Str.string_match re_double_semi str pos then
      Tok_DoubleSemi :: tokenize_helper (pos + 2) str
    else if Str.string_match re_semi str pos then
      Tok_Semi :: tokenize_helper (pos + 1) str
    else if Str.string_match re_num str pos then
      let tok = Str.matched_string str in
      let len = String.length tok in
      Tok_Int (int_of_string tok) :: tokenize_helper (pos + len) str
    else if Str.string_match re_true str pos then
      Tok_Bool true :: tokenize_helper (pos + 4) str
    else if Str.string_match re_false str pos then
      Tok_Bool false :: tokenize_helper (pos + 5) str
    else if Str.string_match re_string str pos then
      let tok = Str.matched_string str in
      let len = String.length tok in
      let sanitized = String.sub tok 1 (len - 2) in
      Tok_String sanitized :: tokenize_helper (pos + len) str
    else if Str.string_match re_id str pos then
      let tok = Str.matched_string str in
      let len = String.length tok in
      Tok_ID tok :: tokenize_helper (pos + len) str
    else if Str.string_match re_whitespace str pos then
      let tok = Str.matched_string str in
      let len = String.length tok in
      tokenize_helper (pos + len) str
    else
      raise (InvalidInputException "Trolled")
  in
  tokenize_helper 0 input

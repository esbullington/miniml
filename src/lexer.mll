{
  open Syntax
  open Parser
}

rule token = parse
    eof         { EOF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "<" { LT }
  | "<=" { LE }
  | "!=" { NE }
  | "::" { CONS }
  | "[" { LBRACKET }
  | "," { COMMA }
  | "]" { RBRACKET }
  | "and" { AND }
  | "or" { OR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "fun" { FUN }
  | "->" { ARROW } 
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE } 
  | "let" { LET }
  | "=" { EQ }
  | "in" { IN }
  | "rec" { REC }
  | ['0'-'9']* as num { INT((int_of_string num)) }
  | ['A'-'Z' 'a'-'z' '0'-'9' '_']* as str { IDENT(str) }
  | [' ' '\n' '\r' '\t'] {token lexbuf} 

  | _           { raise (LexerError ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }


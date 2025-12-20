%{
open Ast
%}

%token EOF
%token LET LETSTACK IN
%token IF THEN ELSE
%token FUN EXPORT
%token TRUE FALSE
%token LPAREN RPAREN
%token COLON
%token ARROW
%token EQ
%token <int> INT
%token <string> IDENT

%start <Ast.expr option> prog

%%

prog:
  | EOF { None }

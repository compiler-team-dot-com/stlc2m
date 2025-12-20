%token EOF

%start <unit option> prog

%%

prog:
  | EOF { None }

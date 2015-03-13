%{
    open Auxparser
    open Printf
%}

%token <int> L_INT
%token <float> L_REAL
%token INT REAL
%token <string> IDENTIFIER
%token ASS DIV MENO PIU PER 
%token SPAREN DPAREN
%token VIRG DUEP PUN PVIRG
%token BEGIN END VAR PROGRAM PROCEDURE
%token WRITE WRITELN
%token NUWLINU EOF
%token IF THEN ELSE WHILE DO
%token AND MAU MA MIU MI UG NU OR
%token DUMPENV

%start program          /* punto di ingresso */
%type <unit> program

%%

program :
    | EOF { printf "File Vuoto!\n" ; raise End_of_file }
    | program_intestazione PVIRG blocco PUN  { 
        global_enviroment.enviroments <- [$3] ;
        execute_global global_enviroment.enviroments ;
        raise End_of_file }
    ;

program_intestazione :
    PROGRAM identifier { $2 }
    ;

blocco:
    sezione_variabili
    sezione_procedure
    sezione_statement { {idlist = $1 ; proclist = $2 ; stmts = $3} }
    ;

    /* ############## Dichiarazioni Variabili ############## */

sezione_variabili :
    VAR lista_dichiarazione_var PVIRG { $2 }
    | { [] }
    ;

lista_dichiarazione_var : 
      lista_dichiarazione_var PVIRG dichiarazione_var { $1 @ $3 }
    | dichiarazione_var { $1 }
    ;

dichiarazione_var : 
    identifier_list DUEP id_tipo { init_vars $3 $1 }
    ;

identifier_list :
    identifier_list VIRG identifier { [$3] @ $1 }
    | identifier { [$1] }
    ;

identifier : IDENTIFIER { $1 } ;

id_tipo :
    INT { "int" }
    | REAL { "real" }
    ;

    /* ########## Procedure ########## */

sezione_procedure :
     lista_dichiarazione_proc PVIRG { $1 }
    | { [] }
    ;

lista_dichiarazione_proc :
     lista_dichiarazione_proc PVIRG dichiarazione_proc  { $1 @ [$3] }
    | dichiarazione_proc { [$1] }
    ;

dichiarazione_proc : 
    procedure { $1 }
    ;

procedure : 
    procedure_intestazione PVIRG blocco 
    { (fst $1, snd $1, { idlist = ($3).idlist ; proclist = ($3).proclist ; stmts = ($3).stmts } ) }
     | procedure_intestazione PVIRG { (fst $1, snd $1, {idlist = [] ; proclist = [] ; stmts = (Void ())} ) }
    ;

procedure_intestazione : 
    PROCEDURE identifier { ($2, []) }
    | PROCEDURE identifier def_param { ($2, $3) }
    ;

def_param : 
    SPAREN dichiarazione_var DPAREN { $2 }
    ;

    /* ################ Espressioni ################ */

expression:
     expr2 { $1 } 
   | expr2 UG expr2 { Ug ($1,$3) }
   | expr2 NU expr2 { Nu ($1,$3) }
   | expr2 MAU expr2 { Mau ($1,$3) }
   | expr2 MA expr2 { Ma ($1,$3) }
   | expr2 MIU expr2 { Miu ($1,$3) }
   | expr2 MI expr2 { Mi ($1,$3) }
   ;

expr2:
    expr3 { $1 }
    | expr2 PIU expr3 { Piu ($1,$3) }  
    | expr2 MENO expr3 { Meno ($1,$3) }  
    | expr2 OR expr3 { Or ($1,$3) }  
    ;

expr3:
    expr4 { $1 }
    | expr3 PER expr4 { Per ($1,$3) }   
    | expr3 DIV expr4 { Div ($1,$3) }   
    | expr3 AND expr4 { And ($1,$3) }   
    ;

expr4:
    PIU expr4 { $2 }
    | MENO expr4 { Neg $2 }
    | SPAREN expression DPAREN { $2 }
    | number { $1 }  
    ;

number:
    L_INT { Num (Int $1)  }
    | L_REAL { Num (Real $1) }
    | identifier { Ident $1 } 
    ; 

    /* ############## Istruzioni (Statements) ############### */

sezione_statement :
    | statement_composto { $1 } 
    ;

statement_composto :
    BEGIN statement_sequenza END { $2 } 
    | statement { $1 }
    ;

statement_sequenza : 
    statement_sequenza PVIRG statement { Seq ($1,$3) }
    | statement { $1 }
    ;

statement : 
    { Void () }
    | assegnamento_statement { $1 }
    | if_statement { $1 }
    | while_statement { $1 }
    | write_statement { $1 }
    | procedure_statement { $1 }
    | dumpenv_statement { $1 }
    ;

assegnamento_statement : 
    identifier ASS expression { Assign ($1,$3) }
    ;
    
if_statement :
    IF expression THEN statement_composto PVIRG { If ($2, $4, (Void ())) }
    | IF expression THEN statement_composto ELSE statement_composto { If ($2, $4, $6) }
    ;

procedure_statement : 
    identifier SPAREN param_list DPAREN { Procedure ($1, $3) }
    ;

param_list : 
    param_list VIRG param { $1 @ [$3] }
    | param { [$1] }
    | { [] }
    ;

param :
     expression { $1 }
     ;

while_statement :
    WHILE expression DO statement_composto { While ($2, $4) }   
    ;

write_statement:
    write { $1 }
    | writeln { $1 }
    ;

write :
    WRITE expression { Write $2 }
    ;

writeln:
    WRITELN expression { Writeln $2 }
    ;

dumpenv_statement :
    DUMPENV { Dumpenv () }
    ;


%{
#include <stdio.h>

void yyerror(const char * msg){
    return;
}
%}

%token tMAIL tENDMAIL tSCHEDULE tENDSCH tSEND tSET tTO tFROM tAT tCOMMA tCOLON tLPR tRPR tLBR tRBR tIDENT tSTRING tDATE tTIME tADDRESS
%start program

%%
program: componenets
;

componenets: 
           | mailblock componenets
           | set_statement componenets
           ;

mailblock: tMAIL tFROM tADDRESS tCOLON statements tENDMAIL
;

statements: 
          | set_statement statements
          | send_statement statements
          | schedule_statement statements
          ;

set_statement: tSET tIDENT tLPR tSTRING tRPR 
             ;

send_statement: tSEND tLBR tIDENT tRBR tTO recipient_list 
              | tSEND tLBR tSTRING tRBR tTO recipient_list 
              ;

list_send_statements: send_statement 
                    | list_send_statements send_statement
                    ;

schedule_statement: tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON list_send_statements tENDSCH
                  ;
      
recipient: tLPR tADDRESS tRPR
         | tLPR tIDENT tCOMMA tADDRESS tRPR
         | tLPR tSTRING tCOMMA tADDRESS tRPR
         ;

recipients: recipient
          | recipients tCOMMA recipient
          ;

recipient_list: tLBR recipients tRBR 
              ;

%%

int main () 
{
  if (yyparse()){       // parse error
    printf("ERROR\n");
    return 1;
  } 
  else {                // successful parsing
    printf("OK\n");
    return 0;
  } 
}

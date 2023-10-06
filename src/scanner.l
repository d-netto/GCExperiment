%{
#include "../include/tokens.h"
%}

%%
<<EOF>>                 return END_OF_FILE;
"class"                 return CLASS_KW;
"public"                return PUBLIC_KW;
"static"                return STATIC_KW;
"void"                  return VOID_KW;
"main"                  return MAIN_KW;
"("                     return LPAREN;
"String"                return STRING_KW;
"["                     return LBRACKET;
"]"                     return RBRACKET;
")"                     return RPAREN;
"{"                     return LBRACE;
"}"                     return RBRACE;
"extends"               return EXTENDS_KW;
";"                     return SEMICOLON;
"int"                   return INT_KW;
"boolean"               return BOOLEAN_KW;
"if"                    return IF_KW;
"else"                  return ELSE_KW;
"while"                 return WHILE_KW;
"System.out.println"    return PRINTLN_KW;
"&&"                    return AND;
"<"                     return LT;
"+"                     return PLUS;
"-"                     return MINUS;
"*"                     return TIMES;
"."                     return DOT;
"length"                return LENGTH_KW;
","                     return COMMA;
"true"                  return TRUE_KW;
"false"                 return FALSE_KW;
"this"                  return THIS_KW;
"new"                   return NEW_KW;
"!"                     return NOT;
"return"                return RETURN_KW;
[_a-zA-Z][_a-zA-Z0-9]*  return IDENTIFIER;
[0-9]+                  return INTEGER;
"="                     return EQUALS;                  
"//".*\n                ;
[\r\t \n]+              ;
.                       return UNKNOWN;

%%
int yywrap(void)
{
    return 1;
}

{
module Parser where

import Grammar
import Lexer
}

%name parseExpr
%tokentype { Token      }
%error     { parseError }
%monad     { Either String }{ >>= }{ return }

%token
    VARIABLE     { Variable $$  }
    LAMBDA       { Lambda        }
    DOT          { Dot          }
    LEFTBRACKET  { LeftBracket  }
    RIGHTBRACKET { RightBracket }

%%

Expr
  : Application LAMBDA VARIABLE DOT Expr          { Application $1 (Abstraction $3 $5) }
  | LAMBDA VARIABLE DOT Expr                      { Abstraction $2 $4                  }
  | Application                                   { $1                                 }

Application
  : Term                            { $1                }
  | Application Term                { Application $1 $2 }

Term
  : LEFTBRACKET Expr RIGHTBRACKET   { $2                }
  | VARIABLE                        { Var $1            }

{
parseError = fail "Parse error"
}






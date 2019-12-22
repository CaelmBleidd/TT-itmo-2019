{
module Lexer where
}

%wrapper "basic"

$digit     = 0-9
$alpha     = [a-z]
$apostrophe = '

tokens :-

    $white+                                 ;
    "#".*                                   ;
    \(                                      { \_ -> LeftBracket   }
    \)                                      { \_ -> RightBracket  }
    \.                                      { \_ -> Dot           }
    \\                                      { \_ -> Lambda        }
    $alpha [$alpha $digit $apostrophe]*     { \s -> Variable s    }

{
data Token = Lambda
           | Dot
           | LeftBracket
           | RightBracket
           | Variable String
           deriving (Show, Eq)
}

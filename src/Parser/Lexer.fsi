module Lexer

open System
open FSharp.Text.Lexing
open Parser  // Reference to parser's token types/// Rule tokenize
val tokenize: lexbuf: LexBuffer<char> -> token

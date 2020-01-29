{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ScopedTypeVariables, KindSignatures #-}
module TxtAd.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos      (newPos)

import TxtAd.Syntax

type Parser = PS.Parser

txtadParse :: SourceName -> Line -> Column -> String -> Either ParseError [TxtAd]
txtadParse fileName line column input
  = PP.parse (do { setPosition (newPos fileName line column)
                 ; whiteSpace
                 ; x <- txtadDecls
                 ; whiteSpace
                 ; eof <|> errorParse
                 ; return x
                 }) fileName input

errorParse = do
  { rest <- manyTill anyToken eof
  ; unexpected rest }

------------------------------------------------------------------------------
{- Grammar:

<txtadDecls> ::= <txtadDecl>*
<txtadDecl>  ::= <startDecl> | <errorDecl> | <sceneDecl>
<startDecl>  ::= 'start' ':' <identifier>
<errorDecl>  ::= 'error' ':' <stringLit>
<sceneDecl>  ::= '{' <sectDecls> '}' end'?
<sectDecls>  ::= <sectDecl>*
<sectDecl>   ::= <stringLit> | <stringLit> '->' <identifier>
<identifier> ::= as defined in Text.Parsec.Token
<stringLit>  ::= as defined in Text.Parsec.Token

 -}
------------------------------------------------------------------------------
-- Top-level parser:

-- A text adventure file has many declarations
txtadDecls = many txtadDecl

-- a declaration is either a start scene declaration, error message
-- declaration, or scene declaration
txtadDecl = txtadStart <||> txtadError <||> txtadScene

txtadStart = startDecl >>= \n -> return $ Start n
txtadError = errorDecl >>= \m -> return $ Msg m
txtadScene = sceneDecl >>= \(n, s) -> return $ Def n s

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

------------------------------------------------------------------------------
-- Parser functions for our language

sceneName :: PS.Parser Name
sceneName = identifier

-- start scene declaration:
startDecl :: PS.Parser Name
startDecl = do
  { reserved "start"
  ; reserved ":"
  ; name <- sceneName
  ; return name
  }

-- error message declaration:
errorDecl :: PS.Parser String
errorDecl = do
  { reserved "error"
  ; reserved ":"
  ; msg <- stringLiteral
  ; return msg
  }

-- scene declaration:
sceneDecl :: PS.Parser (String, Scene)
sceneDecl = do
  { reserved "scene"
  ; name <- sceneName
  ; sects <- braces $ many sectDecl
  ; let (txt, changes) = parseSections sects
  ; maybeFinal <- optionMaybe $ reserved "end"
  ; case maybeFinal of
      Just final -> return $ (name, (True, txt, changes))
      other -> return $ (name, (False, txt, changes))
  }

sectDecl :: PS.Parser (Either String (Input, Name))
sectDecl = do
  { txt <- stringLiteral
  ; maybeChange <- optionMaybe $ reserved "->"
  ; case maybeChange of
      Just change -> do
        { nextScene <- sceneName
        ; return $ Right (txt, nextScene)
        }
      other -> return $ Left txt
  }

parseSections :: [(Either String (Input, Name))] -> (String, [(Input, Name)])
parseSections [] = ("", [])
parseSections (s:sections) =
  let (allTxt, allChanges) = parseSections sections
  in  case s of
        Left txt -> (txt ++ allTxt, allChanges)
        Right change -> (allTxt, change : allChanges)

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = [":", "{", "}", "->"]
  , reservedNames   = ["start", "error", "end"]
  }

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
operator      = PT.operator    lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
stringLiteral = PT.stringLiteral  lexer
braces        = PT.braces      lexer
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Text.Prettyprint.EDoc.Q
( ppr
) where

import Language.Haskell.Meta.Parse ( parseExp )
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec

import Data.Text.Prettyprint.EDoc.Internal

ppr :: QuasiQuoter
ppr = QuasiQuoter { quoteExp = quoteEDocExp
                   , quotePat = error "Prettyprinting pattern not supported"
                   , quoteType = error "Prettyprinting type not supported"
                   , quoteDec = error "Prettyprinting decl not supported"
                   }

quoteEDocExp :: String -> TH.ExpQ
quoteEDocExp s =  do loc <- TH.location
                     let pos = (TH.loc_filename loc,
                               fst (TH.loc_start loc),
                               snd (TH.loc_start loc))
                     term <- parseTerm pos s
                     dataToExpQ (const Nothing `extQ` antiEDoc) term

parseTerm :: Monad m => (String, Int, Int) -> String -> m Term
parseTerm (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              flip setSourceName file $
              flip setSourceLine line $
              setSourceColumn pos col
            spaces
            e <- pTerm
            eof
            return e

data Term = THs String | TWrap Delim Term
          | THCat [Term] | THSep [Term] | TVSep [Term] | TVCat [Term]
          | TNest Int Term
  deriving (Show, Data)

lexeme :: Parsec String u a -> Parsec String u a
lexeme p     = do{ x <- p; spaces; return x  }
symbol :: String -> Parsec String u String
symbol name  = try (lexeme (string name))
-- parens       = between (symbol "(") (symbol ")")
delim :: Parsec String u a -> Parsec String u a
delim = between (symbol "$") (symbol "$")

escape :: Parsec String u String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parsec String u Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parsec String u String
character = fmap return nonEscape <|> escape

parseChar :: Parsec String u String
parseChar = do
    _ <- char '\''
    ch <- character
    _ <- char '\''
    spaces
    return $ "\'" ++ ch ++ "\'"

parseString :: Parsec String u String
parseString = do
    _ <- char '"'
    strings <- many character
    _ <- char '"'
    spaces
    return $ "\"" ++ concat strings ++ "\""



-- Precedences: hcat < hsep < vcat < vsep
-- symbols: hcat: '+'
--          hsep: '>'
--          vcat : '+/'
--          vsep : '//'
pTerm :: Parsec String u Term
pTerm = pVCat `chainl1` vsepSym

pVCat :: Parsec String u Term
pVCat = pHSep `chainl1` vcatSym

pHSep :: Parsec String u Term
pHSep = pHCat `chainl1` hsepSym

pHCat :: Parsec String u Term
pHCat = pAtom `chainl1` hcatSym

pAtom :: Parsec String u Term
pAtom =   (symbol "nest" >> ni >>= \i -> fmap (TNest i) (delim pTerm))
      <|> (symbol "parens" >> fmap (TWrap Parens) (delim pTerm))
      <|> (symbol "brackets" >> fmap (TWrap Brackets) (delim pTerm))
      <|> (symbol "braces" >> fmap (TWrap Braces) (delim pTerm))
      <|> (symbol "angles" >> fmap (TWrap Angles) (delim pTerm))
      <|> (THs <$> try parseString)
      <|> (THs <$> try parseChar)
      <|> (THs <$> manyTill anyChar delimp)
  where
    ni = read <$> many1 digit
    delimp =   try (lookAhead $ symbol "+")
           <|> try (lookAhead $ symbol ">")
           <|> try (lookAhead $ symbol "+/")
           <|> try (lookAhead $ symbol "//")
           <|> try (lookAhead $ symbol "$")
           <|> (try eof >> return "")


vsepSym :: Parsec String u (Term -> Term -> Term)
vsepSym = symbol "|" >> return f
  where
    f (TVSep l1) (TVSep l2) = TVSep $ l1 ++ l2
    f (TVSep l1) l2 = TVSep $ l1 ++ [l2]
    f l1 (TVSep l2) = TVSep $ l1 : l2
    f l1 l2 = TVSep [l1, l2]

vcatSym :: Parsec String u (Term -> Term -> Term)
vcatSym = try (symbol "+|") >> return f
  where
    f (TVCat l1) (TVCat l2) = TVCat $ l1 ++ l2
    f (TVCat l1) l2 = TVCat $ l1 ++ [l2]
    f l1 (TVCat l2) = TVCat $ l1 : l2
    f l1 l2 = TVCat [l1, l2]

hcatSym :: Parsec String u (Term -> Term -> Term)
hcatSym = try (symbol "+") >> return f
  where
    f (THCat l1) (THCat l2) = THCat $ l1 ++ l2
    f (THCat l1) l2 = THCat $ l1 ++ [l2]
    f l1 (THCat l2) = THCat $ l1 : l2
    f l1 l2 = THCat [l1, l2]

hsepSym :: Parsec String u (Term -> Term -> Term)
hsepSym = symbol ">" >> return f
  where
    f (THSep l1) (THSep l2) = THSep $ l1 ++ l2
    f (THSep l1) l2 = THSep $ l1 ++ [l2]
    f l1 (THSep l2) = THSep $ l1 : l2
    f l1 l2 = THSep [l1, l2]

antiEDoc :: Term -> Maybe TH.ExpQ
antiEDoc v =
     case pHs v of
         Right e -> Just [| pretty $(e) |]
         Left _ -> Nothing

pHs :: Term -> Either String TH.ExpQ
pHs (THs v) =
    case parseExp  v of
        Right e -> Right [| Hs $(return e) |]
        Left e -> Left e

pHs (TWrap d t) =
    case pHs t of
       Right e -> Right [| Wrap $(liftD d) $(e) |]
       Left e  -> Left e
  where
    liftD Parens = [| Parens |]
    liftD Braces = [| Braces |]
    liftD Brackets = [| Brackets |]
    liftD Angles = [| Angles |]

pHs (THCat t) =
    case pHss t of
       Right e -> Right [| HCat $(e) |]
       Left e  -> Left e

pHs (THSep t) =
    case pHss t of
       Right e -> Right [| HSep $(e) |]
       Left e  -> Left e

pHs (TVCat t) =
    case pHss t of
       Right e -> Right [| VCat $(e) |]
       Left e  -> Left e

pHs (TVSep t) =
    case pHss t of
       Right e -> Right [| VSep $(e) |]
       Left e  -> Left e

pHs (TNest i t) =
    case pHs t of
       Right e -> Right [| Nest i $(e) |]
       Left e  -> Left e

pHss :: [Term] -> Either String TH.ExpQ
pHss [] = return [| [] |]
pHss (t:ts) = case pHs t of
                Right e -> case pHss ts of
                             Right es -> Right [| $(e) : $(es) |]
                             Left er -> Left er
                Left e -> Left e

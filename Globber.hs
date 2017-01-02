module Globber (matchGlob) where

data GlobASTNode = Lit Char
                 | Question
                 | Kleene
                 | SetMatch [GlobASTNode]
                 | RangeMatch Char Char deriving (Eq, Show)

type GlobAST = [GlobASTNode]

type Result a = Either String (a, String)

type Parser a = String -> Result a

many' :: Parser a -> [a] -> String -> ([a], String)
many' f' acc inp =
  case f' inp of
    Left _          -> (acc, inp)
    Right (a, inp') -> many' f' (acc ++ [a]) inp'

many :: Parser a -> Parser [a]
many f = Right . many' f []

choice :: [Parser a] -> Parser a
choice []     _ = Left "no matching parser"
choice (p:ps) i = either (const $ choice ps i) Right $ p i

parseGlobPattern :: Parser GlobAST
parseGlobPattern = many globAstNode

globAstNode :: Parser GlobASTNode
globAstNode = choice [escaped, setMatch, question, kleene, literal ['\\', '?', '*']]

escaped :: Parser GlobASTNode
escaped ('\\':(ih:it)) = Right (Lit ih, it)
escaped ('\\':[])      = Left "Terminating single backslash"
escaped _              = Left "No escaped character"

question :: Parser GlobASTNode
question ('?':i) = Right (Question, i)
question _       = Left "No '?' found"

kleene :: Parser GlobASTNode
kleene ('*':i) = Right (Kleene, i)
kleene _       = Left "No '*' found"

literal :: [Char] -> String -> Result GlobASTNode
literal _ [] = Left "Premature EOF to literal parser"
literal blacklist (c:cs)
  | c `elem` blacklist = Left $ "Found '" ++ [c] ++ "' char in literal parser"
  | otherwise          = Right (Lit c, cs)

setMatch :: Parser GlobASTNode
setMatch ('[':i) =
  case setMatchChars [] i of
    Left err          -> Left err
    Right (inSet, i') -> Right (SetMatch $ innerPs [] inSet, i')
  where setMatchChars :: [Char] -> Parser [Char]
        setMatchChars _   ""       = Left "Set match called with unmatched ']'"
        setMatchChars acc (']':cs) = Right (acc, cs)
        setMatchChars acc (c:cs)   = setMatchChars (acc ++ [c]) cs
        innerPStep :: [GlobASTNode] -> String -> GlobASTNode -> [GlobASTNode]
        innerPStep acc cs p = innerPs (acc ++ [p]) cs
        innerPs :: [GlobASTNode] -> String -> [GlobASTNode]
        innerPs acc ""                              = acc
        innerPs acc ('\\':(c:('-':('\\':(c':cs))))) = innerPsPostDash (acc  ++ [RangeMatch c c']) cs
        innerPs acc (c:('-':('\\':(c':cs))))        = innerPsPostDash (acc  ++ [RangeMatch c c']) cs
        innerPs acc ('\\':(c:('-':(c':cs))))        = innerPsPostDash (acc  ++ [RangeMatch c c']) cs
        innerPs acc (c:('-':(c':cs)))               = innerPsPostDash (acc  ++ [RangeMatch c c']) cs
        innerPs acc ('\\':(c:cs))                   = innerPStep acc cs (Lit c)
        innerPs acc (c:cs)                          = innerPStep acc cs (Lit c)
        innerPsPostDash :: [GlobASTNode] -> String -> [GlobASTNode]
        innerPsPostDash acc ('\\':(c:cs)) = innerPsPostDash (acc ++ [Lit c]) cs
        innerPsPostDash acc (c:cs)        = innerPsPostDash (acc ++ [Lit c]) cs
        innerPsPostDash acc ""            = acc
setMatch _ = Left "No '[' for set match"

interpret :: GlobAST -> String -> Bool
interpret [Kleene]                 _      = True
interpret []                       ""     = True
interpret []                       _      = False
interpret _                        ""     = False
interpret (Kleene:gs)              (c:cs) = interpret gs (c:cs) || interpret (Kleene:gs) cs
interpret (Question:gs)            (_:cs) = interpret gs cs
interpret (Lit l:gs)               (c:cs) = l == c && interpret gs cs
interpret (SetMatch []:_)          _      = False
interpret ((SetMatch [g]):gs)      (c:cs) = interpret (g:gs) (c:cs)
interpret ((SetMatch gs) : gs')    str    = any (\node -> interpret (node:gs') str) gs
interpret ((RangeMatch a z)  : gs) (c:cs) = elem c [a..z] && interpret gs cs

matchGlob :: String -> String -> Bool
matchGlob pat str =
  case parseGlobPattern pat of
    Right (ast, "") ->
      interpret ast str
    _ -> False

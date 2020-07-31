module Parser where

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Eq, Show)

newtype Parser a =
  Parser { runParser :: String -> Maybe (String, a) }


charP :: Char -> Parser Char
charP x = Parser $ \input@(z:zs) ->
          case input of
            y:ys | y == x -> Just (zs, x)
            _ -> Nothing

stringP :: String -> Parser String
stringP xs = sequenceA $ charP <$> xs

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> do
    (ys, a) <- g s
    return $ (ys, f a)

instance Applicative Parser where
  pure a = Parser $ \_ -> Just (mempty, a)
  (Parser fab) <*> (Parser fa) = Parser $ \xs -> do
     (ys, f) <- fab xs
     (zs, a) <- fa ys 
     return $ (zs, f a)

main :: IO ()
main = undefined

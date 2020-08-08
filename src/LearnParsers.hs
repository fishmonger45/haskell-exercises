module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

one:: Parser a
one = char '1'
one' :: Parser a
one' = one >> stop
one'' :: Parser a
one'' = do
  one
  stop

-- (>>) :: Monad m => m a -> m b -> m b
-- type Parser a = String -> Maybe (a, String)

--f = (runStateT $ put 2 >> return 5) 0
--

oneTwo' :: Parser a
oneTwo :: Parser a
oneTwo = char '1' >> char '2' >> eof
oneTwo' = char '1' >> char '2' >> stop

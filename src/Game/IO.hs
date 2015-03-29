module Game.IO where

class Pretty p where
    -- | Convert to a pretty formatted string
    pretty :: p -> String

class Parsable p where
    -- | Convert a formatted string to a type
    parse :: String -> p
    
    -- | Convert a char to a type, a shorthand when the string is just one character
    parseC :: Char -> p
    parseC = parse . (:[])
    
instance (Parsable a) => Parsable (Maybe a) where
    parse "_" = Nothing
    parse x = Just (parse x)

instance (Pretty a) => Pretty (Maybe a) where
    pretty Nothing  = "_"
    pretty (Just x) = pretty x

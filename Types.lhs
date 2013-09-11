A simple datatype, and perhaps som common utility function
for working with propositional logic

>module Types where
>data Pexp = Bot | Top 
>                | Atom Int
>                | Neg Pexp
>                | Pexp :\/: Pexp
>                | Pexp :/\: Pexp
>                | Pexp :->: Pexp deriving (Show)



A simple validity checker of a propositional formula using CNF.

First some imports
>module CNF where
>import Data.List(partition)
>import Control.Arrow(first,second)
>import Types(Pexp(..))

First how to convert a formula to CNF.
First we remove implication, push negation to inner-most level,
and distributivity of \/ and /\ to get the right form

>toCNF :: Pexp -> Pexp
>toCNF = distr . pushNeg . removeImpl
>
>removeImpl (Neg f) = Neg (removeImpl f)
>removeImpl (p :/\: q) = (removeImpl p) :/\: (removeImpl q)
>removeImpl (p :\/: q) = (removeImpl p) :\/: (removeImpl q)
>removeImpl (p :->: q) = (Neg (removeImpl p)) :\/: (removeImpl q)
>removeImpl pexp = pexp
>
>pushNeg (Neg Bot) = Top
>pushNeg (Neg Top) = Bot
>pushNeg (Neg (Neg p)) = pushNeg p
>pushNeg (Neg (p :\/: q)) = pushNeg (Neg p) :/\: pushNeg (Neg q)
>pushNeg (Neg (p :/\: q)) = pushNeg (Neg p) :\/: pushNeg (Neg q)
>pushNeg (Neg p) = Neg p
>pushNeg (p :\/: q) = pushNeg (p) :\/: pushNeg q
>pushNeg (p :/\: q) = pushNeg (p) :/\: pushNeg q
>pushNeg p = p
>
>distr ((p :/\: q) :\/: r) = (distr (p :\/: r)) :/\: (distr (q :\/: r)) 
>distr (p :\/: (r :/\: s)) = (distr (p :\/: r)) :/\: (distr (p :\/: s))
>distr (p :/\: q) =  (distr p) :/\: (distr q)
>distr p = p
>

To check validity, we check that every disjunction contains
every positiv atom in negativ form aswell.
Todo this we first convert the cnf to a list

>cnfToList = map (atomList . disList) . conList
>conList (p :/\: q) = p : conList q
>conList p = [p]
>disList (p :\/: q) = p : disList q
>disList p = [p]
>atomList = first (map stripNeg) . second (map stripAtom) . partition isNeg
>isNeg (Neg _) = True
>isNeg _ = False
>stripAtom (Atom p) = p
>stripNeg (Neg (Atom p)) = p
>
>isValidSubCNF (negps,ps) = all (`elem` negps) ps 
>
>isValid :: Pexp -> Bool
>isValid = all isValidSubCNF . cnfToList . toCNF

some atoms to help you start playing

>(p,q,r,s) = (Atom 1,Atom 2,Atom 3,Atom 4)

isValid (p :->: Neg (p:/\:q))
True
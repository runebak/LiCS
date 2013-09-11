A naive satisfiability checker for Hornclauses
First some imports

>module Horn where
>import Data.Maybe(mapMaybe)

We represent a Hornclause, as a list of 
hornFractments, and represent a fragment
p /\ q -> r as ([p,q],r)

TODO: Hornclause checker and converter from Types.Pexp

>type Horn = [HornFracment]
>type HornFracment = ([Propersition],Propersition)

a propersition is simple an integer

>type Propersition = Integer
>(bot,top,p,q,r,s,v,w) = (0,1,2,3,4,5,6,7)

>type Markings = [Propersition]

The algorithm work by markings.
first we mark top

>satisfiable :: Horn -> Maybe Markings
>satisfiable = flip satisfiable' [top]

then we check if we should mark more, if
there a new markings we add them to the list of
markings and keep going otherwise if we've marked _|_
it's unsatisfiable, if not we return the markings.


>satisfiable' :: Horn -> Markings -> Maybe Markings
>satisfiable' hoare markings = case mapMaybe (newMark markings) hoare of 
>                                   [] -> if bot `notElem` markings then Just markings else Nothing 
>                                   l -> satisfiable' hoare $ l ++ markings

we should mark a new atom of p_1 /\ ... /\ p_n -> q if
all p's a marked but q is not

>newMark :: Markings -> HornFracment -> Maybe Propersition
>newMark markings (props,prop) = if all (`elem` markings) props && prop `notElem` markings then
>                                   Just prop
>                                else Nothing

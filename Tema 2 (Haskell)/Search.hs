{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = TreeNode {  state :: s,
                            action :: Maybe a,
                            parent :: Maybe (Node s a),
                            depth :: Int,
                            children :: [Node s a] }
                            deriving (Show)

instance Eq s => Eq (Node s a) where
    (TreeNode s1 _ _ _ _) == (TreeNode s2 _ _ _ _) = s1 == s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (TreeNode currState _ _ _ _) = currState

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (TreeNode _ _ currParent _ _) = currParent

nodeDepth :: Node s a -> Int
nodeDepth (TreeNode _ _ _ currDepth _) = currDepth

nodeAction :: Node s a -> Maybe a
nodeAction (TreeNode _ currAction _ _ _) = currAction

nodeChildren :: Node s a -> [Node s a]
nodeChildren (TreeNode _ _ _ _ currChildren) = currChildren

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

recursiveStates :: (ProblemState s a, Eq s) => (a, s) -> Node s a -> Int -> Node s a    --Primeste un nod , face nod din el si din copiii lui
recursiveStates (actiune, stare) parinte adancime = new_node
    where
        new_node = TreeNode stare (Just actiune) (Just parinte) adancime children_Nodes --Construiesc nodul in sine
        children_Nodes = [(recursiveStates curr_succ new_node (adancime + 1)) | curr_succ <- (successors stare)] --Construiesc nodurile pentru copiii lui


createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initial_state =
    let firstNode = TreeNode initial_state Nothing Nothing 0 first_children
        first_children = [(recursiveStates curr_succ firstNode 1) | curr_succ <- (successors initial_state)]
        in firstNode

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsUtil :: (Ord s) => Node s a -> [([Node s a], [Node s a])] -> [s] -> [([Node s a], [Node s a])]
bfsUtil start accumulator visited
    -- | (accumulator == [] && (elem curr_state visited) == False) = bfsUtil (head copii) ([(copii, copii)] ++ accumulator) (visited ++ [curr_state]) --primul apel de bfs, bag toti copiii in frontiera
    | frontiera == [] = accumulator --am terminat de vizitat
    | (elem curr_state visited) == True = accumulator --bfsUtil (head (tail frontiera)) [([], (tail frontiera))] (visited ++ [curr_state])
    | otherwise = bfsUtil (head ((tail frontiera) ++ unvisited_children)) [((unvisited_children), (tail frontiera) ++ unvisited_children)] (visited ++ [curr_state]) --bag copiii in frontiera, scot din frontiera primul nod
    where
        curr_state = nodeState start --nivelul nodului respectiv
        copii = nodeChildren start
        frontiera = snd (head accumulator)
        unvisited_children = [ curr_child | curr_child <- copii, (elem (nodeState curr_child) visited) == False] --copii nevizitati


bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs nod = bfsUtil nod [([],[nod])] []

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined

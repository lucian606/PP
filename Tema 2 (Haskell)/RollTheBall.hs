{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data Cell = HorPipe | VerPipe | TopLeft | BotLeft | BotRight | TopRight | EmptySpace |
            EmptyCell | StartUp | StartDown | StartLeft | StartRight | WinUp | WinDown |
            WinLeft | WinRight
    deriving (Eq,Ord)

instance Show Cell where
    show HorPipe = [horPipe]
    show VerPipe = [verPipe]
    show TopLeft = [topLeft]
    show BotLeft = [botLeft]
    show BotRight = [botRight]
    show TopRight = [topRight]
    show EmptySpace = [emptySpace]
    show EmptyCell = [emptyCell]
    show StartUp = [startUp]
    show StartDown = [startDown]
    show StartLeft = [startLeft]
    show StartRight = [startRight]
    show WinUp = [winUp]
    show WinDown = [winDown]
    show WinLeft = [winLeft]
    show WinRight = [winRight]

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = EmptyLevel |
             NormalLevel { levelCells :: (A.Array Position Cell) }--TODO
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
foldable_show :: [Char] -> ((Int, Int), Cell) -> [Char]
foldable_show acc element =
    if (snd (fst element) == 0) then --daca e coloana noua pun endl
        (acc ++ [endl] ++ (show (snd element)))
    else (acc ++ (show (snd element)))
--functia primeste un acumulator si un element din arrayul de celluri, functia face concat
--de acc si toString cell si adauga endl daca e coloana noua
--folosita la EmptyLevel

instance Show Level where
    show EmptyLevel = ""
    show (NormalLevel cells) = (foldl foldable_show "" (A.assocs cells)) ++ [endl] --adaug la final endl

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (max_lines, max_columns) = NormalLevel (A.array ((0,0), (max_lines,max_columns)) [((i,j), EmptySpace) | i <- [0..max_lines], j<-[0..max_columns]])

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell _ EmptyLevel = EmptyLevel
addCell (tip , pos) (NormalLevel cells)
    | x < 0 = NormalLevel cells
    | y < 0 = NormalLevel cells
    | x > (fst (snd (bounds cells))) = NormalLevel cells
    | y > (snd (snd (bounds cells))) = NormalLevel cells
    | tip == emptySpace = (NormalLevel (cells//[((x,y), EmptySpace)])) --PENTRU STERGERE/SWAP
    | (show (cells A.! (x, y)) /= [emptySpace]) = NormalLevel cells
    | tip == emptyCell = (NormalLevel (cells//[((x,y), EmptyCell)]))
    | tip == verPipe = (NormalLevel (cells//[((x,y), VerPipe)]))
    | tip == horPipe = (NormalLevel (cells//[((x,y), HorPipe)]))
    | tip == topLeft = (NormalLevel (cells//[((x,y), TopLeft)]))
    | tip == botLeft = (NormalLevel (cells//[((x,y), BotLeft)]))
    | tip == topRight = (NormalLevel (cells//[((x,y), TopRight)]))
    | tip == botRight = (NormalLevel (cells//[((x,y), BotRight)]))
    | tip == startUp = (NormalLevel (cells//[((x,y), StartUp)]))
    | tip == startDown = (NormalLevel (cells//[((x,y), StartDown)]))
    | tip == startLeft = (NormalLevel (cells//[((x,y), StartLeft)]))
    | tip == startRight = (NormalLevel (cells//[((x,y), StartRight)]))
    | tip == winUp = (NormalLevel (cells//[((x,y), WinUp)]))
    | tip == winDown = (NormalLevel (cells//[((x,y), WinDown)]))
    | tip == winLeft = (NormalLevel (cells//[((x,y), WinLeft)]))
    | tip == winRight = (NormalLevel (cells//[((x,y), WinRight)]))
    | otherwise = NormalLevel cells
    where
        x = fst pos
        y = snd pos
{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel (max_lines, max_columns) things_to_add = foldr addCell (emptyLevel (max_lines, max_columns)) things_to_add


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell _ _ EmptyLevel = EmptyLevel
moveCell (x, y) North (NormalLevel cells)
    | x - 1 < 0 = NormalLevel cells
    | show (cells A.! (x - 1, y)) /= [emptySpace] = (NormalLevel cells)   --Daca nu am celula empty space nu pot muta
    | show (cells A.! (x, y)) == [startDown] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = (NormalLevel cells) --Nu pot muta winuurile
    | show (cells A.! (x, y)) == [horPipe] = addCell (horPipe, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [verPipe] = addCell (verPipe, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie    
    | show (cells A.! (x, y)) == [topLeft] = addCell (topLeft, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [topRight] = addCell (topRight, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botLeft] = addCell (botLeft, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botRight] = addCell (botRight, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie 
    | show (cells A.! (x, y)) == [emptyCell] = addCell (emptyCell, (x-1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie   
    | otherwise = NormalLevel cells     

moveCell (x, y) South (NormalLevel cells)
    | x + 1 > (fst (snd (bounds cells))) = NormalLevel cells
    | show (cells A.! (x + 1, y)) /= [emptySpace] = (NormalLevel cells)   --Daca nu am celula empty space nu pot muta
    | show (cells A.! (x, y)) == [startDown] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = (NormalLevel cells) --Nu pot muta winuurile
    | show (cells A.! (x, y)) == [horPipe] = addCell (horPipe, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [verPipe] = addCell (verPipe, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie    
    | show (cells A.! (x, y)) == [topLeft] = addCell (topLeft, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [topRight] = addCell (topRight, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botLeft] = addCell (botLeft, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botRight] = addCell (botRight, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie   
    | show (cells A.! (x, y)) == [emptyCell] = addCell (emptyCell, (x + 1, y)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie     
    | otherwise = NormalLevel cells

moveCell (x, y) East (NormalLevel cells)
    | y + 1 > (snd (snd (bounds cells))) = NormalLevel cells
    | show (cells A.! (x, y + 1)) /= [emptySpace] = (NormalLevel cells)   --Daca nu am celula empty space nu pot muta
    | show (cells A.! (x, y)) == [startDown] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = (NormalLevel cells) --Nu pot muta winuurile
    | show (cells A.! (x, y)) == [horPipe] = addCell (horPipe, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [verPipe] = addCell (verPipe, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie    
    | show (cells A.! (x, y)) == [topLeft] = addCell (topLeft, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [topRight] = addCell (topRight, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botLeft] = addCell (botLeft, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botRight] = addCell (botRight, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie   
    | show (cells A.! (x, y)) == [emptyCell] = addCell (emptyCell, (x, y + 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie    
    | otherwise = NormalLevel cells     

moveCell (x, y) West (NormalLevel cells)
    | y - 1 < 0 = NormalLevel cells
    | show (cells A.! (x, y - 1)) /= [emptySpace] = (NormalLevel cells)   --Daca nu am celula empty space nu pot muta
    | show (cells A.! (x, y)) == [startDown] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = (NormalLevel cells) --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = (NormalLevel cells) --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = (NormalLevel cells) --Nu pot muta winuurile
    | show (cells A.! (x, y)) == [horPipe] = addCell (horPipe, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [verPipe] = addCell (verPipe, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie    
    | show (cells A.! (x, y)) == [topLeft] = addCell (topLeft, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [topRight] = addCell (topRight, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botLeft] = addCell (botLeft, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie
    | show (cells A.! (x, y)) == [botRight] = addCell (botRight, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie   
    | show (cells A.! (x, y)) == [emptyCell] = addCell (emptyCell, (x, y - 1)) (addCell (emptySpace, (x, y)) (NormalLevel cells)) --sterg celula curenta si o pun in noua pozitie     
    | otherwise = NormalLevel cells     

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection HorPipe HorPipe East = True
connection HorPipe HorPipe West = True
connection HorPipe TopLeft West = True
connection HorPipe BotLeft West = True
connection HorPipe BotRight East = True
connection HorPipe TopRight East = True
connection HorPipe StartLeft East = True
connection HorPipe StartRight West = True
connection HorPipe WinLeft East = True
connection HorPipe WinRight West = True
connection HorPipe _ _ = False

connection VerPipe VerPipe South = True
connection VerPipe VerPipe North = True
connection VerPipe TopLeft North = True
connection VerPipe BotLeft South = True
connection VerPipe BotRight South = True
connection VerPipe TopRight North = True
connection VerPipe StartUp South = True
connection VerPipe StartDown North = True
connection VerPipe WinUp South = True
connection VerPipe WinDown North = True
connection VerPipe _ _ = False

connection TopLeft HorPipe East = True
connection TopLeft VerPipe South = True
connection TopLeft BotLeft South = True
connection TopLeft BotRight South = True
connection TopLeft BotRight East = True
connection TopLeft TopRight East = True
connection TopLeft StartUp South = True
connection TopLeft StartLeft East = True
connection TopLeft WinUp South = True
connection TopLeft WinLeft East = True
connection TopLeft _ _ = False

connection BotLeft HorPipe East = True
connection BotLeft VerPipe North = True
connection BotLeft TopLeft North = True
connection BotLeft BotRight East = True
connection BotLeft TopRight North = True
connection BotLeft TopRight East = True
connection BotLeft StartDown North = True
connection BotLeft StartLeft East = True
connection BotLeft WinDown North = True
connection BotLeft WinLeft East = True
connection BotLeft _ _ = False

connection BotRight HorPipe West = True
connection BotRight VerPipe North = True
connection BotRight TopLeft North = True
connection BotRight TopLeft West = True
connection BotRight BotLeft West = True
connection BotRight TopRight North = True
connection BotRight StartDown North = True
connection BotRight StartRight West = True
connection BotRight WinDown North = True
connection BotRight WinRight North = True
connection BotRight _ _ = False

connection TopRight HorPipe West = True
connection TopRight VerPipe South = True
connection TopRight TopLeft West = True
connection TopRight BotLeft South = True
connection TopRight BotLeft West = True
connection TopRight BotRight South = True
connection TopRight StartUp South = True
connection TopRight StartRight West = True
connection TopRight WinUp South = True
connection TopRight WinRight West = True
connection TopRight _ _ = False

connection StartUp VerPipe North = True
connection StartUp TopLeft North = True
connection StartUp TopRight North = True
connection StartUp StartDown North = True
connection StartUp WinDown North = True
connection StartUp _ _ = False

connection StartDown VerPipe South = True
connection StartDown BotLeft South = True
connection StartDown BotRight South = True
connection StartDown StartUp South = True
connection StartDown WinDown South = True
connection StartDown _ _ = False

connection StartLeft HorPipe West = True
connection StartLeft BotLeft West = True
connection StartLeft TopLeft West = True
connection StartLeft StartRight West = True
connection StartLeft WinRight West = True
connection StartLeft _ _ = False

connection StartRight HorPipe East = True
connection StartRight BotRight East = True
connection StartRight TopRight East = True
connection StartRight StartLeft East = True
connection StartRight WinLeft East = True
connection StartRight  _ _ = False

connection _ _ _ = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
findStart :: (Position, Cell) -> Bool
findStart cellList = (show (snd cellList)) == [startUp] || (show (snd cellList)) == [startDown] || (show (snd cellList)) == [startLeft] || (show (snd cellList)) == [startRight]

findEnd :: (Position, Cell) -> Bool
findEnd cellList = (show (snd cellList)) == [winUp] || (show (snd cellList)) == [winDown] || (show (snd cellList)) == [winLeft] || (show (snd cellList)) == [winRight]

traverseLevel :: (Position) -> Level -> Bool
traverseLevel _ EmptyLevel = False
traverseLevel (x, y) (NormalLevel cells)
    | (findEnd ((x, y), (cells A.! (x, y))) == True) = True -- Daca e celula de finish win
    | (((x - 1) >= 0) && connection (cells A.! (x, y)) (cells A.! (x-1, y)) North == True) = (traverseLevel (x - 1, y) (addCell (emptySpace ,(x,y)) (NormalLevel cells)))
    | (((x + 1) <= (fst (snd (bounds cells)))) && connection (cells A.! (x, y)) (cells A.! (x + 1, y)) South == True) = (traverseLevel (x + 1, y) (addCell (emptySpace ,(x,y)) (NormalLevel cells)))
    | (((y - 1) >= 0) && connection (cells A.! (x, y)) (cells A.! (x, y - 1)) West == True) = (traverseLevel (x, y - 1) (addCell (emptySpace ,(x,y)) (NormalLevel cells)))
    | (((y + 1) <= (snd (snd (bounds cells)))) && connection (cells A.! (x, y)) (cells A.! (x, y + 1)) East == True) = (traverseLevel (x, y + 1) (addCell (emptySpace ,(x,y)) (NormalLevel cells)))
    | otherwise = False

wonLevel :: Level -> Bool
wonLevel EmptyLevel = False
wonLevel (NormalLevel cells) = traverseLevel (fst (head (filter findStart (A.assocs cells)))) (NormalLevel cells)

checkNorth :: Level -> [((Position, Directions), Level)] -> (Position, Cell) -> [((Position, Directions), Level)]
checkNorth EmptyLevel _ _ = []
checkNorth (NormalLevel cells) acc  ((x,y), _)
    | x - 1 < 0 = acc
    | show (cells A.! (x, y)) == [startDown] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = acc--Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = acc--Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [emptySpace] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = acc --Nu pot muta winuurile
    | show (cells A.! (x - 1, y)) /= [emptySpace] = acc
    | otherwise = acc ++ [(((x,y), North), moveCell (x,y) North (NormalLevel cells))]

checkSouth :: Level -> [((Position, Directions), Level)] -> (Position, Cell) -> [((Position, Directions), Level)]
checkSouth EmptyLevel _  _ = []
checkSouth (NormalLevel cells) acc  ((x,y), _)
    | x + 1 > (fst (snd (bounds cells))) = acc
    | show (cells A.! (x, y)) == [startDown] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = acc--Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = acc --Nu pot muta starturi
    | show (cells A.! (x, y)) == [emptySpace] = acc --Nu pot muta winurilele
    | show (cells A.! (x, y)) == [winDown] = acc--Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = acc --Nu pot muta winuurile
    | show (cells A.! (x + 1, y)) /= [emptySpace] = acc
    | otherwise = acc ++ [(((x,y), South), moveCell (x,y) South (NormalLevel cells))]

checkEast :: Level -> [((Position, Directions), Level)] -> (Position, Cell) -> [((Position, Directions), Level)]
checkEast EmptyLevel _ _ = []
checkEast (NormalLevel cells) acc  ((x,y), _)
    | y - 1 < 0 = acc
    | show (cells A.! (x, y)) == [startDown] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = acc--Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = acc--Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [emptySpace] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = acc --Nu pot muta winuurile
    | show (cells A.! (x, y - 1)) /= [emptySpace] = acc
    | otherwise = acc ++ [(((x,y), West), moveCell (x,y) West (NormalLevel cells))]

checkWest :: Level -> [((Position, Directions), Level)] -> (Position, Cell) -> [((Position, Directions), Level)]
checkWest EmptyLevel _ _ = []
checkWest (NormalLevel cells) acc  ((x,y), _)
    | y + 1 > (snd (snd (bounds cells))) = acc
    | show (cells A.! (x, y)) == [startDown] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startUp] = acc--Nu pot muta starturile
    | show (cells A.! (x, y)) == [startLeft] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [startRight] = acc --Nu pot muta starturile
    | show (cells A.! (x, y)) == [winDown] = acc--Nu pot muta winurile
    | show (cells A.! (x, y)) == [winUp] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winLeft] = acc --Nu pot muta winurile
    | show (cells A.! (x, y)) == [winRight] = acc --Nu pot muta winuurile
    | show (cells A.! (x, y)) == [emptySpace] = acc --Nu pot muta winurile
    | show (cells A.! (x, y + 1)) /= [emptySpace] = acc
    | otherwise = acc ++ [(((x,y), East), moveCell (x,y) East (NormalLevel cells))]

instance ProblemState Level (Position, Directions) where
    successors EmptyLevel = []
    successors (NormalLevel cells) = (foldl (checkEast (NormalLevel cells)) [] (A.assocs cells)) ++ (foldl (checkNorth (NormalLevel cells)) [] (A.assocs cells)) ++ (foldl (checkSouth (NormalLevel cells)) [] (A.assocs cells)) ++ (foldl (checkWest (NormalLevel cells)) [] (A.assocs cells))
    isGoal lvl = wonLevel lvl
    reverseAction (((x, y), South), level) = (((x + 1, y), North) , (moveCell (x + 1, y) North level))
    reverseAction (((x, y), North), level) = (((x - 1, y), South) , (moveCell (x - 1, y) South level))
    reverseAction (((x, y), East), level) = (((x, y + 1), West) , (moveCell (x, y + 1) West level))
    reverseAction (((x, y), West), level) = (((x, y - 1), East) , (moveCell (x, y - 1) East level))
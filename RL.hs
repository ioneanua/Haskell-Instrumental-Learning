module RL where

import Data.Array
import Data.List
import System.Random
import Text.Printf
import Data.List.Split hiding (split)

{-
    O stare este reprezentată ca un număr întreg.
    O cale este o listă de stări, eventual infinită.

    O estimare reprezintă o mulțime de asocieri (stare, informație aferentă).
    Cu toate că mediul este bidimensional, utilizăm o reprezentare liniară
    a acestuia, bazată pe un `Array`, cu stările indexate ca în figura din 
    enunț.
-}
type State      = Int
type Path       = [State]
type Estimation = Array State StateInfo

{-
    Lățimea și înălțimea mediului, precum și numărul de stări.
-}
width, height, nStates :: Int
width   = 4
height  = 3
nStates = width * height

{-
    Perechile de stări vecine.
-}
neighbors :: [(State, State)]
neighbors = [ (1, 2), (1, 5)
            , (2, 1), (2, 3)
            , (3, 2), (3, 4)
            , (3, 7)
            , (4, 3), (4, 8)
            , (5, 1), (5, 9)
            , (7, 3), (7, 8), (7, 11)
            , (8, 4), (8, 7), (8, 12)
            , (9, 5), (9, 10)
            , (10, 9), (10, 11)
            , (11, 7), (11, 10), (11, 12)
            , (12, 8), (12, 11)
            ]

{-
    Starea de pornire.
-}
startState :: State
startState = 1

{-
     Stările terminale.
-}
terminalStates :: [State]
terminalStates = [8, 12]

{-
    Rata de învățare alfa și factorul de scalare a acesteia.
-}
learningRate, scaleFactor :: Float
learningRate = 0.1
scaleFactor  = 0.999

-------------------------------------------------------------------------------
-- Completați sub această linie.


--  === 1. Generarea căilor ===

{-
    *** TODO ***

    Întoarce toate stările vecine ale unei stări.
-}

--primeste starea x si ii intoarce vecinii, o lista de stari
neighborsOf :: State -> [State]
neighborsOf x = map snd (filter ((==x).fst) neighbors)

{-
    *** TODO ***

    Construiește o cale aleatoare infinită, pe baza unui generator.

    Hint: `Data.List.iterate`, `System.Random.split`.
-}

--am folosit split pentru a impartii generatorul dat functiei in doua generatoare g1 si g2, randomPath creeaza pe baza generatorului o pereche formata
--dintr-o lista cu drumul si un generator, iterate facand o lista de astfel de perechi infinita
randomPath :: RandomGen g => g -> (Path, g)
randomPath g = (l, g2)
	where
	l = map fst (iterate funcGen (startState, g1))
	(g1, g2) = split g

--funcGen - returneaza starea aleatoare din lista de stari vecine ale lui State
funcGen :: RandomGen g => (State, g) -> (State, g)
funcGen (state, g) = ((neighborsOf state) !! ((fst (next g)) `mod` (length (neighborsOf state))), snd (next g))


{-
    *** TODO ***

    Trunchiază o cale, eventual infinită, la prima stare terminală.
-}

--daca italneste 8 sau 12 sau este o lista vida, recursivitatea se va opri
terminatePath :: Path -> Path
terminatePath path
 | null path = []
 | (head path == 8) || (head path == 12) = head path : []
 | otherwise = (head path) : terminatePath (tail path)
		
{-
    *** TODO ***

    Construiește o infinitate de căi infinite.
-}

--primeste un generator si plecand de la starea de inceput se apeleaza iterate cu functia randomPath -> ramanand doar lista cu primul element
randomPaths :: RandomGen g => g -> [Path]
randomPaths g = map fst (iterate (\a -> randomPath (snd a)) ([startState], g) )



--  === 2. Estimarea utilităților fără diminuarea ratei de învățare ===

{-
    *** TODO ***

    Array cu conscințele specifice fiecărei stări.
-}

--construieste un array pe baza enuntului problemei; fix ca in imaginea de la *Scopul Temei
reinforcements :: Array State Float
reinforcements = array (1, nStates) ([(i,0) | i <- [1 .. 7]] ++ [(8,-1)] ++ [(i,0) | i <- [9 .. 11]] ++ [(12,1)])

{-
    *** TODO ***

    Valorile inițiale ale stărilor, înaintea rulării algoritmului.
    Se construiesc pe baza array-ului de consecințe.
-}

--contruieste un array la fel ca cel de sus, doar ca acum ficare stare contine si tipul de date StateInfo
initialEstimation :: Estimation
initialEstimation = array (1, nStates) ([(i,(StateInfo 0.0 0)) | i <- [1 .. 7]] ++ [(8,(StateInfo (-1.0) 0))] ++ [(i,(StateInfo 0.0 0)) | i <- [9 .. 11]] ++ [(12,(StateInfo 1 0))])


{-
    *** TODO ***

    Lista de utilități provenite dintr-o estimare.
-}

--ia lista de estimari si de la fiecare stare ia nr de vizitari de la StateInfo
values :: Estimation -> [Float]
values estim = map (\x -> takeSnd x) (assocs estim)

--ia de la o stare anume, al doilea argument de la tipul de date StateInfo
takeSnd :: (State,StateInfo) -> Float
takeSnd (st,sti) = valoare sti 

{-
    *** TODO ***

    Reprezentarea sub formă de șir de caractere a unei estimări.
    Se va întrebuința forma bidimensională, ca în imaginile din enunț.
    De asemenea, utilitățile vor fi rotunjite la 2 zecimale, și vor
    avea semnul inclus.

    Hint: `Text.Printf`.

    Exemplu de rezultat:

    -0.07 +0.06 +0.20 +1.00
    -0.20 +0.00 -0.43 -1.00
    -0.32 -0.45 -0.56 -0.78

    Pentru a vizualiza corect caracterele de linie nouă, aplicați
    în interpretor funcția `putStrLn` asupra șirului obținut.
-}

--splitPlaces imparte estimarea in 3 perechi de cate 4 nr si aplicam reverse pentru a le aranja ca in exemplul de mai sus
showEstimation :: Estimation -> String
showEstimation est =  take lengthStr str
	where 
		rev = reverse (splitPlaces [4,4,4] (map (est !) [1 .. (snd (bounds est))]))
		x = map (\littleList -> map (\x -> valoare x) littleList) rev
		str = concatMap printList x 
		lengthStr = length str - 1
	
--primeste o lista de float-uri si returneaza un string de nr	
printList :: [Float] -> String
printList listL = take lengthX x ++ "\n"
	where 
		x = concatMap printElem listL
		lengthX = length x - 1
		
--printeaza un element de tip float cu 2 zecimale
printElem :: Float -> String
printElem x = printf "%+.2f " x

{-
    *** TODO ***

    Actualizează o estimare în urmare parcurgerii unei căi.

    Hint: `Data.Array.accum`.
-}

--actualizarea estimarii folosind functia accum pentru a retunra estiamrea sub forma (@,#)[($,%),...,($,%)]
updateEstimation :: Estimation -> Path -> Estimation
updateEstimation estimationNew pathNew = accum suma estimationNew (createEstimation pathNew estimationNew)

--primeste o cale si o estimare si creeaza o o lista de perechi State Float
--updateNew face algoritumul prezent in enunt (V[s] <- V[s] + @[R[s] + V[s'] - V[s]) aplicand functia de la taskul 3 (scaledLearningRates) 
--pairs face din liste 1-2-3-4-8 -> [(1-2),(2-3),(3-4),(4-8)]
createEstimation :: Path -> Estimation -> [(State, Float)]
createEstimation pathN estimN = map (\x -> ((fst x), updateNew x)) pairs
	where 
	updateNew x = (scaledLearningRates !! (1 + vizitari (estimN ! (fst x)))) * ((reinforcements ! (fst x)) + (valoare (estimN ! (snd x))) - (valoare (estimN ! (fst x))))
	pairs = zip (take ((length pathN) - 1) pathN) (tail pathN)

--face suma dintre tipul de date StateInfo si o valoare si returneaza un StateInfo
suma :: StateInfo -> Float -> StateInfo
suma s1 x = (StateInfo ((valoare s1) + x) ((vizitari s1) + 1))

{-
    *** TODO ***

    Obține un flux infinit de estimări rafinate succesiv, pe baza unui flux
    infinit de căi finite, încheiate în stări terminale.

    Hint: `Data.List.mapAccumL`.
-}
--functia din interior primeste o estimare si o lista ;ia din perechea determ. de mapAccumL lista(adica snd) 
estimations :: [Path] -> [Estimation]
estimations pathEstim = snd (mapAccumL (\estim list -> ((updateEstimation estim list), estim)) initialEstimation pathEstim)

{-
    *** TODO ***

    Determină estimarea de rang dat ca parametru, pe baza unui generator.
-}
--primeste un rang si un generator ; genereaza caile infinite pe baza generatorului si le truncheaza si ia estimarea data de fluxul de estimari
estimate :: RandomGen g => Int -> g -> Estimation
estimate rang gen = (estimations (map terminatePath (randomPaths gen))) !! rang  


{-
    *** TODO ***

    Pentru o stare, determină vecinul cu cea mai mare valoare estimată.

    Hint: `Data.Function.on`.
-}

--cu ajutorul lui pairs luam fiecare vecin al starii stateB din estimarea estimB si aplicam mamximum pe utilitati pentru a vedea maximul 
--dupa care returnam starea corespindenta
bestNeighborOf :: State -> Estimation -> State
bestNeighborOf stateB estimB = snd (maximum (map (\(x,y) -> (y,x)) pairs))
	where 
		pairs = map (\x -> (x, valoare (estimB ! x))) (neighborsOf stateB)

{-
    *** TODO ***

    Contruiește o cale începută în starea inițială, pe principiul alegerii 
    vecinului cu utilitata maximă.
-}
--intereaza prin vecinii cu utilitatea maxima si truncheaza caile pe care el termina
bestPath :: Estimation -> Path
bestPath estimP = terminatePath (iterate (\x -> bestNeighborOf x estimP) startState)


--  === 3. Estimarea utilităților cu diminuarea ratei de învățare ===

{-
    *** TODO ***

    Fluxul infinit al ratelor de învățare scalate:

    [ 1
    , learningRate
    , learningRate * scaleFactor
    , learningRate * scaleFactor^2
    , ...
    ]
-}
--reproduce formula de mai sus 
scaledLearningRates :: [Float]
scaledLearningRates = 1 : iterate (\x -> x * scaleFactor) learningRate

{-
    *** TODO ***

    Tip de date pentru reținerea atât a valorii estimate a unei stări,
    cât și a numărului de vizitări ale acesteia.
-}

--tipul de date StateInfo ce contine o valoare de tip float si un nr intreg de vizitari
data StateInfo = StateInfo
  { valoare :: Float
  , vizitari :: Int
  } deriving Show
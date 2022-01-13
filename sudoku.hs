import Data.List 

--declaratii
type Grid        = Matrice Val        --gridul = matrice de valori
type Matrice a   = [Linie a]          --matricea = lista de linii
type Linie a     = [a]                --linie = lista de intregi
type Val         = Char               --valorile = caractere


squaresize      :: Int
squaresize      =  3
values          :: [Val]
values          =  ['1'..'9']

--verificare celula goala
empty           :: Val -> Bool
empty           =  (== '-')

--verificare daca aparitia unui elem este 1 
single                :: [a] -> Bool
single [_]            =  True
single _              =  False

grid            :: Grid
grid            =  ["2----1-38",
                "--------5",
                "-7---6---",
                "-------13",
                "-981--257",
                "31----8--",
                "9--8---2-",
                "-5--69784",
                "4--25----"]

blank           :: Grid
blank           =  replicate n (replicate n '-')
                    where n = squaresize ^ 2

rows            :: Matrice a -> [Linie a]        --lista cu toate liniile din matrice
rows l          =  l
--rows(rows l) = l
--rows * rows = id

--transpusa unei matrici
transpunere      :: [[a]]->[[a]]
transpunere ([]:_) = []
transpunere x    = (map head x) : transpunere (map tail x)

cols             :: Matrice a -> [Linie a]         --lista cu toate coloanele din matrice
cols             =  transpunere
--cols * cols = id

---------------------------------------------------------------------------------------------------
squares                  :: Matrice a -> [Linie a]
squares                  =  unpack . map cols . pack
                         where
                            pack   = split . map split
                            split  = chop squaresize
                            unpack = map concat . concat
--squares * squares = id

chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)
------------------------------------------------------------------------------------------------------

--toate elemntele dintr-un sir sa indeplineasca o proprietate
--all :: (a -> Bool) -> [a] -> Bool

--functie care ia o lista si retureaza True/False
noduplicates       :: Eq a => [a] -> Bool
noduplicates []    =  True
noduplicates (x:xs)=  not (elem x xs) && noduplicates xs
--verificam daca gridul de Sudoku este valid (nu exista duplicate pe linii, coloane si in patratele)
valid              :: Grid -> Bool
valid g            =  all noduplicates (rows g) &&
                      all noduplicates (cols g) &&
                      all noduplicates (squares g)

--ia fiecare celula dintr-o linie si il inlocuieste cu un numar de la 1-9
type Choices       =  [Val]                      --lista de valori de la 1-9
choices            :: Grid -> Matrice Choices    
choices            =  map (map choice)           
                       where
                          choice v = if empty v then values else [v]

--toate combinatile dintr-o lista de mai multe liste
--cp [[1,2],[3,4],[5,6]] -> [[1,3,5],[1,3,6],[1,4,5],...]
cp                 :: [[a]] -> [[a]]
cp []              =  [[]]
cp (xs:xss)        =  [y:ys | y <- xs, ys <- cp xss]    --y=prima lista, ys=toate listele posibile

--prima celula este fixa, celelalte doua pot lua orice valori in intervalul 1-9
collapse           :: Matrice [a] -> [Matrice a]     --lista de matrici
collapse           =  cp . map cp                    --map cp pentru fiecare linie din matrice => map cp collapse fiecare linie, cp . map cp - collapse fiecare coloana

--elimina elementele care se gasesc o data
prune              :: Matrice Choices -> Matrice Choices
prune              =  pruneBy squares . pruneBy cols . pruneBy rows
                        where pruneBy f = f . map reduce . f                    --pruneBy - elimina elementul care se gaseste deja
                        
--reduce ["1234","1","34","3"] => ["24","1","4","3"]
--se uita doar la elementele care sunt singure
--elimina elementele single din celelate liste >decat un element
reduce             :: Linie Choices -> Linie Choices
reduce xss         =  [xs `minus` singles | xs <- xss]
                        where singles = concat (filter single xss)
--eliminate ys from xs
minus              :: Choices -> Choices -> Choices
xs `minus` ys      =  if single xs then xs else xs \\ ys

--iteram pana cand gasim doua solutii la fel
fix                :: Eq a => (a -> a) -> a -> a
fix f x            =  if x == x' then x else fix f x'
                       where x' = f x

solver             :: Grid -> [Grid]
solver             =  filter valid . collapse . fix prune . choices          --iteram pruning pana nu mai avem ceva taia, pana nu mai avem ce schimba


--Funcția „choices” înlocuiește pătratele goale din Grid cu toate posibilele valori pentru acel pătrat, oferind o matrice Choices 

--Funcția „pruning” se aplica pentru fiecare patrat, pentru a elimina toate valorile (intrarile single = lista de un element) care deja ocupa un loc in acel patratel, linie sau coloana

--Dupa aplciarea functiei „pruning”, pot exista noi intrari single, pentru care se va aplica din nou pruning. Astfel aplicand functia „fix”, putem itera
--procesul de „pruning” pana cand nu mai existe efecte viitoare.

sudoku                  :: IO ()
sudoku                  =  putStrLn (unlines (head (solver grid)))

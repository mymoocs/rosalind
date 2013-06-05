import qualified Data.Map as M
import Data.List
import System.IO

{--
RNA Transcribing DNA into RNA

--}

toRna :: String -> String

toRna [] = []
toRna ('T':xs) = 'U':toRna xs
toRna (x:xs) = x:toRna xs 

test_toRna = "GAUGGAACUUGACUACGUAAAUU" == toRna "GATGGAACTTGACTACGTAAATT"

{--
Complementing a Strand of DNA
--}
toRevc :: String -> String
toRevc = reverse . toRevc'

toRevc' [] = []
toRevc' ('A':xs) = 'T':toRevc' xs
toRevc' ('T':xs) = 'A':toRevc' xs
toRevc' ('C':xs) = 'G':toRevc' xs
toRevc' ('G':xs) = 'C':toRevc' xs
toRevc' (x:xs) = x:toRevc' xs


{--
Counting Point Mutations
--}

countHamm :: String -> String -> Int

countHamm xs ys = sum [1 | (x, y)<-zip xs ys, x /= y] 


--perm [x] = [x]
--perm (x:xs) = map (\ys->[x]++ys) (perm xs)

--Translating RNA into Proteinperm (x:xs) = map (\ys->[x]++ys) (perm xs)
--


rnaTable = [("UUU",'F'),("UUC",'F'),("UUA",'L'),("UUG",'L')
           ,("UCU",'S'),("UCC",'S'),("UCA",'S'),("UCG",'S')
           ,("UAU",'Y'),("UAC",'Y'),("UAA",'5'),("UAG",'5')
           ,("UGU",'C'),("UGC",'C'),("UGA",'5'),("UGG",'W')
           ,("CUU",'L'),("CUC",'L'),("CUA",'L'),("CUG",'L')
           ,("CCU",'P'),("CCC",'P'),("CCA",'P'),("CCG",'P')
           ,("CAU",'H'),("CAC",'H'),("CAA",'Q'),("CAG",'Q')
           ,("CGU",'R'),("CGC",'R'),("CGA",'R'),("CGG",'R')
           ,("AUU",'I'),("AUC",'I'),("AUA",'I'),("AUG",'M')
           ,("ACU",'T'),("ACC",'T'),("ACA",'T'),("ACG",'T')
           ,("AAU",'N'),("AAC",'N'),("AAA",'K'),("AAG",'K')
           ,("AGU",'S'),("AGC",'S'),("AGA",'R'),("AGG",'R')
           ,("GUU",'V'),("GUC",'V'),("GUA",'V'),("GUG",'V')
           ,("GCU",'A'),("GCC",'A'),("GCA",'A'),("GCG",'A')
           ,("GAU",'D'),("GAC",'D'),("GAA",'E'),("GAG",'E') 
           ,("GGU",'G'),("GGC",'G'),("GGA",'G'),("GGG",'G') 
           ]

rnaMap = M.fromList rnaTable           
rnaToProtein :: String -> String

rnaToProtein [] = []
rnaToProtein str = case (M.lookup a rnaMap) of
                    Just c -> if(c /= '5') then c : rnaToProtein xs
                                           else rnaToProtein xs  
                    Nothing -> error "coud not find " ++ a ++ "in lookup table "
                    where 
                    (a, xs) = splitAt 3 str 



printList [] = ""
printList (x:xs) = show x ++" " ++printList xs

--permuList n =  mapM_ putStrLn  $ map printList $ permutations [1 .. n]
permuList n =  map printList $ permutations [1 .. n]
signedPermuList n =  map printList $ permutations [(negate n) .. n]

--Enumerating Gene Orders
--PERM
main = do
    getN <- getLine
    putStr "Sign permutation? (y/n)"
    yesno <- getLine
    let number = read getN
       {- if yesno == 'y' 
            then  permType = signedPermuList
            else  permType = permuList
        -}
    handle <- openFile "result.txt" WriteMode
    mapM_ (hPutStrLn handle) $ permuList number
    hClose handle

--Monoisotopic mass table   

monoMassTable =[('A',   71.03711),
                ('C',   103.00919),
                ('D',   115.02694),
                ('E',   129.04259),
                ('F',   147.06841),
                ('G',   57.02146),
                ('H',   137.05891),
                ('I',   113.08406),
                ('K',   128.09496),
                ('L',   113.08406),
                ('M',   131.04049),
                ('N',   114.04293),
                ('P',   97.05276),
                ('Q',   128.05858),
                ('R',   156.10111),
                ('S',   87.03203),
                ('T',   101.04768),
                ('V',   99.06841),
                ('W',   186.07931),
                ('Y',   163.06333 )]

monoMassMap = M.fromList monoMassTable  

--Calculating Protein Mass


toProteinWaights [] = []
toProteinWaights (x:xs) = 
    case M.lookup x monoMassMap of
        Just w ->  w : toProteinWaights xs
        Nothing -> []--error "the key " ++ [x] ++ " cloud not find in mass table"                                


toProteinMass   = sum . toProteinWaights     



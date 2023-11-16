import Control.Monad (unless, when)
import Data.Char (digitToInt, isDigit)
import Data.List ()

{- Kommentar - Martin Sjåvik

Har ikke gjort siste oppgave om hint
z tar ikke et nummer (Går bare et steg tilbake)
z gir en feil hvis du har mer enn 9 plater
ellers tror jeg alt skal være bra
-}

main :: IO ()
main = do
  writeFile "moves.txt" "" -- Gjør filene tomme
  writeFile "save.txt" "" --
  putStrLn "Start a new game with b <nbOfRings>, or quit with <q>"
  line <- getLine

  if head line == 'q'
    then do
      putStrLn "Spillet er Ferdig"
    else
      if head line == 'b'
        then do
          move <- moves
          let t1 = tårn (read (last (words line)) :: Int)
          let t2 = tomtTårn (read (last (words line)) :: Int)
          let t3 = tomtTårn (read (last (words line)) :: Int)
          appendSave (show t1) (show t2) (show t3)
          treTårn (visueltTårn t1 (length t1)) (visueltTårn t2 (length t2)) (visueltTårn t3 (length t3))
          putStrLn ("number of moves: " ++ show move)
          line2 <- getLine

          if head line2 == 'q'
            then do
              putStrLn "Spillet er Ferdig"
            else
              if head line2 == '1' && last line2 == '2'
                then do
                  if all (== 0) t1
                    then do
                      putStrLn "Kan ikke flytte fra et tomt tårn"
                      play t1 t2 t3
                    else do
                      let new = putInt t1 t2
                      let new2 = replace (snd (putInt t1 t2)) t1
                      addMove
                      appendSave (show new2) (show (fst new)) (show t3)
                      play new2 (fst new) t3
                else
                  if head line2 == '1' && last line2 == '3'
                    then do
                      if all (== 0) t1
                        then do
                          putStrLn "Kan ikke flytte fra et tomt tårn"
                          play t1 t2 t3
                        else do
                          let new = putInt t1 t3
                          let new2 = replace (snd (putInt t1 t3)) t1
                          addMove
                          appendSave (show new2) (show t2) (show (fst new))
                          play new2 t2 (fst new)
                    else
                      if head line2 == '2' && last line2 == '1'
                        then do
                          if all (== 0) t2
                            then do
                              putStrLn "Kan ikke flytte fra et tomt tårn"
                              play t1 t2 t3
                            else do
                              let new = putInt t2 t1
                              let new2 = replace (snd (putInt t2 t1)) t2
                              addMove
                              appendSave (show (fst new)) (show new2) (show t3)
                              play (fst new) new2 t3
                        else
                          if head line2 == '2' && last line2 == '3'
                            then do
                              if all (== 0) t2
                                then do
                                  putStrLn "Kan ikke flytte fra et tomt tårn"
                                  play t1 t2 t3
                                else do
                                  let new = putInt t2 t3
                                  let new2 = replace (snd (putInt t2 t3)) t2
                                  addMove
                                  appendSave (show t1) (show new2) (show (fst new))
                                  play t1 new2 (fst new)
                            else
                              if head line2 == '3' && last line2 == '1'
                                then do
                                  if all (== 0) t3
                                    then do
                                      putStrLn "Kan ikke flytte fra et tomt tårn"
                                      play t1 t2 t3
                                    else do
                                      let new = putInt t3 t1
                                      let new2 = replace (snd (putInt t3 t1)) t3
                                      addMove
                                      appendSave (show (fst new)) (show t2) (show new2)
                                      play (fst new) t2 new2
                                else
                                  if head line2 == '3' && last line2 == '2'
                                    then do
                                      if all (== 0) t3
                                        then do
                                          putStrLn "Kan ikke flytte fra et tomt tårn"
                                          play t1 t2 t3
                                        else do
                                          let new = putInt t3 t2
                                          let new2 = replace (snd (putInt t3 t2)) t3
                                          addMove
                                          appendSave (show t1) (show (fst new)) (show new2)
                                          play t1 (fst new) new2
                                    else do
                                      putStrLn "Feil Kommando! Oppgi: <Plate1, Plate2> eller <z> eller <b> eller <q>"
                                      play t1 t2 t3
        else do
          putStrLn "Feil Kommando! Oppgi: <b (nummer)> for å starte spill eller <q> for å avslutte"
          main

-- Spillet
play :: [Int] -> [Int] -> [Int] -> IO ()
play a b c = do
  treTårn (visueltTårn a (length a)) (visueltTårn b (length b)) (visueltTårn c (length c))
  move <- moves
  if all (== 0) a && all (== 0) b
    then do
      putStrLn ("GRATULERER DU VANT!! Du løste " ++ show (length a - 1) ++ " plater med " ++ show move ++ " trekk")
      main
    else do
      putStrLn ("number of moves: " ++ show move)
      line2 <- getLine

      if head line2 == 'q'
        then do
          putStrLn "Spillet er Ferdig"
        else
          if head line2 == 'o'
            then do
              print a
              print b
              print c
            else
              if head line2 == 'b'
                then do
                  main
                else
                  if head line2 == 'z'
                    then do
                      let len = length a
                      delMove (len - 1)
                      del1
                      save <- lastMove (len - 1)
                      let tårn = chunks len (change save)
                      play (tårn !! 0) (tårn !! 1) (tårn !! 2)
                    else
                      if head line2 == '1' && last line2 == '2'
                        then do
                          if fstNum a > fstNum b && fstNum b /= 0
                            then do
                              putStrLn "For stor plate! Prøv på nytt"
                              play a b c
                            else
                              if all (== 0) a
                                then do
                                  putStrLn "Kan ikke flytte fra et tomt tårn"
                                  play a b c
                                else do
                                  let new = putInt a b
                                  let new2 = replace (snd (putInt a b)) a
                                  addMove
                                  appendSave (show new2) (show (fst new)) (show c)
                                  play new2 (fst new) c
                        else
                          if head line2 == '1' && last line2 == '3'
                            then do
                              if fstNum a > fstNum c && fstNum c /= 0
                                then do
                                  putStrLn "For stor plate! Prøv på nytt"
                                  play a b c
                                else
                                  if all (== 0) a
                                    then do
                                      putStrLn "Kan ikke flytte fra et tomt tårn"
                                      play a b c
                                    else do
                                      let new = putInt a c
                                      let new2 = replace (snd (putInt a c)) a
                                      addMove
                                      appendSave (show new2) (show b) (show (fst new))
                                      play new2 b (fst new)
                            else
                              if head line2 == '2' && last line2 == '1'
                                then do
                                  if fstNum b > fstNum a && fstNum a /= 0
                                    then do
                                      putStrLn "For stor plate! Prøv på nytt"
                                      play a b c
                                    else
                                      if all (== 0) b
                                        then do
                                          putStrLn "Kan ikke flytte fra et tomt tårn"
                                          play a b c
                                        else do
                                          let new = putInt b a
                                          let new2 = replace (snd (putInt b a)) b
                                          addMove
                                          appendSave (show (fst new)) (show new2) (show c)
                                          play (fst new) new2 c
                                else
                                  if head line2 == '2' && last line2 == '3'
                                    then do
                                      if fstNum b > fstNum c && fstNum c /= 0
                                        then do
                                          putStrLn "For stor plate! Prøv på nytt"
                                          play a b c
                                        else
                                          if all (== 0) b
                                            then do
                                              putStrLn "Kan ikke flytte fra et tomt tårn"
                                              play a b c
                                            else do
                                              let new = putInt b c
                                              let new2 = replace (snd (putInt b c)) b
                                              addMove
                                              appendSave (show a) (show new2) (show (fst new))
                                              play a new2 (fst new)
                                    else
                                      if head line2 == '3' && last line2 == '1'
                                        then do
                                          if fstNum c > fstNum a && fstNum a /= 0
                                            then do
                                              putStrLn "For stor plate! Prøv på nytt"
                                              play a b c
                                            else
                                              if all (== 0) c
                                                then do
                                                  putStrLn "Kan ikke flytte fra et tomt tårn"
                                                  play a b c
                                                else do
                                                  let new = putInt c a
                                                  let new2 = replace (snd (putInt c a)) c
                                                  addMove
                                                  appendSave (show (fst new)) (show b) (show new2)
                                                  play (fst new) b new2
                                        else
                                          if head line2 == '3' && last line2 == '2'
                                            then do
                                              if fstNum c > fstNum b && fstNum b /= 0
                                                then do
                                                  putStrLn "For stor plate! Prøv på nytt"
                                                  play a b c
                                                else
                                                  if all (== 0) c
                                                    then do
                                                      putStrLn "Kan ikke flytte fra et tomt tårn"
                                                      play a b c
                                                    else do
                                                      let new = putInt c b
                                                      let new2 = replace (snd (putInt c b)) c
                                                      addMove
                                                      appendSave (show a) (show (fst new)) (show new2)
                                                      play a (fst new) new2
                                            else do
                                              putStrLn "Feil Kommando! Oppgi: <Plate1, Plate2> eller <z> eller <b> eller <q>"
                                              play a b c

-- Lager visuell plate fra en int
plate :: Int -> Int -> String
plate 0 0 = ""
plate x y =
  let a = concat (replicate x "# ")
      m = spc y
   in m ++ a ++ m

-- lager "tom" plate (0)
tomPlate :: Int -> String
tomPlate 0 = ""
tomPlate x =
  let m = replicate (x - 1) " "
   in concat m ++ "| " ++ concat m

newline :: IO ()
newline = putChar '\n'

-- Tårn representert som liste av ints e.g [0,1,2,3,4]
tårn :: Int -> [Int]
tårn x = [0 .. x]

-- tomt tårn
tomtTårn :: (Num a) => Int -> [a]
tomtTårn x = replicate (x + 1) 0

-- Gjør liste av Ints til liste av strings klar til å printes. med riktig spaces
visueltTårn :: [Int] -> Int -> [String]
visueltTårn [] _ = []
visueltTårn (x : xs) akk =
  let space = spc (akk - x)
   in if x > 0
        then (space ++ plate x 0 ++ space) : visueltTårn xs akk
        else tomPlate akk : visueltTårn xs akk

-- 3 Tårn ved siden av hverandre
treTårn :: [String] -> [String] -> [String] -> IO ()
treTårn [] [] [] = do return ()
treTårn (x : xs) (y : ys) (z : zs) = do
  putStrLn (x ++ y ++ z)
  treTårn xs ys zs

-- mellomrom
spc :: Int -> String
spc n = concat (replicate n " ")

-- gir første nummer som ikke er 0 i en liste
fstNum :: [Int] -> Int
fstNum [] = 0
fstNum (x : xs) = if x == 0 then fstNum xs else x

-- Hvor mange av en type int i en liste
howMany :: (Num a) => (t -> Bool) -> [t] -> a
howMany p xs = sum [1 | x <- xs, p x]

-- Setter en ny int i en gitt posisjon i en liste
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement : as
insertAt newElement i (a : as) = a : insertAt newElement (i - 1) as

-- Putter første int fra liste1 i rikitg posisjon i liste2. Returner tuppel med oppdatert liste og nummeret som ble puttet inn
putInt :: [Int] -> [Int] -> ([Int], Int)
putInt xs [] = ([], 0)
putInt xs ys = (tail $ insertAt (fstNum xs) (howMany (== 0) ys) ys, fstNum xs)

-- Bytter første tall z med 0 i en liste
replace :: (Eq b, Num b) => b -> [b] -> [b]
replace z = map (\x -> if x == z then 0 else x)

-- legger +1 til moves filen
addMove :: IO ()
addMove = appendFile "moves.txt" "1"

-- returnerer hvor mange moves brukeren har brukt
moves :: IO Int
moves = do
  a <- readFile "moves.txt"
  let b = length a
  return b

-- Leser hele save filen
leseFil :: IO String
leseFil = readFile "save.txt"

-- Finner siste move
lastMove :: Int -> IO [Char]
lastMove x = do
  innhold <- leseFil
  let rev = drop (length innhold - ((x * 2 + 3) * 3)) innhold
  return rev

-- Gjør en String fra filen til et gyldig tårn
change :: String -> [Int]
change [] = []
change (x : xs) = if isDigit x then digitToInt x : change xs else change xs

-- Sletter siste Move fra Save filen
delMove :: Int -> IO ()
delMove x = do
  innhold <- readFile "save.txt"
  let rev = take (length innhold - ((x * 2 + 3) * 3)) innhold
  unless (null rev) $
    writeFile "save.txt" rev

-- Sletter 1 fra moves når man bruker går tilbake et steg
del1 :: IO ()
del1 = do
  innhold <- readFile "moves.txt"
  if length innhold > 1
    then do
      let rev = take (length innhold - 1) innhold
      unless (null rev) $
        writeFile "moves.txt" rev
    else do writeFile "moves.txt" ""

-- Lagrer spillet i "Save"
appendSave :: String -> String -> String -> IO ()
appendSave a b c = do
  appendFile "save.txt" a
  appendFile "save.txt" b
  appendFile "save.txt" c

-- Tar en liste med Ints og lager 3 lister som blir brukt til spillet. Brukes for å hente gamle trekk
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l
  | n > 0 = take n l : chunks n (drop n l)
  | otherwise = error "Negative or zero n"

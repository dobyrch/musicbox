module MusicBox where

import Control.Concurrent (threadDelay)
import Data.List
import Data.Maybe

clefs = [('&', Pitch G 4),
         ('}', Pitch C 4),
         ('@', Pitch F 3)]

main = getContents >>= play . readSheet

play :: [Double] -> IO ()
play [] = return ()
play (freq : rest) = do
    let freqStr = show $ round freq
    -- Escape sequences set frequency/duration of bell
    -- (See `man console_codes`)
    putStr $ "\ESC[10;" ++ freqStr ++ "]\ESC[11;400]\BEL"
    putStrLn freqStr
    threadDelay 500000
    play rest

readSheet :: String -> [Double]
readSheet = readColumn Nothing . transpose . reverse . lines

readColumn :: Maybe Clef -> [String] -> [Double]
readColumn clef [] = []
readColumn clef (column : rest)
    | Just newClef <- findClef column
    = readColumn (Just newClef) rest
    | Just (Clef clefPitch clefLine) <- clef,
      Just noteLine <- findIndex isNote column
    = let offset = noteLine - clefLine
          freq = frequency $ clefPitch `transposeLines` offset
      in freq : readColumn clef rest
    | otherwise = readColumn clef rest

findClef :: [Char] -> Maybe Clef
findClef column = listToMaybe [Clef p l | p <- ps, l <- ls]
    where ps = mapMaybe (`lookup` clefs) column
          ls = findIndices isClef column

isClef :: Char -> Bool
isClef = (`elem` map fst clefs)

isNote :: Char -> Bool
isNote = (`elem` "dp")

data Note = C | Csharp |
            D | Dsharp |
            E |
            F | Fsharp |
            G | Gsharp |
            A | Asharp |
            B deriving (Bounded, Enum, Eq, Ord)

type Octave = Int
data Pitch = Pitch Note Octave deriving Eq
type Line = Int
data Clef = Clef Pitch Line

instance Ord Pitch where
    compare (Pitch n1 o1) (Pitch n2 o2)
        | o1 /= o2 = compare o1 o2
        | n1 /= n2 = compare n1 n2
        | otherwise = EQ

scale :: Pitch -> Bool -> [Pitch]
scale pitch@(Pitch note octave) ascending =
    dropWhile (/= pitch) [Pitch n o | o <- os, n <- ns]
    where ns = choose [C ..] [B, Asharp ..]
          os = choose [octave ..] [octave, octave-1 ..]
          choose t f = if ascending then t else f

interval :: Pitch -> Pitch -> Int
interval p1 p2 = if ascending then semitones else -semitones
    where ascending = p1 < p2
          semitones = length $ takeWhile (/= p2) (scale p1 ascending)

transposeSemitones :: Pitch -> Int -> Pitch
transposeSemitones pitch semitones = s !! (abs semitones)
    where s = scale pitch (semitones > 0)

transposeLines :: Pitch -> Int -> Pitch
transposeLines pitch lines = s !! (abs lines)
    where s = filter natural $ scale pitch (lines > 0)

natural :: Pitch -> Bool
natural (Pitch note octave) = note `elem` [A, B, C, D, E, F, G]

frequency :: Pitch -> Double
frequency pitch = 440 * freqRatio^^(interval refPitch pitch)
    where freqRatio = 2 ** (1/12)
          refPitch = Pitch A 4

module MusicBox where

import Control.Concurrent (threadDelay)
import Data.List
import Data.Maybe
import Data.Monoid

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

data Pitch = Pitch {note::Note, octave::Int} deriving Eq
data Clef = Clef {pitch::Pitch, line::Int}

instance Ord Pitch where
    compare (Pitch n1 o1) (Pitch n2 o2) =
        mappend (compare o1 o2) (compare n1 n2)

instance Enum Pitch where
    toEnum i =
        Pitch (toEnum $ i `mod` noteCount) (i `div` noteCount)
    fromEnum (Pitch note octave) =
        (fromEnum note) + octave * noteCount

noteCount = length [C ..]

transposeSemitones :: Pitch -> Int -> Pitch
transposeSemitones pitch semitones =
    toEnum $ (fromEnum pitch) + semitones

transposeLines :: Pitch -> Int -> Pitch
transposeLines pitch lines = staff !! (abs lines)
    where staff = filter natural $ scale pitch
          scale = iterate (if ascending then succ else pred)
          ascending = lines > 0

natural :: Pitch -> Bool
natural (Pitch note _) = note `elem` [A, B, C, D, E, F, G]

frequency :: Pitch -> Double
frequency pitch = freqRatio^^(interval pitch refPitch) * refFreq
    where freqRatio = 2 ** (1/12)
          interval p1 p2 = (fromEnum p1) - (fromEnum p2)
          refPitch = Pitch A 4
          refFreq = 440

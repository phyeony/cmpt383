import RainbowAssign
import qualified Data.Map as Map

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table


-- convert a hash value back to a possible password
pwReduce :: Hash -> Passwd
pwReduce n = 
    map toLetter $ convert2BaseN (fromEnum n) nLetters pwLength

convert2BaseN :: Int -> Int -> Int -> [Int]
convert2BaseN num baseN length =
    reverse $ take length $ compute2BaseNInfinite num baseN
    where
        compute2BaseNInfinite :: Int -> Int -> [Int]
        compute2BaseNInfinite num baseN = (num `mod` baseN) : compute2BaseNInfinite (num `div` baseN) baseN

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable width passwds =
    Map.fromList $ zip (map (\passwd -> computeHashValue width passwd) passwds) passwds
    where 
        computeHashValue :: Int -> Passwd -> Hash
        computeHashValue 0 passwd = 
            pwHash passwd
        computeHashValue width passwd = 
            computeHashValue (width-1) (pwReduce $ pwHash $ passwd)

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

--test1 = do
  --table <- readTable filename
  --return (Map.lookup 0 table)

-- findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
-- findPassword rainbowTable width hash = 
    
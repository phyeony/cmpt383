import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)

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
convert2BaseN num baseN len =
    reverse $ take len $ compute2BaseNInfinite num baseN
    where
        compute2BaseNInfinite :: Int -> Int -> [Int]
        compute2BaseNInfinite num' baseN' = (num' `mod` baseN') : compute2BaseNInfinite (num' `div` baseN') baseN'

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable tableWidth passwds =
    Map.fromList $ zip (map (\passwd -> computeHashValue tableWidth passwd) passwds) passwds
    where 
        computeHashValue :: Int -> Passwd -> Hash
        computeHashValue 0 passwd = 
            pwHash passwd
        computeHashValue tableWidth' passwd = 
            computeHashValue (tableWidth'-1) (pwReduce $ pwHash $ passwd)

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table tableWidth hash = 
    listToMaybe $ catMaybes $ Map.elems result
    where
        result = Map.map (\passwd -> Map.lookup hash (newHashPassPairMap passwd)) (candidateMap table tableWidth hash)
        newHashPassPairMap passwd = Map.fromList $ computeHashPassPairs tableWidth passwd []

computeHashPassPairs :: Int -> Passwd -> [(Hash, Passwd)] -> [(Hash, Passwd)]
computeHashPassPairs 0 passwd accPair = (pwHash passwd, passwd):accPair
computeHashPassPairs tableWidth passwd accPair =
    computeHashPassPairs (tableWidth-1) (pwReduce $ hashed) ((hashed, passwd) : accPair) 
    where 
        hashed = pwHash $ passwd

candidateMap :: Map.Map Hash Passwd -> Int -> Hash -> Map.Map Hash Passwd 
candidateMap table tableWidth hash = Map.filterWithKey (\k _ -> elem k reversedHashValues) table
    where
        reversedHashValues = computeReverseHashedValues tableWidth hash []
        computeReverseHashedValues :: Int -> Hash -> [Hash] -> [Hash]
        computeReverseHashedValues 0 prevHash accHashes = prevHash:accHashes
        computeReverseHashedValues tableWidth' prevHash accHashes = 
            computeReverseHashedValues (tableWidth'-1) (pwHash $ pwReduce $ prevHash) (prevHash : accHashes)


-- test1 = do
--   table <- readTable filename
--   return (Map.lookup 1136298627 table)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = mapMaybe (findPassword table width) hs
  return (result, length result)

main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res
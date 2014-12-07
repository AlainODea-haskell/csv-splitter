import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment (getArgs)
import Data.Ratio
import Control.Monad (forM_)
import Safe (tailNote,headNote)

main :: IO ()
main = do
  args <- getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
  csvData <- TL.readFile csvFile
  split fractions csvData csvFile
  where csvFile = headNote "USAGE: split-csv <csvfile> [fraction...]" args
        fractions = tailNote "ERROR: no csvfile was specified" args

split :: [String] -> TL.Text -> String -> IO ()
split fractions csvData csvFile =
    forM_ (segmentedRowsAndIndices fractions csvLines) (\(rowsSegment,index) ->
                                 if not $ null rowsSegment then
                                   do
                                    let csvSegmentFile = csvFile ++ show index ++ ".csv"
                                    putStrLn $ csvSegmentFile ++ ": " ++
                                      (show (length rowsSegment))
                                    TL.writeFile csvSegmentFile
                                      (TL.unlines (header:rowsSegment))
                                 else
                                   return ()
                                )
      where header = headNote "ERROR: csv file was empty" csvLines
            csvLines = TL.lines csvData

segmentedRowsAndIndices :: [String] -> [TL.Text] -> [([TL.Text],Int)]
segmentedRowsAndIndices fractions csvLines = zip segmentedRows [0..]
  where segmentedRows = map (\x ->
                                  take (fromIntegral (snd x)) $
                                  drop (fromIntegral (fst x)) rows)
                            segmentIndexLengthPairs
        rows = tail csvLines
        segmentIndexLengthPairs = zip segmentIndices segmentLengths
        segmentIndices = scanl1 (+) $ 0:segmentLengths
        segmentLengths = map ((\x -> numerator x `div` denominator x) .
                            (\x -> x * toRational count)
                           ) segmentFractions
        count = length rows
        segmentFractions = map (readRational . map
                                (\x -> case x of
                                            '/' -> '%'
                                            _ -> x)
                               ) fractions
        readRational x = read x :: Rational

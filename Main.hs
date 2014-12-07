import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment (getArgs)
import Data.Ratio
import Control.Monad (forM_)

main :: IO ()
main = do
  args <- getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
  csvData <- TL.readFile csvFile
  split args csvData
  where csvFile = head args

split :: [String] -> TL.Text -> IO ()
split args csvData =
    forM_ (segmentedRowsAndIndices args csvLines) (\(rowsSegment,index) ->
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
      where csvFile = head args
            header = head csvLines
            csvLines = TL.lines csvData

segmentedRowsAndIndices :: [String] -> [TL.Text] -> [([TL.Text],Int)]
segmentedRowsAndIndices args csvLines = zip segmentedRows [0..]
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
                               ) $ tail args
        readRational x = read x :: Rational

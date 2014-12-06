import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment (getArgs)
import Data.Ratio
import Control.Monad (forM_)

main :: IO ()
main = do
    args <- getArgs
    let csvFile = head args
    csvData <- TL.readFile csvFile
    let readRational x = read x :: Rational
    let segmentFractions = map (readRational . map
                                (\x -> case x of
                                            '/' -> '%'
                                            _ -> x)
                               ) $ tail args
    let csvLines = TL.lines csvData
    let header = head csvLines
    let rows = tail csvLines
    let count = length rows
    let segmentLengths = map ((\x -> numerator x `div` denominator x) .
                            (\x -> x * toRational count)
                           ) segmentFractions
    let segmentIndices = scanl1 (+) $ 0:segmentLengths
    let segmentIndexLengthPairs = zip segmentIndices segmentLengths
    putStrLn $ show segmentIndexLengthPairs
    let segmentedRows = map (\x ->
                                  take (fromIntegral (snd x)) $
                                  drop (fromIntegral (fst x)) rows)
                            segmentIndexLengthPairs
    let segmentedRowsAndIndices = zip segmentedRows [0..]
    forM_ segmentedRowsAndIndices (\(rowsSegment,index) ->
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

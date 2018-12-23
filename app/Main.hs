{-# LAnguage OverloadedStrings #-}

module Main where

import qualified Data.Algorithm.Diff     as Diff
import qualified Data.Diff.Flexible      as ADiff
import qualified Data.Diff.Flexible.Text as TDiff

import qualified Data.Vector  as V
import           Data.Vector  ((!))
import qualified Debug.Trace  as D
import qualified Data.List    as L
import qualified Data.Text    as TXT
import qualified Data.Text.IO as TXT
import           Data.Foldable (for_)

import qualified System.Console.ANSI as ANSI
import qualified System.Environment  as Env


main :: IO ()
main = do
--    print diffs
--{-
    args <- Env.getArgs
    case args of
        b:a:_ -> do
            txtb <- TXT.readFile b
            txta <- TXT.readFile a
            let diff = TDiff.getTextDiff txtb txta
                settings = TDiff.defaultColorSettings
            for_ (TDiff.diffToColoredText settings diff) TXT.putStrLn
        _ -> error "args insufficient"
---}
  where
--    diffs = ADiff.getDiff ("aaaaaaaXYZaa" :: TXT.Text) ("aaaaaaA123456aa" :: TXT.Text)
--    diffs = ADiff.getDiff ("aaaaaaA123456aa" :: TXT.Text) ("aaaaaaaXYZaa" :: TXT.Text)
    -- print test
    -- print testL
    -- print testT
    -- print diff1
    -- print diff2
    -- print diff3
    -- print diff4
{-
    TXT.putStrLn $ "aaaa" <> setColor1 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor2 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor3 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor4 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor5 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor6 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor7 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor8 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor9 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor10 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor11 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor12 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor13 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor14 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor15 <> "bbbb" <> resetColor <> "cccc"
    TXT.putStrLn $ "aaaa" <> setColor16 <> "bbbb" <> resetColor <> "cccc"
-}
{-
  where
    -- setColor1 = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red, ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Blue]
    setColor1 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    setColor2 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
    setColor3 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
    setColor4 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
    setColor5 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
    setColor6 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
    setColor7 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
    setColor8 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
    setColor9 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    setColor10 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue]
    setColor11 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta]
    setColor12 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta]
    setColor13 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
    setColor14 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
    setColor15 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
    setColor16 = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    resetColor = TXT.pack $ ANSI.setSGRCode []
    xxx cost reach k diffs = ADiff.Status diffs cost reach k
    (a1, b1) = dataSet1
    (a2, b2) = dataSetTxt
    diff1 = Diff.getDiff a1 b1
    diff2 = ADiff.getDiff a1 b1
    diff3 = ADiff.getDiffByDiff a1 b1
    diff4 = TDiff.getTextDiff a2 b2
-}{-
    countDiffLine = concat . map
      
    diffRatio as bs = Diff.getDiff as bs
      where
        lenA = length as
        lenB = length bs
    flip map dataA $ \ lineA -> flip map dataB $ \ lineB ->
      diffRatio lineA lineB
---}

{-
test :: V.Vector Int
test = D.trace "vector" v
  where
    v = V.unfoldr (f v) 0
    f :: V.Vector Int -> Int -> Maybe (Int, Int)
    f v_ k | k == 10   = D.traceShow k $ Nothing
           | k == 9    = D.traceShow k $ Just (V.length v_, k + 1)
           | k > 0     = D.traceShow k $ Just ((D.traceShowId $ V.unsafeIndex v_ $ k + 1) + 1, k + 1)
           | otherwise = D.traceShow k $ Just (D.traceShowId (V.length v_), k + 1)

testL :: [Int]
testL = list
  where
    list = L.unfoldr f 1
    f k | k > 10    = D.traceShow k $ D.traceShowId $ Nothing
        | k > 1     = D.traceShow k $ D.traceShowId $ Just (1 + (head $ drop (k-2) list), k + 1)
        | otherwise = D.traceShow k $ D.traceShowId $ Just (1, k + 1)

testT :: (Int, Int)
testT = tuple
  where
    tuple = (1, fst tuple + 2)

dataSet1 :: ([String], [String])
dataSet1 = (dataA, dataB)
  where
    dataA = [ "aaaaaaaaa"
            , "bbbbbbbbb"
            , "ccccccccc"
            , "ddddddddd"
            , "xxxxxxxxx"
            , "eeeeeeeee"
            , "aaaaaaAaa"
            , "bbbbbbBbb"
            , "fffffffff"
            ]
    dataB = [ "aaaaaaAaa"
            , "bbbbbbBbb"
            , "ccccccCcc"
            , "ddddddDdd"
            , "eeeeeeEee"
            , "zzzzzzzzz"
            ]

dataSetTxt :: (TXT.Text, TXT.Text)
dataSetTxt = (TXT.intercalate "\n" dataA, TXT.intercalate "\n" dataB)
  where
    dataA = [ "aaaaaaaaa"
            , "---------"
            , "bbbbbbbbb"
            , "ccccccccc"
            , "ddddddddd"
            , "xxxxxxxxx"
            , "eeeeeeeee"
            , "aaaaaaAaa"
            , "bbbbbbBbb"
            , "fffffffff"
            ]
    dataB = [ "aaaaaaAaa"
            , "---------"
            , "bbbbbbBbb"
            , "ccccccCcc"
            , "ddddddDdd"
            , "eeeeeeEee"
            , "zzzzzzzzz"
            ]
---}

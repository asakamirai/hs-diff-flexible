{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Data.Diff.Flexible.Text where

import qualified Data.Diff.Flexible.Internal as I

import qualified Data.List   as L
import qualified Data.Text   as TXT
import qualified Data.Vector as V
import qualified System.Console.ANSI as ANSI

import           GHC.Exts (IsList(..))

-- import qualified Debug.Trace as D

type TextDiff = I.RawDiff [I.RawDiff_ Char] TXT.Text

getTextDiff :: TXT.Text -> TXT.Text -> [TextDiff]
getTextDiff beforeTxt afterTxt = I.getVectorDiffBy f beforev afterv
  where
    beforev = toVector $ TXT.lines beforeTxt
    afterv  = toVector $ TXT.lines afterTxt
    toVector :: (IsList l) => l -> V.Vector (Item l)
    toVector = V.fromList . toList
    f :: TXT.Text -> TXT.Text -> I.CompareResult [I.RawDiff_ Char]
    f mtext ntext | mtext == ntext            = I.ItemEqual
                  | cost < I.doubleCost * 0.7 = I.ItemModify cost diffs
                  | otherwise                 = I.ItemDeleteAdd
      where
        diffs = I.getDiff mtext ntext
        cost  = I.doubleCost * ses / len
        len   = toEnum $ TXT.length mtext + TXT.length ntext
        ses   = sum $ fmap I.toCost diffs

data ColorSettings = ColorSettings
    { eqlColor       :: TXT.Text -> TXT.Text
    , delColor       :: TXT.Text -> TXT.Text
    , addColor       :: TXT.Text -> TXT.Text
    , modDelEqlColor :: TXT.Text -> TXT.Text
    , modDelModColor :: TXT.Text -> TXT.Text
    , modAddEqlColor :: TXT.Text -> TXT.Text
    , modAddModColor :: TXT.Text -> TXT.Text
    }

defaultColorSettings :: ColorSettings
defaultColorSettings = ColorSettings
    { eqlColor       = setColor eql
    , delColor       = setColor del
    , addColor       = setColor add
    , modDelEqlColor = setColor modDelEql
    , modDelModColor = setColor modDelMod
    , modAddEqlColor = setColor modAddEql
    , modAddModColor = setColor modAddMod
    }
  where
    reset = TXT.pack $ ANSI.setSGRCode []
    eql = reset
    del = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
    add = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
    modDelEql = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
    modDelMod = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Yellow]
    modAddEql = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
    modAddMod = TXT.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Cyan]
    setColor :: TXT.Text -> TXT.Text -> TXT.Text
    setColor c txt = c <> txt <> reset

diffToColoredLine :: ColorSettings -> TextDiff -> [TXT.Text]
diffToColoredLine s d = case d of
    I.Delete txt       -> [delColor s $ "-" <> txt]
    I.Add    txt       -> [addColor s $ "+" <> txt]
    I.Equal  txt _     -> [eqlColor s $ " " <> txt]
    I.Modify _ _ _ aux -> [fb aux, fa aux]
    _                  -> error "unreachable"
  where
    fb :: [I.RawDiff_ Char] -> TXT.Text
    fb = foldr foldBTxtF "" . fmap (fmap TXT.pack) . (I.Equal "-" "+" :) . foldr foldBFunc []
    fa :: [I.RawDiff_ Char] -> TXT.Text
    fa = foldr foldATxtF "" . fmap (fmap TXT.pack) . (I.Equal "-" "+" :) . foldr foldAFunc []
    foldBTxtF :: I.RawDiff_ TXT.Text -> TXT.Text -> TXT.Text
    foldBTxtF (I.Equal txt _) prev = modDelEqlColor s txt <> prev
    foldBTxtF (I.Delete txt)  prev = modDelModColor s txt <> prev
    foldBTxtF _               _    = error "unreachable"
    foldATxtF :: I.RawDiff_ TXT.Text -> TXT.Text -> TXT.Text
    foldATxtF (I.Equal _ txt) prev = modAddEqlColor s txt <> prev
    foldATxtF (I.Add txt)     prev = modAddModColor s txt <> prev
    foldATxtF _               _    = error "unreachable"

foldBFunc ::
    I.RawDiff_ Char -> [I.RawDiff_ String] -> [I.RawDiff_ String]
foldBFunc diff diffs = case (diff, diffs) of
    (I.Add    _,        prev)               -> prev
    (I.Delete cb,       I.Delete sb:prev)   -> I.Delete (cb:sb):prev
    (I.Delete cb,       prev)               -> I.Delete [cb]:prev
    (I.Equal  cb ca,    I.Equal sb sa:prev) -> I.Equal (cb:sb) (ca:sa):prev
    (I.Equal  cb ca,    prev)               -> I.Equal [cb] [ca]:prev
    (I.Modify cb _ _ _, I.Delete sb:prev)   -> I.Delete (cb:sb):prev
    (I.Modify cb _ _ _, prev)               -> I.Delete [cb]:prev
    (_,                 _)                  -> error "unreachable"

foldAFunc :: I.RawDiff_ Char -> [I.RawDiff_ String] -> [I.RawDiff_ String]
foldAFunc diff diffs = case (diff, diffs) of
    (I.Delete _,        prev)               -> prev
    (I.Add    ca,       I.Add sa:prev)      -> I.Add (ca:sa):prev
    (I.Add    ca,       prev)               -> I.Add [ca]:prev
    (I.Equal  cb ca,    I.Equal sb sa:prev) -> I.Equal (cb:sb) (ca:sa):prev
    (I.Equal  cb ca,    prev)               -> I.Equal [cb] [ca]:prev
    (I.Modify _ ca _ _, I.Add sa:prev)      -> I.Add (ca:sa):prev
    (I.Modify _ ca _ _, prev)               -> I.Add [ca]:prev
    (_,                 _)                  -> error "unreachable"

diffToColoredText :: ColorSettings -> [TextDiff] -> [TXT.Text]
diffToColoredText s = L.concat . fmap (diffToColoredLine s)


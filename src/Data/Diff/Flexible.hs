{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Data.Diff.Flexible
  ( I.Cost
  , I.ToCost(..)
  , I.doubleCost
  , I.singleCost
  , I.noCost
  ) where

import qualified Data.Diff.Flexible.Internal as I

{-
import qualified Data.Vector as V
import           Data.Vector ((!), (!?))
import qualified Data.List   as L
import qualified Data.Maybe  as MB
import           Data.Coerce (coerce)

import           GHC.Exts (IsList(..))

import qualified Debug.Trace as D


------

newtype MPos  = MPos  Int deriving (Show, Eq, Ord, Enum, Num)
newtype NPos  = NPos  Int deriving (Show, Eq, Ord, Enum, Num)
newtype MSize = MSize Int deriving (Show, Eq, Ord, Enum, Num)
newtype NSize = NSize Int deriving (Show, Eq, Ord, Enum, Num)
newtype MVector a = MVector (V.Vector a) deriving (Show, Eq)
newtype NVector a = NVector (V.Vector a) deriving (Show, Eq)

newtype Cost = Cost Double deriving (Show, Eq, Ord, Enum, Num, Fractional)

class ToCost a where
    toCost :: a -> Cost

doubleCost :: Cost
doubleCost = 2
singleCost :: Cost
singleCost = 1
noCost :: Cost
noCost = 0

mindex :: MVector a -> MPos -> Maybe a
mindex (MVector mv) mpos = mv !? fromEnum mpos
nindex :: NVector a -> NPos -> Maybe a
nindex (NVector nv) npos = nv !? fromEnum npos
mlength :: MVector a -> MSize
mlength (MVector mv) = MSize $ V.length mv
nlength :: NVector a -> NSize
nlength (NVector nv) = NSize $ V.length nv

data CompareResult aux =
      ItemEqual
    | ItemModify Cost aux
    | ItemDeleteAdd
    deriving (Eq, Ord)

instance Show (CompareResult aux) where
    show ItemEqual        = "EQ"
    show (ItemModify c _) = "M" ++ show (fromEnum $ 100 * c)
    show ItemDeleteAdd    = "NE"

instance ToCost (CompareResult aux) where
    toCost ItemEqual        = noCost
    toCost (ItemModify c _) = c
    toCost ItemDeleteAdd    = doubleCost

data CompareTable item aux =
    CompareTable (MVector (NVector (CompareResult aux, (item, item))))
    deriving (Show, Eq)

getCompareResult ::
    MPos -> NPos -> CompareTable item aux -> Maybe (CompareResult aux, (item, item))
getCompareResult mpos npos (CompareTable mvector) =
    mindex mvector mpos >>= flip nindex npos

showCompareTable :: CompareTable item aux -> [String]
showCompareTable (CompareTable (MVector mvector)) =
    V.toList $ flip fmap mvector $ \ (NVector nvector) ->
        show $ V.toList $ flip fmap nvector $ fromEnum . (100 *) . toCost . fst

mkCompareTable ::
       (MPos -> NPos -> (CompareResult aux, (item, item)))
    -> (MSize, NSize)
    -> CompareTable item aux
mkCompareTable f (msize, nsize) = CompareTable mvector
  where
    mvector            = MVector $ V.generate (fromEnum msize) genNVector
    genNVector midx    = NVector $ V.generate (fromEnum nsize) $ genValue midx
    genValue midx nidx = f (MPos midx) (NPos nidx)

------

newtype K     = K     Int deriving (Show, Eq, Ord, Enum, Num)
newtype P     = P     Int deriving (Show, Eq, Ord, Enum, Num)
newtype Delta = Delta Int deriving (Show, Eq, Ord, Enum, Num)
newtype Index = Index Int deriving (Show, Eq, Ord, Enum, Num)

instance ToCost K where
    toCost = (singleCost *) . toEnum . coerce
instance ToCost P where
    toCost = (doubleCost *) . toEnum . coerce

mkIndex :: K -> P -> Index
mkIndex k p = coerce k + coerce p

nposFromKWithMPos :: K -> MPos -> NPos
nposFromKWithMPos k mpos = NPos $ fromEnum mpos - fromEnum k
deltaFromVectors :: MVector a -> NVector a -> Delta
deltaFromVectors mv nv = Delta $ fromEnum (mlength mv) - fromEnum (nlength nv)

data Diff item aux =
      Delete    item
    | Add       item
    | Equal     item item
    | Modify    item item Cost aux
    | DeleteAdd item item
    deriving (Eq, Ord)

instance Show item => Show (Diff item aux) where
    show (Delete    v)         = "Del "    <> show v
    show (Add       v)         = "Add "    <> show v
    show (Equal     vb va)     = "Eql "    <> show vb <> " = " <> show va
    show (Modify    vb va c _) = "Mod "    <> show vb <> " -> " <> show va <> " (" <> show c <> ")"
    show (DeleteAdd vb va)     = "DelAdd " <> show vb <> " -> " <> show va

instance ToCost (Diff item aux) where
    toCost (Delete    _)       = singleCost
    toCost (Add       _)       = singleCost
    toCost (Equal     _ _)     = noCost
    toCost (Modify    _ _ c _) = c
    toCost (DeleteAdd _ _)     = doubleCost

data MakeDiff item aux = MakeDiff
    { mkOnlyM   :: item -> Diff item aux
    , mkOnlyN   :: item -> Diff item aux
    --, mkDiffMN  :: item -> item -> CompareResult aux -> Diff item aux
    }

makeDiffMtoN :: MakeDiff item aux
makeDiffMtoN = MakeDiff Delete Add

makeDiffNtoM :: MakeDiff item aux
makeDiffNtoM = MakeDiff Add Delete

newtype WrappedDiff item aux = WrappedDiff { unwrap :: Diff item aux }

instance Show (WrappedDiff item aux) where
    show (WrappedDiff (Delete    _))       = "D"
    show (WrappedDiff (Add       _))       = "A"
    show (WrappedDiff (Equal     _ _))     = "E"
    show (WrappedDiff (Modify    _ _ c _)) = "M" <> show (fromEnum $ 100 * c)
    show (WrappedDiff (DeleteAdd _ _))     = "DA"

instance Eq (WrappedDiff item aux) where
    WrappedDiff (Delete    _)       == WrappedDiff (Delete    _)       = True
    WrappedDiff (Add       _)       == WrappedDiff (Add       _)       = True
    WrappedDiff (Equal     _ _)     == WrappedDiff (Equal     _ _)     = True
    WrappedDiff (Modify    _ _ _ _) == WrappedDiff (Modify    _ _ _ _) = True
    WrappedDiff (DeleteAdd _ _)     == WrappedDiff (DeleteAdd _ _)     = True
    _                               == _                               = False

instance Ord (WrappedDiff item aux) where
    compare (WrappedDiff s1) (WrappedDiff s2) =
        compare (diffInt s1) (diffInt s2)
      where
        diffInt :: Diff item aux -> Int
        diffInt (Equal     _ _)     = 5
        diffInt (Modify    _ _ _ _) = 4
        diffInt (DeleteAdd _ _)     = 3
        diffInt (Delete    _)       = 2
        diffInt (Add       _)       = 1

data Status item aux = Status
    { sDiffs :: [WrappedDiff item aux]
    , sCost  :: Cost
    , sReach :: MPos
    , sK     :: K
    } deriving (Eq)

instance Show (Status item aux) where
    show s = "Status{cost:" <> show (fromEnum $ sCost s) <>
             ", reach:" <> show (fromEnum $ sReach s) <>
             ", k:" <> show (fromEnum $ sK s) <>
             ", diff:" <> show (L.reverse $ sDiffs s) <>
             "}"

instance Ord (Status item aux) where
    compare s1 s2
        | rReach /= EQ = rReach
        | rCost  /= EQ = rCost
        | otherwise    = rDiffs
      where
        rReach = sReach s1 `compare` sReach s2
        rCost  = sCost  s2 `compare` sCost  s1 -- reverse order
        rDiffs = L.reverse (sDiffs s1) `compare` L.reverse (sDiffs s2)

expandDeleteAdd :: Status item aux -> Status item aux
expandDeleteAdd status = status {sDiffs = expandedDiffs}
  where
    expandedDiffs = fmap WrappedDiff $ expandDiff $ fmap unwrap $ sDiffs status
    expandDiff :: [Diff item aux] -> [Diff item aux]
    expandDiff = foldr foldProc []
    foldProc :: Diff item aux -> [Diff item aux] -> [Diff item aux]
    foldProc (DeleteAdd itemb itema) prev = (Add itema:(takeWhile isAdd prev)) <> (Delete itemb:(dropWhile isAdd prev))
    foldProc (Delete itemb)          prev = takeWhile isAdd prev <> (Delete itemb:(dropWhile isAdd prev))
    foldProc diff                    prev = diff:prev
    isAdd (Add _) = True
    isAdd _       = False

type OverlappedStatus item aux = [Status item aux]
type StatusLine item aux = V.Vector (OverlappedStatus item aux)

data StatusField item aux = StatusField
    { sfStatusLine :: StatusLine item aux
    , sfPrevious   :: Maybe (StatusField item aux)
    , sfPhase      :: P
    } deriving (Show, Eq)

showStatusField :: StatusField item aux -> [String]
showStatusField field = prevLineStrs ++ lineStrs
  where
    len  = V.length line
    idxs = [0..(len - 1)]
    line = sfStatusLine field
    p    = sfPhase field
    prevLineStrs :: [String] = case sfPrevious field of
        Nothing   -> []
        Just prev -> showStatusField prev
    lineStrs :: [String] =
        L.concat $
            flip map idxs $ \ idx ->
                flip fmap (line ! idx) $ \ s ->
                    show (fromEnum p) <> "-" <> show idx <> ": " <> show s

snakeXY ::
       CompareTable item aux
    -> Cost
    -> (MPos, NPos)
    -> Status item aux
    -> [Status item aux]
snakeXY table maxCost = go []
  where
    go results (mpos, npos) status = case cond of
        Nothing                         -> status:results
        Just (itemb, itema, compResult) -> go nextResults nextPos nextStatus
          where
            nextPos = (mpos + 1, npos + 1)
            nextResults = if toCost compResult == noCost
                then results
                else status : results
            rawDiff ItemEqual          = Equal itemb itema
            rawDiff (ItemModify c aux) = Modify itemb itema c aux
            rawDiff ItemDeleteAdd      = DeleteAdd itemb itema
            nextStatus = status
                { sDiffs = WrappedDiff (rawDiff compResult) : sDiffs status
                , sReach = sReach status + 1
                , sCost  = sCost  status + toCost compResult
                }
      where
        cond = do
            (compResult, (itemb, itema)) <- getCompareResult mpos npos table
            if sCost status + toCost compResult > maxCost
                then Nothing
                else Just (itemb, itema, compResult)

snakeK ::
       CompareTable item aux
    -> Cost
    -> K
    -> Status item aux
    -> [Status item aux]
snakeK table maxCost k status = result
  where
    result = snakeXY table maxCost (mpos, npos) status
    mpos = sReach status
    npos = nposFromKWithMPos k mpos
    resultPosStr = case result of
        [] -> "<no result>"
        _  -> show (fromEnum resultMPos, fromEnum resultNPos)
    resultMPos = sReach resultMax
    resultNPos = nposFromKWithMPos (sK resultMax) resultMPos
    resultMax  = maximum result
    trace = D.trace ("snake " <> show (fromEnum mpos, fromEnum npos) <> " (" <> (show $ getCost mpos npos table) <> ") -> " <> resultPosStr <> " (OriginCost: " <> show (fromEnum $ sCost status) <> ", MaxCost: " <> show (fromEnum maxCost) <> ")")

solve ::
     (MVector item, NVector item)
  -> MakeDiff item aux
  -> CompareTable item aux
  -> StatusField item aux
  -> Status item aux
solve (mv, nv) mkDiff table = go
  where
    go field | resultReach /= goalReach = go nextField
             | otherwise                = expandDeleteAdd $ traceField "result" resultStatus
      where
        nextPhase    = sfPhase field + 1
        nextField    = step (mv, nv) mkDiff delta nextPhase table field
        nextLine     = sfStatusLine nextField
        kpidx        = Index $ fromEnum delta + fromEnum nextPhase
        resultStatus = case getFromLine nextLine kpidx of
            [] -> error $ "solve empty list: " <> show nextPhase <> ", " <> show kpidx <> ", " <> show delta
            ss -> maximum ss
        resultReach  = sReach resultStatus
        traceField str = D.trace $
            str <>
            " P=" <> show (fromEnum nextPhase) <>
            " reach=" <> show (fromEnum resultReach) <>
            "/" <> show (fromEnum goalReach) <> "\n" <>
            L.intercalate "\n" (showStatusField nextField) <>
            "\n----------------"
    delta = deltaFromVectors mv nv
    goalReach = toEnum . fromEnum $ mlength mv

step ::
       (MVector item, NVector item)
    -> MakeDiff item aux
    -> Delta
    -> P
    -> CompareTable item aux
    -> StatusField item aux
    -> StatusField item aux
step (mv, nv) mkDiff delta nextPhase table field = field
    { sfStatusLine = nextLine
    , sfPrevious   = Just field
    , sfPhase      = nextPhase
    }
  where
    startK   = - coerce nextPhase
    finishK  = coerce nextPhase + coerce delta
    prevLine = sfStatusLine field
    nextLine = V.unfoldr (unfoldFunc nextLine nextPhase) startK
    unfoldFunc currentLine p k
        | k <= finishK = mkUnfoldResult (k+1) $ snakeMulti p k ss
        | otherwise    = Nothing
      where
        kpidx = mkIndex k p
        ss | p == 0 && k == 0 = [initStatus]
           | otherwise        = case k `compare` coerce delta of
            LT -> moveYCurrent <> moveXYModify <> moveXPrev
            EQ -> moveYCurrent <> moveXYModify <> moveXCurrent
            GT -> moveYPrev    <> moveXYModify <> moveXCurrent
        moveXCurrent  = mapMoveX $ getFromLine currentLine $ kpidx + 1
        moveYCurrent  = mapMoveY $ getFromLine currentLine $ kpidx - 1
        moveXPrev     = mapMoveX $ getFromLine prevLine    $ kpidx
        moveYPrev     = mapMoveY $ getFromLine prevLine    $ kpidx - 2
        moveXYModify  = maxReach $ getFromLine prevLine    $ kpidx - 1
    mapMoveX = MB.mapMaybe moveX
    mapMoveY = MB.mapMaybe moveY
    moveX status = case nindex nv npos of
        Nothing -> Nothing
        Just n  -> Just status
            { sDiffs = WrappedDiff (mkOnlyN mkDiff n) : sDiffs status
            , sCost  = sCost status + singleCost
            , sK     = sK    status - 1
            }
      where
        mpos :: MPos = sReach status
        npos :: NPos = nposFromKWithMPos (sK status) mpos
    moveY status = case mindex mv $ sReach status of
        Nothing -> Nothing
        Just m  -> Just status
            { sDiffs = WrappedDiff (mkOnlyM mkDiff m) : sDiffs status
            , sReach = sReach status + 1
            , sCost  = sCost  status + singleCost
            , sK     = sK     status + 1
            }
    maxReach :: [Status item aux] -> [Status item aux]
    maxReach [] = []
    maxReach ss = (: []) $ L.maximumBy (\ a b -> sReach a `compare` sReach b) ss
    snakeMulti p k = filterMinCost k p . shrink . mapSnake k p . shrink
    mkUnfoldResult k ss = Just (ss, k)
    mapSnake k p = concat . map (mapSnakeFunc k p)
    mapSnakeFunc k p = snakeK table (calcMaxCost k p) k
    prevMaxCost k p = calcMaxCost k $ p - 1
    calcMaxCost k p | k <= coerce delta = toCost k + toCost p
                    | otherwise         = toCost (2 * coerce delta - k) + toCost p
    filterMinCost k p = filter ((prevMaxCost k p <) . sCost)
    initStatus = Status
        { sDiffs = []
        , sCost  = 0
        , sReach = 0
        , sK     = 0
        }

getFromLine :: StatusLine item aux -> Index -> [Status item aux]
getFromLine line (Index idx) = case line !? idx of
    Just ss -> ss
    Nothing -> []

shrink :: [Status item aux] -> [Status item aux]
shrink = foldr foldFunc [] . L.sort
  where
    foldFunc :: Status item aux -> [Status item aux] -> [Status item aux]
    foldFunc s [] = [s]
    foldFunc s results@(r:_)
        | sReach s == sReach r = results
        | otherwise            = if all (allFunc s) results
            then s : results
            else results
    allFunc :: Status item aux -> Status item aux -> Bool
    allFunc s result = sCost result > sCost s

getVectorDiffBy ::
     (item -> item -> CompareResult aux)
  -> V.Vector item
  -> V.Vector item
  -> [Diff item aux]
getVectorDiffBy f beforev afterv =
    debug $
    L.reverse $ map unwrap $ sDiffs $ solve vectors makeDiff table initStatus
  where
    debug = D.trace (L.intercalate "\n" $ showCompareTable table)
    lenB = V.length beforev
    lenA = V.length afterv
    vectorsMtoN = (MVector beforev, NVector afterv)
    vectorsNtoM = (MVector afterv, NVector beforev)
    size2dMtoN  = (MSize lenB, NSize lenA)
    size2dNtoM  = (MSize lenA, NSize lenB)
    itemsfMtoN mpos npos = (MB.fromJust $ mindex mvector mpos, MB.fromJust $ nindex nvector npos)
    itemsfNtoM mpos npos = (MB.fromJust $ nindex nvector npos, MB.fromJust $ mindex mvector mpos)
    g mpos npos = (f itemb itema, (itemb, itema))
      where
        (itemb, itema) = itemsf mpos npos
    (vectors@(mvector, nvector), size2d, itemsf, makeDiff)
        | lenB > lenA = (vectorsMtoN, size2dMtoN, itemsfMtoN, makeDiffMtoN)
        | otherwise   = (vectorsNtoM, size2dNtoM, itemsfNtoM, makeDiffNtoM)
    table = mkCompareTable g size2d
    initStatus = StatusField
        { sfStatusLine = V.empty
        , sfPrevious   = Nothing
        , sfPhase      = -1
        }

getVectorDiff :: Eq item => V.Vector item -> V.Vector item -> [Diff item ()]
getVectorDiff = getVectorDiffBy f
  where
    f itemb itema | itemb == itema = ItemEqual
                  | otherwise      = ItemDeleteAdd

getDiff :: (IsList l, Eq (Item l)) => l -> l -> [Diff (Item l) ()]
getDiff before after =
    getVectorDiff (V.fromList $ toList before) (V.fromList $ toList after)

getDiffByDiff ::
    (IsList l, IsList (Item l), Eq l, Eq (Item l), Eq (Item (Item l))) =>
    l -> l -> [Diff (Item l) [Diff (Item (Item l)) ()]]
getDiffByDiff before after =
    getVectorDiffBy f (toVector before) (toVector after)
  where
    toVector :: (IsList l, Eq l, Eq (Item l)) => l -> V.Vector (Item l)
    toVector = V.fromList . toList
    f :: (IsList l, Eq l, Eq (Item l)) => l -> l -> CompareResult [Diff (Item l) ()]
    f mitem nitem | mitem == nitem          = ItemEqual
                  | cost < doubleCost * 0.7 = ItemModify cost diffs
                  | otherwise               = ItemDeleteAdd
      where
        diffs = getDiff mitem nitem
        cost  = doubleCost * ses / len
        len   = toEnum $ length (toList mitem) + length (toList nitem)
        ses   = sum $ fmap toCost diffs

traceIdWith :: (a -> String) -> a -> a
traceIdWith f a = D.trace (f a) a

traceShowIdWithTag :: Show a => String -> a -> a
traceShowIdWithTag str = traceIdWith (((str <> ": ") <>) . show)

traceSSS :: String -> [[Status item aux]] -> [[Status item aux]]
traceSSS str = traceIdWith ((<> ("\n" <> "<-----------" <> str)) . ((">-----------" <> str <>"\n") <>) . L.intercalate "\n----<>----\n" . fmap (L.intercalate "\n" . fmap show))

traceSS :: String -> [Status item aux] -> [Status item aux]
traceSS str = traceIdWith ((<> ("\n" <> "<--------" <> str)) . ((">--------" <> str <>"\n") <>) . L.intercalate "\n" . fmap show)

traceS :: String -> Status item aux -> Status item aux
traceS str = traceIdWith ((<> ("\n" <> "<-----" <> str)) . ((">-----" <> str <>"\n") <>) . show)
---}

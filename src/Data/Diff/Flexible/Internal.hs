{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Data.Diff.Flexible.Internal
    ( Cost
    , ToCost(..)
    , doubleCost
    , singleCost
    , noCost
    , CompareResult(..)
    , RawDiff(..)
    , RawDiff_
    , getVectorDiffBy
    , getVectorDiff
    , getDiff
    , getDiffByDiff
    ) where

import qualified Data.Vector as V
import           Data.Vector ((!), (!?))
import qualified Data.List   as L
import qualified Data.Maybe  as MB
import           Data.Coerce (coerce)

import           GHC.Exts (IsList(..))

-- import qualified Debug.Trace as D

------
-- | = Cost and CompareResult

newtype Cost = Cost Double deriving (Show, Eq, Ord, Enum, Num, Fractional)

class ToCost a where
    toCost :: a -> Cost

doubleCost :: Cost
doubleCost = 2
singleCost :: Cost
singleCost = 1
noCost :: Cost
noCost = 0

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

------
-- | = Diff

data RawDiff aux item =
      Delete    item
    | Add       item
    | Equal     item item
    | Modify    item item Cost aux
    | DeleteAdd item item
    deriving (Eq, Ord)

type RawDiff_ item = RawDiff () item

instance Show item => Show (RawDiff aux item) where
    show (Delete    v)         = "Del "    <> show v
    show (Add       v)         = "Add "    <> show v
    show (Equal     vb va)     = "Eql "    <> show vb <> " = " <> show va
    show (Modify    vb va c _) = "Mod "    <> show vb <> " -> " <> show va <> " (" <> show c <> ")"
    show (DeleteAdd vb va)     = "DelAdd " <> show vb <> " -> " <> show va

instance ToCost (RawDiff aux item) where
    toCost (Delete    _)       = singleCost
    toCost (Add       _)       = singleCost
    toCost (Equal     _ _)     = noCost
    toCost (Modify    _ _ c _) = c
    toCost (DeleteAdd _ _)     = doubleCost

instance Functor (RawDiff aux) where
    fmap f (Delete    itemb)             = Delete    (f itemb)
    fmap f (Add       itema)             = Add       (f itema)
    fmap f (Equal     itemb itema)       = Equal     (f itemb) (f itema)
    fmap f (Modify    itemb itema c aux) = Modify    (f itemb) (f itema) c aux
    fmap f (DeleteAdd itemb itema)       = DeleteAdd (f itemb) (f itema)

------
-- M and N

newtype MPos  = MPos  Int deriving (Show, Eq, Ord, Enum, Num)
newtype NPos  = NPos  Int deriving (Show, Eq, Ord, Enum, Num)
newtype MSize = MSize Int deriving (Show, Eq, Ord, Enum, Num)
newtype NSize = NSize Int deriving (Show, Eq, Ord, Enum, Num)
newtype MVector a = MVector (V.Vector a) deriving (Show, Eq)
newtype NVector a = NVector (V.Vector a) deriving (Show, Eq)

mindex :: MVector a -> MPos -> Maybe a
mindex (MVector mv) mpos = mv !? fromEnum mpos
nindex :: NVector a -> NPos -> Maybe a
nindex (NVector nv) npos = nv !? fromEnum npos
mindexUnsafe :: MVector a -> MPos -> a
mindexUnsafe (MVector mv) mpos = mv ! fromEnum mpos
nindexUnsafe :: NVector a -> NPos -> a
nindexUnsafe (NVector nv) npos = nv ! fromEnum npos
mlength :: MVector a -> MSize
mlength (MVector mv) = MSize $ V.length mv
nlength :: NVector a -> NSize
nlength (NVector nv) = NSize $ V.length nv

------
-- CompareTable

data Cell aux item = Cell
    { cellResult     :: CompareResult aux
    , cellItemBefore :: item
    , cellItemAfter  :: item
    } deriving (Show, Eq, Ord)

instance ToCost (Cell aux item) where
    toCost = toCost . cellResult

diffFromCell :: Cell aux item -> RawDiff aux item
diffFromCell cell = case cellResult cell of
    ItemEqual        -> Equal itemb itema
    ItemModify c aux -> Modify itemb itema c aux
    ItemDeleteAdd    -> DeleteAdd itemb itema
  where
    itemb = cellItemBefore cell
    itema = cellItemAfter  cell

data CompareTable aux item =
    CompareTable (MVector (NVector (Cell aux item)))
    deriving (Show, Eq)

showCompareTable :: CompareTable aux item -> [String]
showCompareTable (CompareTable (MVector mvector)) =
    V.toList $ flip fmap mvector $ \ (NVector nvector) ->
        show $ V.toList $ flip fmap nvector $ fromEnum . (100 *) . toCost

getCompareResult ::
       MPos
    -> NPos
    -> CompareTable aux item
    -> Maybe (Cell aux item)
getCompareResult mpos npos (CompareTable mvector) =
    mindex mvector mpos >>= flip nindex npos

mkCompareTable ::
       (MPos -> NPos -> Cell aux item)
    -> (MSize, NSize)
    -> CompareTable aux item
mkCompareTable f (msize, nsize) = CompareTable mvector
  where
    mvector            = MVector $ V.generate (fromEnum msize) genNVector
    genNVector midx    = NVector $ V.generate (fromEnum nsize) $ genValue midx
    genValue midx nidx = f (MPos midx) (NPos nidx)

------
-- K and P

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

------
-- MakeDiff

data MakeDiff aux item = MakeDiff
    { mkOnlyM :: item -> RawDiff aux item
    , mkOnlyN :: item -> RawDiff aux item
    }

makeDiffMtoN :: MakeDiff aux item
makeDiffMtoN = MakeDiff Delete Add

makeDiffNtoM :: MakeDiff aux item
makeDiffNtoM = MakeDiff Add Delete

------
-- WrappedDiff

newtype WrappedDiff aux item = WrappedDiff { unwrap :: RawDiff aux item }

instance Show (WrappedDiff aux item) where
    show (WrappedDiff (Delete    _))       = "D"
    show (WrappedDiff (Add       _))       = "A"
    show (WrappedDiff (Equal     _ _))     = "E"
    show (WrappedDiff (Modify    _ _ c _)) = "M" <> show (fromEnum $ 100 * c)
    show (WrappedDiff (DeleteAdd _ _))     = "DA"

instance Eq (WrappedDiff aux item) where
    WrappedDiff (Delete    _)       == WrappedDiff (Delete    _)       = True
    WrappedDiff (Add       _)       == WrappedDiff (Add       _)       = True
    WrappedDiff (Equal     _ _)     == WrappedDiff (Equal     _ _)     = True
    WrappedDiff (Modify    _ _ _ _) == WrappedDiff (Modify    _ _ _ _) = True
    WrappedDiff (DeleteAdd _ _)     == WrappedDiff (DeleteAdd _ _)     = True
    _                               == _                               = False

instance Ord (WrappedDiff aux item) where
    compare (WrappedDiff s1) (WrappedDiff s2) =
        compare (diffInt s1) (diffInt s2)
      where
        diffInt :: RawDiff aux item -> Int
        diffInt (Equal     _ _)     = 5
        diffInt (Modify    _ _ _ _) = 4
        diffInt (DeleteAdd _ _)     = 3
        diffInt (Delete    _)       = 2
        diffInt (Add       _)       = 1

diffTypeEq :: RawDiff aux item -> RawDiff aux item -> Bool
diffTypeEq diff1 diff2 = WrappedDiff diff1 == WrappedDiff diff2

------
-- Status

data Status aux item = Status
    { sDiffs :: [WrappedDiff aux item] -- reverse ordered
    , sCost  :: Cost
    , sReach :: MPos
    , sK     :: K
    } deriving (Eq)

instance Show (Status aux item) where
    show s = "Status{cost:" <> show (fromEnum $ sCost s) <>
             ", reach:"     <> show (fromEnum $ sReach s) <>
             ", k:"         <> show (fromEnum $ sK s) <>
             ", diff:"      <> show (L.reverse $ sDiffs s) <>
             "}"

instance Ord (Status aux item) where
    compare s1 s2
        | rReach /= EQ = rReach
        | rCost  /= EQ = rCost
        | otherwise    = rDiffs
      where
        rReach = sReach s1 `compare` sReach s2
        rCost  = sCost  s2 `compare` sCost  s1 -- reverse order
        rDiffs = L.reverse (sDiffs s1) `compare` L.reverse (sDiffs s2)

posMNFromStatus :: Status aux item -> (MPos, NPos)
posMNFromStatus status = (mpos, npos)
  where
    mpos = sReach status
    npos = nposFromKWithMPos (sK status) mpos

expandDeleteAdd :: Status aux item -> Status aux item
expandDeleteAdd status = status {sDiffs = expandedDiffs}
  where
    expandedDiffs = fmap WrappedDiff $ expandDiff $ fmap unwrap $ sDiffs status
    expandDiff :: [RawDiff aux item] -> [RawDiff aux item]
    expandDiff = fst . foldr foldFunc ([], ([], []))
    foldFunc ::
           RawDiff aux item
        -> ([RawDiff aux item], ([RawDiff aux item], [RawDiff aux item]))
        -> ([RawDiff aux item], ([RawDiff aux item], [RawDiff aux item]))
    foldFunc diff (_, (adds, prev)) = case diff of
        DeleteAdd b a -> (Add a : adds <> (Delete b : prev), (Add a : adds, Delete b : prev))
        Delete    b   -> (adds <> (Delete b : prev), (adds, Delete b : prev))
        diff          -> (diff : adds <> prev, ([], diff : adds <> prev))

------
-- StatusLine, StatusField

type StatusLine aux item = V.Vector [Status aux item]

getFromLine :: StatusLine aux item -> Index -> [Status aux item]
getFromLine line (Index idx) = case line !? idx of
    Just ss -> ss
    Nothing -> []

data StatusField aux item = StatusField
    { sfStatusLine :: StatusLine aux item
    , sfPrevious   :: Maybe (StatusField aux item)
    , sfPhase      :: P
    } deriving (Show, Eq)

------
-- algorithm

snakeXY ::
       CompareTable aux item
    -> Cost
    -> (MPos, NPos)
    -> Status aux item
    -> [Status aux item]
snakeXY table maxCost = uncurry $ go []
  where
    go results mpos npos status = case mcell of
        Nothing   -> status : results
        Just cell -> go nextResults (mpos + 1) (npos + 1) nextStatus
          where
            nextResults = if toCost cell == noCost
                then results
                else status : results
            nextStatus = status
                { sDiffs = WrappedDiff (diffFromCell cell) : sDiffs status
                , sReach = sReach status + 1
                , sCost  = sCost  status + toCost cell
                }
      where
        mcell = do
            cell <- getCompareResult mpos npos table
            if sCost status + toCost cell > maxCost
                then Nothing
                else Just cell

snakeK ::
       CompareTable aux item
    -> Cost
    -> Status aux item
    -> [Status aux item]
snakeK table maxCost status = result
  where
    result = snakeXY table maxCost (mpos, npos) status
    (mpos, npos) = posMNFromStatus status

solve ::
     (MVector item, NVector item)
  -> MakeDiff aux item
  -> CompareTable aux item
  -> StatusField aux item
  -> Status aux item
solve (mv, nv) mkDiff table = go
  where
    go field | resultReach /= goalReach = go nextField
             | otherwise                = expandDeleteAdd resultStatus
      where
        nextPhase    = sfPhase field + 1
        nextField    = step (mv, nv) mkDiff delta nextPhase table field
        nextLine     = sfStatusLine nextField
        kpidx        = Index $ fromEnum delta + fromEnum nextPhase
        resultStatus = case getFromLine nextLine kpidx of
            [] -> error $ "solve empty list: " <> show nextPhase <> ", " <> show kpidx <> ", " <> show delta
            ss -> maximum ss
        resultReach  = sReach resultStatus
    delta = deltaFromVectors mv nv
    goalReach = toEnum . fromEnum $ mlength mv

step ::
       (MVector item, NVector item)
    -> MakeDiff aux item
    -> Delta
    -> P
    -> CompareTable aux item
    -> StatusField aux item
    -> StatusField aux item
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
    moveX status = do
        nitem <- nindex nv npos
        return status
            { sDiffs = WrappedDiff (mkOnlyN mkDiff nitem) : sDiffs status
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
    maxReach :: [Status aux item] -> [Status aux item]
    maxReach [] = []
    maxReach ss = (: []) $ L.maximumBy compareReach ss
      where
        compareReach a b = sReach a `compare` sReach b
    snakeMulti p k = filterMinCost k p . shrink . mapSnake k p . shrink
    mkUnfoldResult k ss = Just (ss, k)
    mapSnake k p = concat . map (mapSnakeFunc k p)
    mapSnakeFunc k p = snakeK table (calcMaxCost k p)
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

shrink :: [Status aux item] -> [Status aux item]
shrink = foldr foldFunc [] . L.sort
  where
    foldFunc :: Status aux item -> [Status aux item] -> [Status aux item]
    foldFunc s [] = [s]
    foldFunc s results@(r:_)
        | sReach s == sReach r = results
        | otherwise            = if all (allFunc s) results
            then s : results
            else results
    allFunc :: Status aux item -> Status aux item -> Bool
    allFunc s result = sCost result > sCost s

getVectorDiffBy ::
     (item -> item -> CompareResult aux)
  -> V.Vector item
  -> V.Vector item
  -> [RawDiff aux item]
getVectorDiffBy f beforev afterv =
    L.reverse $ map unwrap $ sDiffs $ solve vectors makeDiff table initStatus
  where
    lenB = V.length beforev
    lenA = V.length afterv
    vectorsMtoN = (MVector beforev, NVector afterv)
    vectorsNtoM = (MVector afterv, NVector beforev)
    size2dMtoN  = (MSize lenB, NSize lenA)
    size2dNtoM  = (MSize lenA, NSize lenB)
    itemsfMtoN mpos npos = (mindexUnsafe mvector mpos, nindexUnsafe nvector npos)
    itemsfNtoM mpos npos = (nindexUnsafe nvector npos, mindexUnsafe mvector mpos)
    g mpos npos = Cell
        { cellResult     = f itemb itema
        , cellItemBefore = itemb
        , cellItemAfter  = itema
        }
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

getVectorDiff :: Eq item =>
    V.Vector item -> V.Vector item -> [RawDiff () item]
getVectorDiff = getVectorDiffBy f
  where
    f itemb itema | itemb == itema = ItemEqual
                  | otherwise      = ItemDeleteAdd

getDiff :: (IsList l, Eq (Item l)) => l -> l -> [RawDiff () (Item l)]
getDiff before after =
    getVectorDiff (V.fromList $ toList before) (V.fromList $ toList after)

getDiffByDiff ::
    (IsList l, IsList (Item l), Eq l, Eq (Item l), Eq (Item (Item l))) =>
    l -> l -> [RawDiff [RawDiff () (Item (Item l))] (Item l)]
getDiffByDiff before after =
    getVectorDiffBy f (toVector before) (toVector after)
  where
    toVector :: (IsList l, Eq l, Eq (Item l)) => l -> V.Vector (Item l)
    toVector = V.fromList . toList
    f :: (IsList l, Eq l, Eq (Item l)) =>
        l -> l -> CompareResult [RawDiff () (Item l)]
    f mitem nitem | mitem == nitem          = ItemEqual
                  | cost < doubleCost * 0.7 = ItemModify cost diffs
                  | otherwise               = ItemDeleteAdd
      where
        diffs = getDiff mitem nitem
        cost  = doubleCost * ses / len
        len   = toEnum $ length (toList mitem) + length (toList nitem)
        ses   = sum $ fmap toCost diffs

{-
traceIdWith :: (a -> String) -> a -> a
traceIdWith f a = D.trace (f a) a

traceShowIdWithTag :: Show a => String -> a -> a
traceShowIdWithTag str = traceIdWith (((str <> ": ") <>) . show)

traceSSS :: String -> [[Status aux item]] -> [[Status aux item]]
traceSSS str = traceIdWith ((<> ("\n" <> "<-----------" <> str)) . ((">-----------" <> str <>"\n") <>) . L.intercalate "\n----<>----\n" . fmap (L.intercalate "\n" . fmap show))

traceSS :: String -> [Status aux item] -> [Status aux item]
traceSS str = traceIdWith ((<> ("\n" <> "<--------" <> str)) . ((">--------" <> str <>"\n") <>) . L.intercalate "\n" . fmap show)

traceS :: String -> Status aux item -> Status aux item
traceS str = traceIdWith ((<> ("\n" <> "<-----" <> str)) . ((">-----" <> str <>"\n") <>) . show)

traceSnake ::
       (CompareTable aux item -> Cost -> Status aux item -> [Status aux item])
    -> CompareTable aux item -> Cost -> Status aux item -> [Status aux item]
traceSnake snake table maxCost status = trace results
  where
    results = snake table maxCost status
    (mpos, npos) = posMNFromStatus status
    posStr     = show (fromEnum mpos, fromEnum npos)
    costStr    = show (fromEnum . (100 *) $ sCost status)
    maxCostStr = show (fromEnum maxCost)
    (resultPosStr, resultCostStr) = case results of
        [] -> ("<no result>", "<no result>")
        _  -> (show (fromEnum resultMPos, fromEnum resultNPos), show )
          where
            resultStatus = maximum results
            (resultMPos, resultNPos) = posMNFromStatus resultStatus
            resultCost = fromEnum . (100 *) $ toCost resultStatus
    trace = D.trace $
        "snake "  <> posStr  <> " -> " <> resultPosStr  <>
        " (Cost " <> costStr <> " -> " <> resultCostStr <>
        ", MaxCost: " <> maxCostStr <> ")"

showStatusField :: StatusField aux item -> [String]
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

traceField :: String -> StatusField aux item -> a -> a
traceField str field = D.trace $ str <> debugStr
  where
    debugStr = L.intercalate "\n" $
        ["statusField---------------->"] <>
        showStatusField field <>
        ["statusField<----------------"]

traceCompareTable :: CompareTable aux item -> a -> a
traceCompareTable = D.trace (L.intercalate "\n" $ showCompareTable table)

---}

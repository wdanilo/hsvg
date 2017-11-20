{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prologue hiding (Simple, subtract, point, duplicate)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set        as Set
import           Data.Set        (Set)
import           Control.Monad.State.Layered
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import qualified Data.Matrix  as Matrix
import           Data.Matrix  (Matrix(..))

import qualified Data.Color as Color
import           Data.Fixed              (mod')
import           Data.Hashable (Hashable, hash)

-----------------------
-- === Transform === --
-----------------------

-- === Definition === --

newtype Transform = Transform (Matrix Double) deriving (Eq, Show)
makeLenses ''Transform


-- === Instances === --

instance Mempty Transform where
  mempty = wrap $ Matrix.identity 3

-- FIXME: TO REMOVE:
instance Ord Transform where
  compare = compare `on` (Matrix.toList . unwrap)



----------------------
-- === Booleans === --
----------------------

data Boolean a
  = Subtract  a a
  | Intersect a a
  | Merge     a a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)



class Sub a b where
    type family SubResult a b
    (/-) :: a -> b -> SubResult a b

class Add a b where
    type family AddResult a b
    (/+) :: a -> b -> AddResult a b

class Div a b where
    type family DivResult a b
    (//) :: a -> b -> DivResult a b

class Mul a b where
    type family MulResult a b
    (/*) :: a -> b -> MulResult a b


-----------------
-- === Dim === --
-----------------

-- === Definition === --

class           Dim1 a where x :: Lens' a Double
class Dim1 a => Dim2 a where y :: Lens' a Double
class Dim2 a => Dim3 a where z :: Lens' a Double


-- === Utils === --

dist :: Dim2 a => a -> a -> Double
dist start end = sqrt $ dx * dx + dy * dy where
    dx = end^.x - start^.x
    dy = end^.y - start^.y

len :: Dim2 a => a -> Double
len a = sqrt $ (a ^. x) * (a ^. x) + (a ^. y) * (a ^. y)



--------------------
-- === Points === --
--------------------

-- === Definition === --

data Point = Point { __1 :: Double, __2 :: Double } deriving (Eq, Ord, Show)
makeLenses ''Point


-- === Utils === --

angle :: Point -> Point -> Double
angle (Point sx sy) (Point ex ey) = atan2 (ey - sy) (ex - sx)


-- === Instances === --

instance Dim1 Point where x = point_1
instance Dim2 Point where y = point_2

instance (a~b, a~Double) => Convertible (a,b) Point where convert (a,b) = Point a b
instance (a~b, a~Double) => Convertible Point (a,b) where convert (Point a b) = (a,b)



---------------------
-- === Vectors === --
---------------------

-- === Definition === --

data Vector = Vector { __1 :: Double, __2 :: Double }
makeLenses ''Vector

instance Dim1 Vector where x = vector_1
instance Dim2 Vector where y = vector_2


-- === Utils === --

unit :: Vector -> Vector
unit v = v // len v


-- === Arithmetic === ---

instance Sub Point Point where
    type SubResult Point Point = Vector
    Point x y /- Point x' y' = Vector (x - x') (y - y')

instance Add Point Vector where
    type AddResult Point Vector = Point
    Point x y /+ Vector x' y' = Point (x + x') (y + y')

instance Div Vector Double where
    type DivResult Vector Double = Vector
    Vector x y // a = Vector (x/a) (y/a)

instance Mul Vector Double where
    type MulResult Vector Double = Vector
    Vector x y /* a = Vector (x*a) (y*a)


-- === Instances === --

instance Num Vector where
    Vector x y + Vector x' y' = Vector (x + x') (y + y')
    Vector x y - Vector x' y' = Vector (x - x') (y - y')



--------------------------
-- === ControlPoint === --
--------------------------

data ControlPoint = ControlPoint
    { _leftHandle  :: Maybe Point
    , _rightHandle :: Maybe Point
    , _point       :: Point
    } deriving (Eq, Ord, Show)
makeLenses ''ControlPoint


instance (a~b, a~Double) => Convertible (a,b) ControlPoint where
  convert = ControlPoint mempty mempty . convert



pt   ::                     Double -> Double                     -> ControlPoint
lpt  :: Double -> Double -> Double -> Double                     -> ControlPoint
rpt  ::                     Double -> Double -> Double -> Double -> ControlPoint
lrpt :: Double -> Double -> Double -> Double -> Double -> Double -> ControlPoint
pt         x y       = ControlPoint Nothing              Nothing              $ Point x y
lpt  lx ly x y       = ControlPoint (Just $ Point lx ly) Nothing              $ Point x y
rpt        x y rx ry = ControlPoint Nothing              (Just $ Point rx ry) $ Point x y
lrpt lx ly x y rx ry = ControlPoint (Just $ Point lx ly) (Just $ Point rx ry) $ Point x y





---------------------------
-- === Smooth Points === --
---------------------------

data SmoothPoint = SmoothPoint { _point :: Point, _smooth :: Double } deriving (Eq, Ord, Show)

spt :: Double -> Double -> Double -> SmoothPoint
spt x y s = SmoothPoint (Point x y) s


------------------
-- === BBox === --
------------------

data BBox = BBox { _topLeft :: Point, _width :: Double, _height :: Double } deriving (Show)
makeLenses ''BBox

class Bounding a where
    bbox :: a -> BBox

bbcat :: BBox -> BBox -> BBox
bbcat (BBox (Point x y) w h) (BBox (Point x' y') w' h') = BBox (Point left top) (right - left) (bottom - top) where
    left   = min x x'
    top    = min y y'
    right  = max (x + w) (x' + w')
    bottom = max (y + h) (y' + h')

instance Mempty BBox where mempty = BBox (Point 0 0) 0 0

instance Bounding a => Bounding (Boolean a) where
    bbox = \case
        Subtract  a b -> bbcat (bbox a) (bbox b)
        Intersect a b -> bbcat (bbox a) (bbox b)
        Merge     a b -> bbcat (bbox a) (bbox b)



----------------------
-- === Geometry === --
----------------------

-- === Definition === --

type Material  = [Style]

data GeoDef
  = GeoEmpty
  | GeoSimple Shape
  | GeoCompound (Boolean Geo)
  deriving (Eq, Ord, Show)

data Shape
  = Rect   Double Double
  | Circle Double
  | Arc    Double Double
  | Path   [ControlPoint]
  | SPath  [SmoothPoint]
  deriving (Eq, Ord, Show)

data Hinting
  = None
  | SnapPixel
  deriving (Eq, Ord, Show)

data Style
  = Fill        Text
  | Stroke      Text
  | StrokeWidth Double
  | Hinting     Hinting
  deriving (Eq, Ord, Show)

data Geo = Geo { _material   :: Material
               , _transform  :: Transform
               , _definition :: GeoDef
               } deriving (Eq, Ord, Show)
makeLenses ''Geo


-- === Smart constructors === --

rect :: Double -> Double -> Geo
rect = Geo mempty mempty . GeoSimple .: Rect

square :: Double -> Geo
square a = rect a a

circle :: Double -> Geo
circle = Geo mempty mempty . GeoSimple . Circle

arc :: Double -> Double -> Geo
arc x y = Geo mempty mempty . GeoSimple $ Arc x y

path :: Convertible' p ControlPoint => [p] -> Geo
path = Geo mempty mempty . GeoSimple . Path . fmap convert'

spath :: Convertible' p SmoothPoint => [p] -> Geo
spath = Geo mempty mempty . GeoSimple . SPath . fmap convert'

spath2Path :: [SmoothPoint] -> [ControlPoint]
spath2Path = \case
    []  -> []
    lst -> spath2Path' (unsafeLast lst : lst <> [unsafeHead lst])

-- FIXME: TO FINISH
spath2Path' :: [SmoothPoint] -> [ControlPoint]
spath2Path' = undefined -- \case
    -- (SmoothPoint prevPt _ : current@(SmoothPoint pt s rs) : next@(SmoothPoint nextPt _) : rest) -> ControlPoint lh rh pt : spath2Path' (current : next : rest) where
    --     npts = if s == 0 then [ControlPoint Nothing Nothing pt]
    --                      else [ControlPoint Nothing (Just pt) $ txPt (mul s . unit $ sub prevPt pt) pt
    --     -- lh = Nothing
    --     -- rh = Nothing
    -- _ -> []

-- === Styling === --

fill :: Text -> Geo -> Geo
fill t = material %~ (Fill   t:)

strokeWidth :: Double -> Geo -> Geo
strokeColor :: Text   -> Geo -> Geo
strokeWidth a = material %~ (StrokeWidth a:)
strokeColor t = material %~ (Stroke t:)



-- === Booleans === --

subtract :: Geo -> Geo -> Geo
subtract = Geo mempty mempty . GeoCompound .: Subtract

intersect :: Geo -> Geo -> Geo
intersect = Geo mempty mempty . GeoCompound .: Intersect

merge :: Geo -> Geo -> Geo
merge = Geo mempty mempty . GeoCompound .: Merge

instance Mempty    Geo where mempty = Geo mempty mempty GeoEmpty
instance Semigroup Geo where
  (<>) = merge
instance Num Geo where
  (+) = merge
  (-) = flip subtract
  (*) = intersect


-- === Transformations === --

move :: Double -> Double -> Geo -> Geo
move x y = (transform . wrapped) %~ Matrix.multStd2 m where
  m = Matrix.fromList 3 3
    $ [ 1, 0, x
      , 0, 1, y
      , 0, 0, 1
      ]

rotate :: Double -> Geo -> Geo
rotate = rotateRad . (*(pi/180))

rotateRad :: Double -> Geo -> Geo
rotateRad a = (transform . wrapped) %~ Matrix.multStd2 m where
  m = Matrix.fromList 3 3
    $ [ cos a, -(sin a), 0
      , sin a,   cos a , 0
      , 0    , 0       , 1
      ]

alignLeft :: Geo -> Geo
alignLeft g = move (-x) 0 g where
    BBox (Point x _) _ _ = bbox g

alignRight :: Geo -> Geo
alignRight g = move (-x-w) 0 g where
    BBox (Point x _) w _ = bbox g

alignTop :: Geo -> Geo
alignTop g = move 0 (-y) g where
    BBox (Point _ y) _ _ = bbox g

alignBottom :: Geo -> Geo
alignBottom g = move 0 (-y-h) g where
    BBox (Point _ y) _ h = bbox g

alignTopLeft, alignTopRight, alignBottomLeft, alignBottomRight :: Geo -> Geo
alignTopLeft     = alignTop    . alignLeft
alignTopRight    = alignTop    . alignRight
alignBottomLeft  = alignBottom . alignLeft
alignBottomRight = alignBottom . alignRight


-- === Instances === --

instance Bounding Geo where
    bbox = bbox . view definition -- FIXME: handle transform

instance Bounding GeoDef where
    bbox = \case
      GeoEmpty      -> mempty
      GeoSimple   s -> bbox s
      GeoCompound b -> bbox b

instance Bounding Shape where
  bbox = \case
    Rect   w h -> BBox (Point (-w/2) (-h/2)) w h
    Circle r   -> BBox (Point (-r/2) (-r/2)) r r
    Arc    r _ -> BBox (Point (-r/2) (-r/2)) r r
    -- Path   pts -> mconcat $ bbox <$> pts
    -- Spath  pts -> mconcat $ bbox <$> pts


-----------------------
-- === Rendering === --
-----------------------

-- === DefMap === --

newtype DefMap = DefMap (Map Int Text) deriving (Show, Mempty)
makeLenses ''DefMap

registerDef :: MonadState DefMap m => Int -> Text -> m ()
registerDef idx d = modify_ @DefMap $ wrapped . at idx .~ Just d


-- === GeoMap === --

newtype GeoMap = GeoMap (Map Geo Int) deriving (Show, Mempty)
makeLenses ''GeoMap

registerGeo :: MonadState GeoMap m => Geo -> m Int
registerGeo g = do
  m <- unwrap <$> get @GeoMap
  case Map.lookup g m of
    Just i  -> return i
    Nothing -> do
      let idx = Map.size m
      put @GeoMap (wrap $ m & at g .~ Just idx)
      return idx


-- === MaskSet === --

newtype MaskSet = MaskSet (Set Int) deriving (Show, Mempty)
makeLenses ''MaskSet

registerMask :: MonadState MaskSet m => Int -> m ()
registerMask idx = modify_ @MaskSet $ wrapped %~ Set.insert idx


-- === Utils === --

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""


-- === Rendering === --

render :: Double -> Double -> Geo -> Text
render w h g = header <> defs <> main <> "</svg>" where
  defs          = renderDefs defMap maskSet
  main          = "<use href=\"#s" <> show' idx <> "\"/>"
  header        = "<svg width=\"" <> show' w <> "\" height=\"" <> show' h <> "\">"
  ((idx, maskSet), defMap)
                = flip (runState   @DefMap)  mempty
                $ flip (runStateT  @MaskSet) mempty
                $ flip (evalStateT @GeoMap)  mempty
                $ renderGeo g

renderDefs :: DefMap -> MaskSet -> Text
renderDefs defMap maskSet = "<defs>" <> shapeDefs <> maskDefs <> "</defs>" where
  mkShapeDef idx t = "<g id="    <> quoted ("s" <> show' idx) <> ">" <> t <> "</g>"
  mkMaskDef  idx   = "<mask id=" <> quoted ("m" <> show' idx) <> ">"
                  <> "<use href=\"#s" <> show' idx <> "\"" <> "/>"
                  <> "</mask>"
  shapeDefs  = mconcat $ uncurry mkShapeDef <$> Map.assocs (unwrap defMap)
  maskDefs   = mconcat $ mkMaskDef          <$> Set.elems  (unwrap maskSet)

renderGeo :: MonadStates '[MaskSet, GeoMap, DefMap] m => Geo -> m Int
renderGeo g@(Geo mat trans gd) = do
  idx <- registerGeo  g
  txt <- renderGeoDef (renderMat mat <> renderTrans trans) gd
  registerDef idx txt
  return idx


renderTrans :: Transform -> Text
renderTrans t@(Transform mx) = if t == mempty then "" else attrib where
  [a,c,e,b,d,f] = take 6 $ Matrix.toList mx
  call s        = " transform=\"matrix(" <> s <> ")\""
  attrib        = call . intercalate "," . fmap show' $ [a,b,c,d,e,f]

renderMat :: Material -> Text
renderMat = (" style=" <>) . quoted . intercalate ";" . fmap renderStyle

renderStyle :: Style -> Text
renderStyle = \case
  Fill        col -> "fill:"         <> col
  Stroke      col -> "stroke:"       <> col
  StrokeWidth a   -> "stroke-width:" <> show' a

-- FIXME: we should use XML library here instead of passing "rest" variable containing text that we want to put in the header
renderGeoDef :: MonadStates '[MaskSet, GeoMap, DefMap] m => Text -> GeoDef -> m Text
renderGeoDef rest g = case g of
  GeoEmpty      -> return mempty
  GeoCompound c -> case c of

    Merge a b -> do
      ida <- renderGeo a
      idb <- renderGeo b
      return $ "<g" <> rest <> ">"
                 <> "<use href=\"#s" <> show' ida <> "\"" <> "/>"
                 <> "<use href=\"#s" <> show' idb <> "\"" <> "/>"
            <> "</g>"

    Intersect a b -> do
      ida <- renderGeo a
      idb <- renderGeo (fill "white" b)
      registerMask idb
      return $ "<use href=\"#s" <> show' ida <> "\"" <> " mask=" <> quoted ("url(#m" <> show' idb <> ")") <> rest <> "/>"

    Subtract b a -> do
      ida <- renderGeo a
      idb <- renderGeo (fullBg + fill "black" b)
      registerMask idb
      return $ "<use href=\"#s" <> show' ida <> "\"" <> " mask=" <> quoted ("url(#m" <> show' idb <> ")") <> rest <> "/>"

-- M0 0 A 100 100 0 0 0 100 100 L 100 0
  GeoSimple s -> case s of
    -- Rect w h -> return $ "<g><rect width=" <> quoted (show' w) <> " height=" <> quoted(show' h) <> " transform=\"translate(" <> show' (-w/2) <> "," <> show' (-h/2) <> ")\" " <> "/>" <> rest <> "/>"
    Rect w h -> return $ "<g" <> rest <> ">"
                           <> "<rect width=" <> quoted (show' w) <> " height=" <> quoted(show' h) <> " transform=\"translate(" <> show' (-w/2) <> "," <> show' (-h/2) <> ")\" " <> "/>"
                         <> "</g>"
    Circle r -> return $ "<circle r=" <> quoted (show' r) <> rest <> "/>"
    Arc r a  -> return $ "<path d=" <> quoted ("M" <> show' (-r) <> " 0 A " <> show' r <> " " <> show' r <> " 0 " <> show' flag <> " 0 " <> show' nx <> " " <> show' ny <> "L 0 0 Z") <> rest <> "/>" where
        nx = - r * cos a
        ny = r * sin a
        flag = if a > pi then 1 else 0
    SPath pts -> renderGeoDef rest (GeoSimple . Path $ spath2Path pts)
    Path pts -> return $ "<path d=" <> quoted body <> rest <> "/>" where
      body = case pts of
        (p:ps) -> "M" <> show' (p ^. point.x) <> "," <> show' (p ^. point.y) <> " "
                      <> (intercalate " " $ fmap (uncurry renderPoint) $ zip (shift pts') pts') where pts' = ps <> [p]
        _      -> ""
      lastPt = unsafeLast pts

      renderPoint ppt pt = case (ppt ^. rightHandle, pt ^. leftHandle) of
          (Nothing, Nothing) -> "L" <> ending
          (Just h , Nothing) -> "Q" <> show' (h  ^. x) <> "," <> show' (h  ^. y) <> " " <> ending
          (Nothing, Just h ) -> "Q" <> show' (h  ^. x) <> "," <> show' (h  ^. y) <> " " <> ending
          (Just lh, Just rh) -> "C" <> show' (lh ^. x) <> "," <> show' (lh ^. y) <> " "
                                    <> show' (rh ^. x) <> "," <> show' (rh ^. y) <> " " <> ending
          where ending = show' (pt ^. point.x) <> "," <> show' (pt ^. point.y)
      shift :: [a] -> [a]
      shift = \case
        [] -> []
        s  -> unsafeLast s : unsafeInit s

_w = 2880
_h = 1800

fullBg = fill "white" $ rect (2*_w) (2*_h)

roundedRect :: Double -> Double -> Double -> Double -> Double -> Double -> Geo
roundedRect rtl rtr rbr rbl w h = path $ tl <> tr <> br <> bl where
  tl   = if rtl == 0 then [pt (-w2) (-h2)] else [ pt (-w2) (-(h2-rtl')), lpt (-w2) (-h2) (rtl' - w2) (-h2) ]
  tr   = if rtr == 0 then [pt w2    (-h2)] else [ pt (w2-rtr') (-h2)   , lpt w2 (-h2) w2 (-h2+rtr')        ]
  br   = if rbr == 0 then [pt w2    h2]    else [ pt w2 (h2-rbr')      , lpt w2 h2 (w2-rbr') h2            ]
  bl   = if rbl == 0 then [pt (-w2) h2]    else [ pt (rbl'-w2) h2      , lpt (-w2) h2 (-w2) (h2-rbl')      ]
  w2   = w/2
  h2   = h/2
  mwh  = min w h
  mwh2 = mwh / 2
  rtl' = min mwh2 rtl
  rtr' = min mwh2 rtr
  rbr' = min mwh2 rbr
  rbl' = min mwh2 rbl


sw = 3
baseColor  = "rgb(160,160,160)"
baseColor2 = "rgb(170,170,170)"
baseColor3 = "rgb(180,180,180)"

-- main :: IO ()
-- main = do
--   let ball    = circle 5
--       border rt rb = roundedRect rt rt rb rb 128 32
--       dot     = rect 3 3
--       dotLine = mconcat $ (\i -> move (i*10) 0 dot) <$> [0..5]
--       dots    = dotLine + move 5 6 dotLine
--
--       s1 rt rb = fill "none" $ strokeWidth sw $ strokeColor baseColor $ move (sw/2) (sw/2) $ border rt rb + move 18 15.5 ball
--       s2       = move 50 13 $ fill baseColor dots
--       segment rt rb = s1 rt rb + s2
--       icon = segment 10 0 + move 0 32 (segment 0 0) + move 0 64 (segment 0 10)
--       -- c  = circle 20 - circle 18
--       -- r3 = strokeWidth 10 $ strokeColor "#ff0000" $ path [pt 0 0, lpt 10 50 0 100]
--       -- svg = render _w _h (r1 - r2)
--       svg = render _w _h $ move 12 12 icon
--
--
--   print svg
--   Text.writeFile "/tmp/test.svg"  svg
--   Text.writeFile "/tmp/test.html" (mkView svg)
--   print "hello"

duplicate i x y s = mconcat $ (\j -> move (j*x) (j*y) s) <$> [0..i]

bgColor = "#141210"

databaseIcon :: Geo
databaseIcon = seg baseColor 10 0 + move 0 32 (seg baseColor2 0 0) + move 0 64 (seg baseColor3 0 10) where
  ball         = circle 6
  dot          = square 4
  dotLine      = duplicate 5 10 0 dot
  dots         = dotLine + move 5 6 dotLine
  seg  c rt rb = back c rt rb + front
  back c rt rb = fill c $ roundedRect rt rt rb rb 128 29
  front        = move 18 15 (fill bgColor ball)
               + move 50 10 (fill bgColor dots)

panelColor = "#161514"
tabHeight = 60

-- 2880 x 1600
navPanel :: Geo
navPanel = fill panelColor $ alignTopLeft $ rect 400 _h

graphPanel :: Geo
graphPanel = fill panelColor $ bg + tb where
    bg = move 0 tabHeight $ alignTopLeft $ rect 2480 (_h - tabHeight)
    tb = tab 300

vsep :: Double -> Geo
vsep x = move x 0 $ alignTopLeft $ mconcat
         [ fill "#000000" (rect 4 _h)
         , fill "rgba(255,255,255,0.03)" (move 4 0 $ rect 2 _h)
         ]

tabBar :: Double -> Geo
tabBar w = fill "#000000" $ rect w 60

tab :: Double -> Geo
tab w = fill panelColor $ path [pt 0 0, rpt w 0 (w + r) 0, lpt (w + slope - r) tabHeight (w + slope) tabHeight, pt 0 tabHeight] where
    r     = 70
    slope = 100

portWidth :: Double
portWidth = 6

gridElemHeight :: Double
gridElemHeight = 40

compactNode :: [String] -> [String] -> Geo
compactNode ins outs = ports ins + rotate 180 (ports outs) where
    portShape s = rotate (90) $ arc rad (pi / s) - circle (rad - width) - span - rotateRad (-pi / s) span
    port n s   = fill ("rgb(" <> convert (show $ round r) <> "," <> convert (show $ round g) <> "," <> convert (show $ round b) <> ")") $ portShape s where
        Color.RGB r g b _ = Color.lch2rgb $ buildLCH (Color $ hash n)
    ports ns  = mconcat $ (\(n,i) -> rotate (-i * 180 / num) $ port n num) <$> zip ns [0 .. num - 1] where num = convert (length ns)
    span    = rect (2 * rad + 10) off
    width   = portWidth
    rad     = 50
    off     = 4

normalNode :: [String] -> [String] -> Geo
normalNode ins outs = fill "rgba(255,255,255,0.05)" $ (move 150 200 $ roundedRect 30 30 30 30 300 400) + move 0 30 (ports ins) where
    portShape = alignTopRight $ rect portWidth gridElemHeight
    port n    = fill ("rgb(" <> convert (show $ round r) <> "," <> convert (show $ round g) <> "," <> convert (show $ round b) <> ")") $ portShape where
        Color.RGB r g b _ = Color.lch2rgb $ buildLCH (Color $ hash n)
    ports ns  = mconcat $ (\(n,i) -> move 0 (i * (gridElemHeight + 4)) $ port n) <$> zip ns [0 .. num - 1] where num = convert (length ns)

arrow :: Point -> Point -> Geo
arrow p p' = rotateRad (angle p p') $ linkLine len + (move len 0 arrowHead) where
    len    = dist p p'

arrowHead :: Geo
arrowHead = path [pt 0 headW, pt headL 0, pt 0 (-headW)] where
    headW  = 8
    headL  = 10

link :: Point -> Point -> Geo
link p p' = rotateRad (angle p p') $ linkLine len + move (len / 2) 0 arrowHead where
    len   = dist p p'

linkLine :: Double -> Geo
linkLine len = alignLeft $ rect len aWidth where
    aWidth = 4






-- graph :: Map Int


data NodeType = CompactNode | NormalNode deriving (Show)

data Node = Node { _pos     :: Point
                 , _inputs  :: [String]
                 , _outputs :: [String]
                 , _panels  :: NodeType
                 }

renderNode :: Node -> Geo
renderNode (Node pos ins outs pans) = move (pos ^. x) (pos ^. y) $ case pans of
    CompactNode -> compactNode ins outs
    NormalNode  -> normalNode  ins outs


newtype Color = Color { fromColor :: Int } deriving (Eq, Generic, Ord, Show)

colorC :: Int -> Float
colorC _ = 45

colorL :: Int -> Float
colorL _ = 30

colorH :: Int -> Int
colorH _ = 100

buildLCH :: Color -> Color.LCH
buildLCH (Color 0) = Color.LCH 50  0 0 255
buildLCH (Color i) = Color.LCH (colorL i) (colorC i) (convert h') 255 where
    h'    = (colorH i + (i - 1)) `mod'` 360


main :: IO ()
main = do
  let nodes = [ Node (Point 600 600) ["Int", "String", "Vector"] ["Int"] CompactNode
              , Node (Point 800 600) ["Int"] ["Int"] CompactNode
              , Node (Point 950 500) ["Int", "String", "Vector"] ["Int"] NormalNode
              ]
  let svg = render _w _h
        --   $ fill "#ff0000" $ mconcat (renderNode <$> nodes)
        --   $ mconcat [navPanel, move 400 0 graphPanel, vsep 400]
          $ mconcat [navPanel, move 400 0 graphPanel, vsep 400
                    , fill "#ff0000" $ move 650 600 $ link (Point 0 0) (Point 100 0)
                    ] <> mconcat (renderNode <$> nodes)

  print $ Color.lch2rgb $ buildLCH (Color 7)
  print svg
  Text.writeFile "/tmp/test.svg"  svg
  Text.writeFile "/tmp/test.html" (mkView svg)
  print "hello"


mkView s = "<!DOCTYPE html>"
        <> "<html>"
        <> "<body id=\"body\" style=\"zoom:1; margin:0; background-color:#000000\">"
        <> s
        <> "</body></html>"

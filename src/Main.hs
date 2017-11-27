{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prologue hiding (Simple, subtract, point, duplicate)
import qualified "containers" Data.Map.Strict as Map
import           "containers" Data.Map.Strict (Map)
import qualified "containers" Data.Set        as Set
import           "containers" Data.Set        (Set)
import           Control.Monad.State.Layered
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import qualified Data.Matrix  as Matrix
import           Data.Matrix  (Matrix(..))

import qualified Data.Color as Color
import           Data.Fixed              (mod')
import           Data.Hashable (Hashable, hash)

import qualified Shelly as Shelly

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

instance Bounding ControlPoint where
    bbox (ControlPoint _ _ p) = bbox p


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

data BBox = BBox Point Double Double
          | BBoxNull
          deriving (Show)

class Bounding a where
    bbox :: a -> BBox

bbcat :: BBox -> BBox -> BBox
bbcat a b = case (a,b) of
    (BBoxNull, a) -> a
    (a, BBoxNull) -> a
    (BBox (Point x y) w h, BBox (Point x' y') w' h') -> BBox (Point left top) (right - left) (bottom - top) where
        left   = min x x'
        top    = min y y'
        right  = max (x + w) (x' + w')
        bottom = max (y + h) (y' + h')

instance Mempty    BBox where mempty = BBoxNull
instance Semigroup BBox where (<>)   = bbcat

instance Bounding a => Bounding (Boolean a) where
    bbox = \case
        Subtract  a b -> bbcat (bbox a) (bbox b)
        Intersect a b -> bbcat (bbox a) (bbox b)
        Merge     a b -> bbcat (bbox a) (bbox b)

instance Bounding Point where
    bbox p = BBox p 0 0



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
  | Text   Text
  deriving (Eq, Ord, Show)

data Hinting
  = None
  | SnapPixel
  deriving (Eq, Ord, Show)

data TextAnchor
  = AnchorStart
  | AnchorMiddle
  | AnchorEnd
  deriving (Eq, Ord, Show)

data Style
  = Fill        Text
  | Opacity     Double
  | Stroke      Text
  | StrokeWidth Double
  | Hinting     Hinting
  | FontFamily  Text
  | FontSize    Int
  | FontWeight  Int
  | TextAnchor  TextAnchor
  | CSS         Text Text
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

text :: Text -> Geo
text = Geo mempty mempty . GeoSimple . Text

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

opacity :: Double -> Geo -> Geo
opacity a = material %~ (Opacity a:)

strokeWidth :: Double -> Geo -> Geo
strokeColor :: Text   -> Geo -> Geo
strokeWidth a = material %~ (StrokeWidth a:)
strokeColor t = material %~ (Stroke t:)

textAnchor :: TextAnchor -> Geo -> Geo
textAnchor a = material %~ (TextAnchor a:)

textAnchorStart, textAnchorMiddle, textAnchorEnd :: Geo -> Geo
textAnchorStart  = textAnchor AnchorStart
textAnchorMiddle = textAnchor AnchorMiddle
textAnchorEnd    = textAnchor AnchorEnd

css :: Text -> Text -> Geo -> Geo
css k v = material %~ (CSS k v:)

textVAlignMiddle :: Geo -> Geo
textVAlignMiddle = css "alignment-baseline" "middle"

fontFamily :: Text -> Geo -> Geo
fontFamily f = material %~ (FontFamily f:)

fontSize :: Int -> Geo -> Geo
fontSize s = material %~ (FontSize s:)

fontWeight :: Int -> Geo -> Geo
fontWeight s = material %~ (FontWeight s:)


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
    Circle r   -> BBox (Point (-r) (-r)) r r
    Arc    r _ -> BBox (Point (-r) (-r)) r r
    Path   pts -> mconcat $ bbox <$> pts
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
  Opacity     a   -> "opacity:"      <> show' a
  Stroke      col -> "stroke:"       <> col
  StrokeWidth a   -> "stroke-width:" <> show' a
  FontFamily  f   -> "font-family:"  <> f
  FontSize    s   -> "font-size:"    <> show' s <> "px"
  FontWeight  w   -> "font-weight:"  <> show' w
  TextAnchor  a   -> "text-anchor:"  <> a' where
      a' = case a of
          AnchorStart  -> "start"
          AnchorMiddle -> "middle"
          AnchorEnd    -> "end"
  CSS         k v -> k <> ":" <> v


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
    Text t -> return $ "<text" <> rest <> ">" <> t <> "</text>"

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

-- bgColor = "#141210"
bgColor = "#201E1A"

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

-- panelColor = "#161514"
-- panelColor = "#201E1A"
panelColor = "hsl(40,8%,9%)"

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
portWidth = 4

gridElemHeight, gridElemHeight2 :: Double
gridElemOffset :: Double
gridElemHeight  = 30
gridElemHeight2 = gridElemHeight / 2
gridElemOffset  = 6

typeColor :: Text -> Text
typeColor s = "rgb(" <> convert (show $ round r) <> "," <> convert (show $ round g) <> "," <> convert (show $ round b) <> ")" where
    Color.RGB r g b _ = Color.lch2rgb $ buildLCH (Color $ 100 + hash (convert s :: String)) -- FIXME: why we need string conversion here?

compactNode :: [Port] -> [Port] -> Geo
compactNode ins outs = ports ins + rotate 180 (ports outs) where
    portShape s = rotate (90) $ arc rad (pi / s) - circle (rad - width) - span - rotateRad (-pi / s) span
    port n s   = fill (typeColor n) $ portShape s
    ports ps  = mconcat $ (\(Port _ t _,i) -> rotate (-i * 180 / num) $ port t num) <$> zip ps [0 .. num - 1] where num = convert (length ps)
    span    = rect (2 * rad + 10) off
    width   = portWidth
    rad     = 30
    off     = gridElemOffset

circleShadow :: Int -> Int -> Geo
circleShadow small big = mconcat $ (fill "rgba(0,0,0,0.02)" . circle . convert) <$> reverse [small .. big]
--

-- === Compact node === --

compactNodeRadius :: Double
compactNodeRadius = 20

nodeBorder :: Double
nodeBorder = compactNodeRadius - selfPortRadius

compactNodeBg :: Text
compactNodeBg  = "rgba(255,255,255,0.16)"
compactNodeBg2 = "rgba(255,255,255,0.06)"

arrowOffset :: Double
arrowOffset = gridElemOffset + 2

compactNode2 :: Maybe BasePort -> [Port] -> [Port] -> [Mod] -> Geo
compactNode2 base ins outs mods = selection + body + inPorts + outPorts + nextPort + expr where
    expr          = move 0 (-rad - 16) $ exprLabel "outArrow"
    inPorts       = ports True  ins
    outPorts      = ports False outs
    body          = fill refcolor (circle rad) + selfPort mt
    inArrow       = move (-roff) 0 inPortHead * circle 40
    outArrow      = move roff 0 outPortHead
    outPortShape  = outArrow - circle roff
    inPortShape n = (if Hovered `elem` mods then (+ txt) else id) inArrow where
        txt = move arrOff 5 (portInLabelBase n)
    port  b n t   = fill col $ if b then inPortShape n else outPortShape where
        col = if Hovered `elem` mods then typeColor t else compactNodeBg2
    ports b ps    = rotate rootAngle $ mconcat $ (\(Port n t _,i) -> rotate (-i * baseAngle) $ port b n t) <$> zip ps [0 .. num - 1] where
        num       = convert (length ps)
        baseAngle = 180 / (num + 1)
        rootAngle = (num - 1) * baseAngle / 2
    nextPort      = fill compactNodeBg2 $ rotate (-90) $ inPortShape ""
    (selfColor, refcolor) = case base of
        Nothing           -> ("rgba(255,255,255,0.2)",compactNodeBg2)
        Just (SelfPort t) -> (typeColor "Int", "rgba(255,255,255,0.1)")
        Just (RefPort  t) -> ("rgba(255,255,255,0.1)", typeColor "Int")
    mt = case base of
        Just (SelfPort t) -> Just t
        _                 -> Nothing
    selection     = if Selected `elem` mods then fill "hsla(50, 100%, 60%, 0.2)" $ circle 60 - circle 50 else mempty

    rad     = 20
    roff    = rad + arrowOffset
    arrOff  = (-rad - arrowOffset*2 - 14)


-- === Self port === --

selfPortRadius :: Double
selfPortRadius = 10

selfPort :: Maybe Text -> Geo
selfPort mt = circleShadow (round $ selfPortRadius) (round $ selfPortRadius + shadowRad) - circle selfPortRadius + fill selfColor (circle selfPortRadius) where
    shadowRad = 4
    selfColor = case mt of
        Nothing -> "rgba(255,255,255,0.2)"
        Just t  -> typeColor t


label :: Text -> Geo
label = fill "rgba(255,255,255,0.3)" . textAnchorEnd . fontWeight 600 . fontSize 17 . fontFamily "Helvetica Neue" . text


valueLabelBase :: Text -> Geo
valueLabelBase = textVAlignMiddle . fontWeight 600 . fontSize 17 . fontFamily "Helvetica Neue" . text

valueLabel :: Text -> Geo
valueLabel = move 0 gridElemHeight2 . fill "rgba(255,255,255,0.4)" . valueLabelBase

valueNumLabel :: Text -> Geo
valueNumLabel = move 0 1 . valueLabel


exprLabel :: Text -> Geo
exprLabel = fill "rgba(255,255,255,0.3)" . textAnchorMiddle . fontWeight 600 . fontSize 17 . fontFamily "Helvetica Neue" . text

portInLabelBase :: Text -> Geo
portInLabelBase = textAnchorEnd . portLabelBase'

portOutLabelBase :: Text -> Geo
portOutLabelBase = textAnchorStart . portLabelBase'

portLabelBase :: Bool -> Text -> Geo
portLabelBase b = if b then portInLabelBase else portOutLabelBase

portLabelBase' :: Text -> Geo
portLabelBase' = fontWeight 800 . fontSize 17 . fontFamily "Helvetica Neue" . text


normalNode2 :: [Port] -> [Port] -> [Mod] -> Geo
normalNode2 ins outs mods = body + header + selfPort Nothing where
    header     = fill panelColor (circle compactNodeRadius) + opacity 0.06 (fill "white" headerNeck)
    headerNeck = circle compactNodeRadius
               + (move (-compactNodeRadius) 0 $ alignTopLeft $ rect (3*compactNodeRadius + bodyOffset) (compactNodeRadius + bodyOffset))
               - move (2*compactNodeRadius + bodyOffset) 0 (circle $ compactNodeRadius + bodyOffset)
    body       = move (-compactNodeRadius) (compactNodeRadius + bodyOffset) $ bodyBg + move 0 off ports
    bodyBg     = fill compactNodeBg2 $ (alignTopLeft $ roundedRect 0 r r r 300 height) where r = compactNodeRadius
    ports      = mkPorts ins
    bodyOffset = arrowOffset
    portShape  = move (-off) gridElemHeight2 $ alignRight arrowHead
    port n t (SingleVal v) = fill (typeColor t) (portShape + lab) + move off 0 (widget v) where
        widget = fromJust (const mempty) $ Map.lookup t widgets
        lab    = if Hovered `elem` mods then move (-2*off - 10) (gridElemHeight2 + 5) (portInLabelBase n) else mempty
    mkPorts ps  = mconcat $ (\(Port n t v,i) -> move 0 (i * (gridElemHeight + gridElemOffset)) $ port n t v) <$> zip ps [0 .. num - 1] where num = convert (length ps)
    portsHeight = convert (max (length ins) (length outs)) * (gridElemHeight + gridElemOffset) - gridElemOffset + 2 * off
    height      = portsHeight
    off         = 10


extendPlaces :: Double -> Int
extendPlaces i = if mod' i 1 == 0 then floor i else extendPlaces (10 * i)

slider :: Double -> Double -> Geo
slider width s = body + val + txt (show' ipart) (show' fpart) where
    txt s t = move (width/2) 0
            $ move (-dotoff) 0 (textAnchorEnd    $ valueNumLabel s)
            + move   dotoff  0 (textAnchorStart  $ valueNumLabel t)
            +                  (textAnchorMiddle $ valueNumLabel ".")
    ipart   = floor s
    fpart   = extendPlaces (s - fromIntegral ipart)
    val     = fill layerVal $ alignTopLeft $ roundedRect gridElemHeight2 0 0 gridElemHeight2 (width * p) gridElemHeight
    body    = fill layerBg  $ alignTopLeft $ roundedRect gridElemHeight2 gridElemHeight2 gridElemHeight2 gridElemHeight2 width gridElemHeight
    dotoff  = 5
    p       = s / 10 ^^ (ceiling $ logBase 10 s)


toggle :: Bool -> Geo
toggle t = body + val where
    val   = fill valColor $ alignTopLeft $ move off off $ circle (gridElemHeight2 - off)
    body  = fill layerBg  $ alignTopLeft $ roundedRect gridElemHeight2 gridElemHeight2 gridElemHeight2 gridElemHeight2 width gridElemHeight
    width = gridElemHeight * 2
    off   = 6

textField :: Double -> Text -> Geo
textField width t = body + txt where
    txt   = move 8 0 $ valueLabel t
    body  = fill layerBg  $ alignTopLeft $ roundedRect gridElemHeight2 gridElemHeight2 gridElemHeight2 gridElemHeight2 width gridElemHeight


layerBg  = "rgba(255,255,255,0.06)"
layerVal = "rgba(255,255,255,0.08)"
valColor = "rgba(255,255,255,0.14)"

-- widgets :: Map Text Geo
-- widgets = Map.insert "Int"  (slider 280 0.3)
--         $ Map.insert "Bool" (toggle True)
--         $ mempty

widgets :: Map Text (Text -> Geo)
widgets = Map.insert "Number" (slider 280 . unsafeRead . convert)
        $ Map.insert "Bool"   (toggle . unsafeRead . convert)
        $ Map.insert "Text"   (textField 280)
        $ mempty

arrow :: Point -> Point -> Geo
arrow p p' = rotateRad (angle p p') $ linkLine len + (move len 0 arrowHead) where
    len    = dist p p'

arrowHead :: Geo
arrowHead = path [pt 0 headW, pt headL 0, pt 0 (-headW)] where
    headW  = 8
    headL  = 10

arrowHead2 :: Geo
arrowHead2 = path [pt 0 headW, pt headL 0, pt 0 (-headW)] where
    headW  = 10
    headL  = 14


inPortHead, outPortHead :: Geo
inPortHead  = move (-14) 0 arrowHead2
outPortHead = move (-3) 0 arrowHead2

link :: Point -> Point -> Geo
link p p' = rotateRad (angle p p') $ linkLine len + move (len / 2) 0 arrowHead where
    len   = dist p p'

linkLine :: Double -> Geo
linkLine len = alignLeft $ rect len aWidth where
    aWidth = 4






-- graph :: Map Int

data Mod
  = Hovered
  | Selected
  deriving (Show, Eq)

data NodeType
  = CompactNode
  | CompactNode2
  | NormalNode2
  deriving (Show)

data Node = Node { _pos     :: Point
                 , _base    :: Maybe BasePort
                 , _inputs  :: [Port]
                 , _outputs :: [Port]
                 , _panels  :: NodeType
                 , _mods    :: [Mod]
                 }

data BasePort = SelfPort Text
              | RefPort  Text
              deriving (Show)

data PortVal
  = SingleVal Text
  | MultiVal  [Port]

data Port = Port { _name :: Text
                 , _tp   :: Text
                 , _val  :: PortVal
                 }

renderNode :: Node -> Geo
renderNode (Node pos base ins outs pans mods) = move (pos ^. x) (pos ^. y) $ case pans of
    CompactNode  -> compactNode  ins outs
    CompactNode2 -> compactNode2 base ins outs mods
    NormalNode2  -> normalNode2       ins outs mods


newtype Color = Color { fromColor :: Int } deriving (Eq, Generic, Ord, Show)

colorC :: Int -> Float
colorC _ = 40

colorL :: Int -> Float
colorL _ = 35 -- 30

colorH :: Int -> Int
colorH _ = 100

buildLCH :: Color -> Color.LCH
buildLCH (Color 0) = Color.LCH 50  0 0 255
buildLCH (Color i) = Color.LCH (colorL i) (colorC i) (convert h') 255 where
    h'    = 190 + (colorH i + (i * 3 - 1)) `mod'` 360


main :: IO ()
main = do
  let nodes = [ Node (Point 600 600) Nothing
                    [ Port "blur"     "Number" (SingleVal "0.3")
                    , Port "name"     "Text"   (SingleVal "name")
                    , Port "position" "Vector" (SingleVal "")
                    ]
                    [ Port "out" "Number" (SingleVal "0")]
                    CompactNode  []
              , Node (Point 800 600) Nothing
                    [ Port "in" "Number"  (SingleVal "0") ]
                    [ Port "out" "Number" (SingleVal "0") ]
                    CompactNode  []
              , Node (Point 750 800) Nothing
                    [ Port "blur"    "Number" (SingleVal "0.3")
                    , Port "name"    "Text"   (SingleVal "name")
                    , Port "enabled" "Bool"   (SingleVal "True")
                    ]
                    [ Port "out" "Number" (SingleVal "0")]
                    CompactNode2 [Hovered]
              , Node (Point 750 1000) Nothing
                    []
                    [ Port "out" "Number" (SingleVal "0")]
                    CompactNode2 [Hovered]
              , Node (Point 950 800) Nothing
                    [ Port "blur"     "Number" (SingleVal "0.7")
                    , Port "name"     "Text"   (SingleVal "name")
                    , Port "enabled"  "Bool"   (SingleVal "True")
                    , Port "position" "Vector" (SingleVal "x") ]
                    [ Port "out"      "Int"    (SingleVal "0") ]
                    NormalNode2  [Hovered]
              ]

  let svg = render _w _h
        --   $ strokeWidth 4 $ strokeColor "#ff0000" $ rect 100 100
        --   $ fill "#ff0000" $ mconcat (renderNode <$> nodes)
        --   $ mconcat [navPanel, move 400 0 graphPanel, vsep 400]
          $ mconcat [navPanel, move 400 0 graphPanel, vsep 400
                    , move 630 600 $ fill (typeColor "Int") $ link (Point 0 0) (Point 140 0)
                    ] <> mconcat (renderNode <$> nodes)
        --   $ opacity 0.2 $ fill "rgb(255,255,255)" (circle 50 + rect 30 100)
  print $ Color.lch2rgb $ buildLCH (Color 7)
  print svg
  Text.writeFile "/tmp/test.svg"  svg
  Text.writeFile "/tmp/test.html" (mkView svg)
  Shelly.shelly $ Shelly.run "google-chrome-stable" ["/tmp/test.html"]
  return ()


mkView s = "<!DOCTYPE html>"
        <> "<html>"
        <> "<body id=\"body\" style=\"zoom:1; margin:0; background-color:#000000\">"
        <> s
        <> "</body></html>"

module Component.SVG where

import Halogen.HTML (ElemName(..), HTML, Namespace(..), PropName(..))
import Halogen.HTML.Elements (elementNS)
import Halogen.HTML.Properties (IProp, prop)

svgns :: String
svgns = "http://www.w3.org/2000/svg"

svgNamespace :: Namespace
svgNamespace = Namespace svgns

svgElement :: forall r p i. ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
svgElement = elementNS svgNamespace

svg :: forall r p i. Array (IProp r i) -> HTML p i
svg props = svgElement (ElemName "svg") props []

circle :: forall r p i. Array (IProp r i) -> HTML p i
circle props = svgElement (ElemName "circle") props []

line :: forall r p i. Array (IProp r i) -> HTML p i
line props = svgElement (ElemName "line") props []

cx :: forall r i. Int -> IProp (cx :: Int | r) i
cx = prop (PropName "cx")

cy :: forall r i. Int -> IProp (cy :: Int | r) i
cy = prop (PropName "cy")

r :: forall rty i. Int -> IProp (r :: Int | rty) i
r = prop (PropName "r")

x1 :: forall r i. Int -> IProp (x1 :: Int | r) i
x1 = prop (PropName "x1")

y1 :: forall r i. Int -> IProp (y1 :: Int | r) i
y1 = prop (PropName "y1")

x2 :: forall r i. Int -> IProp (x2 :: Int | r) i
x2 = prop (PropName "x2")

y2 :: forall r i. Int -> IProp (y2 :: Int | r) i
y2 = prop (PropName "y2")

stroke :: forall r i. String -> IProp (stroke :: String | r) i
stroke = prop (PropName "stroke")

fill :: forall r i. String -> IProp (fill :: String | r) i
fill = prop (PropName "fill")

module Component.Common.SVG where

import Halogen.HTML (AttrName(..), ElemName(..), HTML, Namespace(..))
import Halogen.HTML.Elements (elementNS)
import Halogen.HTML.Properties (IProp, attr)
import Prelude (show, (<<<))

svgns :: String
svgns = "http://www.w3.org/2000/svg"

svgNamespace :: Namespace
svgNamespace = Namespace svgns

svgElement :: forall r p i. ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
svgElement = elementNS svgNamespace

svg :: forall r p i. Array (IProp r i) -> Array (HTML p i) -> HTML p i
svg = svgElement (ElemName "svg")

text :: forall r p i. Array (IProp r i) -> Array (HTML p i) -> HTML p i
text = svgElement (ElemName "text")

circle :: forall r p i. Array (IProp r i) -> HTML p i
circle props = svgElement (ElemName "circle") props []

rect :: forall r p i. Array (IProp r i) -> HTML p i
rect props = svgElement (ElemName "rect") props []

line :: forall r p i. Array (IProp r i) -> HTML p i
line props = svgElement (ElemName "line") props []

width :: forall r i. Int -> IProp (width :: Int | r) i
width = attr (AttrName "width") <<< show

strokeWidth :: forall r i. Int -> IProp (strokeWidth :: Int | r) i
strokeWidth = attr (AttrName "stroke-width") <<< show

height :: forall r i. Int -> IProp (height :: Int | r) i
height = attr (AttrName "height") <<< show

viewBox :: forall r i. String -> IProp (viewBox :: String | r) i
viewBox = attr (AttrName "viewBox")

x :: forall r i. Int -> IProp (x :: Int | r) i
x = attr (AttrName "x") <<< show

y :: forall r i. Int -> IProp (y :: Int | r) i
y = attr (AttrName "y") <<< show

cx :: forall r i. Int -> IProp (cx :: Int | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Int -> IProp (cy :: Int | r) i
cy = attr (AttrName "cy") <<< show

r :: forall rty i. Int -> IProp (r :: Int | rty) i
r = attr (AttrName "r") <<< show

x1 :: forall r i. Int -> IProp (x1 :: Int | r) i
x1 = attr (AttrName "x1") <<< show

y1 :: forall r i. Int -> IProp (y1 :: Int | r) i
y1 = attr (AttrName "y1") <<< show

x2 :: forall r i. Int -> IProp (x2 :: Int | r) i
x2 = attr (AttrName "x2") <<< show

y2 :: forall r i. Int -> IProp (y2 :: Int | r) i
y2 = attr (AttrName "y2") <<< show

stroke :: forall r i. String -> IProp (stroke :: String | r) i
stroke = attr (AttrName "stroke")

fill :: forall r i. String -> IProp (fill :: String | r) i
fill = attr (AttrName "fill")

class_ :: forall r i. String -> IProp (class_ :: String | r) i
class_ = attr (AttrName "class")

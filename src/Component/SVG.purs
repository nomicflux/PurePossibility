module Component.SVG where

import Data.Function (($))
import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (HTML, ElemName(ElemName), PropName(..), AttrName(..))
import Halogen.HTML.Elements (Node, Leaf)
import Halogen.HTML.Properties (I, IProp, prop)

import Halogen.VDom as VDom
import Halogen.VDom.DOM.Prop (Prop)

import Unsafe.Coerce (unsafeCoerce)

svgns :: String
svgns = "http://www.w3.org/2000/svg"

-- | A smart constructor for SVG elements.
celement :: forall p i. VDom.ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
celement = coe (\name props children -> VDom.Elem (VDom.ElemSpec ns name props) children)
  where
    coe :: (VDom.ElemName -> Array (Prop i) -> Array (VDom.VDom (Array (Prop i)) p) -> VDom.VDom (Array (Prop i)) p)
           -> VDom.ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
    coe = unsafeCoerce
    ns :: Maybe VDom.Namespace
    ns = Just $ VDom.Namespace svgns

-- | Creates an SVG element that expects indexed properties.
element :: forall r p i. ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
element = coe celement
  where
    coe :: (ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i)
           -> ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
    coe = unsafeCoerce

svg :: forall p i. Node (viewBox :: I, height :: I, width :: I) p i
svg = element (ElemName "svg")

circle :: forall p i. Leaf (cx :: I, cy :: I, r :: I) p i
circle props = element (ElemName "circle") props []

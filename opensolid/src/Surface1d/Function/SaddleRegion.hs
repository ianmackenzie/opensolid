module Surface1d.Function.SaddleRegion
  ( SaddleRegion (..)
  , Frame
  )
where

import Curve2d (Curve2d)
import Frame2d (Frame2d)
import OpenSolid
import Text qualified
import Uv qualified
import Prelude qualified

type Frame = Frame2d Uv.Coordinates (Defines Uv.Space)

data SaddleRegion = SaddleRegion
  { point :: Uv.Point
  , connectingCurves :: Uv.Point -> List (Curve2d Uv.Coordinates)
  }

instance Show SaddleRegion where
  showsPrec precedence (SaddleRegion{point}) =
    Prelude.showParen (precedence > 10) $
      ( Prelude.showString (Text.unpack "SaddleRegion ")
          . Prelude.showsPrec (precedence + 1) point
          . Prelude.showString (Text.unpack " <function>")
      )

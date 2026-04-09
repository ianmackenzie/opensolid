module Main (main) where

import Edge2D qualified
import Edge3D (Edge3D (..))
import Edge3D qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Ap242 qualified as Ap242
import OpenSolid.Ap242.Surface qualified as Ap242.Surface
import OpenSolid.Arc2D qualified as Arc2D
import OpenSolid.Axis3D (Axis3D (Axis3D))
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Length qualified as Length
import OpenSolid.Line2D (pattern Line2D)
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.Line3D (pattern Line3D)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Step qualified as Step
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D
import Path2D qualified
import Path3D qualified

faceBoundEntity :: Tolerance Meters => NonEmpty (Edge3D space) -> Step.Entity
faceBoundEntity loop =
  Step.entity "FACE_BOUND" $
    [ Step.text ""
    , Step.referenceTo $
        Step.entity "EDGE_LOOP" $
          [ Step.text ""
          , Step.list (Step.referenceTo . Edge3D.entity) (NonEmpty.toList loop)
          ]
    , Step.bool True
    ]

advancedFaceEntity :: Tolerance Meters => NonEmpty (Edge3D space) -> Step.Entity -> Step.Entity
advancedFaceEntity loop surface =
  Step.entity "ADVANCED_FACE" $
    [ Step.text ""
    , Step.list Step.referenceTo [faceBoundEntity loop]
    , Step.referenceTo surface
    , Step.bool True
    ]

main :: IO ()
main = Tolerance.using Length.defaultTolerance do
  let rLarge = Length.centimeters 1.0
  let rSmall = Length.centimeters 0.5
  let thickness = Length.millimeters 3.0
  let width = Length.centimeters 5.0

  let baseLine = Line2D Point2D.origin (Point2D.centimeters 5.0 0.0)
  profilePath <-
    Ok (NonEmpty.one (Edge2D.Line baseLine))
      & Result.andThen (Path2D.addArc rLarge (Angle.degrees 100.0))
      & Result.andThen (Path2D.addLine (Length.centimeters 5.0))
      & Result.andThen (Path2D.addArc rLarge (Angle.degrees 150.0))
      & Result.andThen (Path2D.addLine (Length.centimeters 3.0))
      & Result.andThen (Path2D.addArc rSmall (Angle.degrees -90.0))
      & Result.andThen (Path2D.addLine (Length.centimeters 1.0))
      & Result.andThen (Path2D.addArc rSmall (Angle.degrees -90.0))
      & Result.andThen (Path2D.addLine (Length.centimeters 1.0))
      & Result.orFail

  profile2D <- Path2D.thickenLeftwardBy thickness profilePath & Result.orFail

  let midPlane = World3D.rightPlane
  let rightPlane = midPlane & Plane3D.offsetBy (0.5 * width)
  let rightProfile = Path3D.on rightPlane profile2D
  let leftPlane = rightPlane & Plane3D.mirrorAcross midPlane
  let leftProfile = Path3D.on leftPlane profile2D & Path3D.reverse
  let rightSurface = Ap242.Surface.plane3D rightPlane
  let leftSurface = Ap242.Surface.plane3D (Plane3D.flip leftPlane)
  let leftFace = advancedFaceEntity leftProfile leftSurface
  let rightFace = advancedFaceEntity rightProfile rightSurface

  let extrudedSurface edge2D = case edge2D of
        Edge2D.Line line2D -> do
          lineDirection <- Line2D.direction line2D
          let normalDirection = Direction3D.on midPlane (Direction2D.rotateRight lineDirection)
          let originPoint = Point3D.on midPlane (Line2D.startPoint line2D)
          let plane = Plane3D.fromPointAndNormal originPoint normalDirection
          Ok (Ap242.Surface.plane3D plane)
        Edge2D.Arc arc2D -> do
          let originPoint = Point3D.on midPlane (Arc2D.centerPoint arc2D)
          let cylinderAxis = Axis3D originPoint (Plane3D.normalDirection midPlane)
          Ok (Ap242.Surface.cylinder3D cylinderAxis (#radius (Arc2D.radius arc2D)))

  let extrudedFace (edge, nextEdge) = do
        surface <- extrudedSurface edge
        let leftEdge = Edge3D.on leftPlane edge
        let rightEdge = Edge3D.on rightPlane edge & Edge3D.reverse
        let startPoint = Edge2D.startPoint edge
        let endPoint = Edge2D.startPoint nextEdge
        let startLine = Line3D (Point3D.on rightPlane startPoint) (Point3D.on leftPlane startPoint)
        let endLine = Line3D (Point3D.on rightPlane endPoint) (Point3D.on leftPlane endPoint)
        let startEdge = Edge3D.line startLine
        let endEdge = Edge3D.line endLine & Edge3D.reverse
        let loop = NonEmpty.four leftEdge endEdge rightEdge startEdge
        Ok (advancedFaceEntity loop surface)

  extrudedFaces <- Result.collect extrudedFace (NonEmpty.loop (,) profile2D) & Result.orFail
  let allFaces = extrudedFaces & NonEmpty.push leftFace & NonEmpty.push rightFace
  let closedShell =
        Step.entity "CLOSED_SHELL" $
          [ Step.text ""
          , Step.list Step.referenceTo (NonEmpty.toList allFaces)
          ]
  let partName = "Phone holder"
  let manifoldSolidBRep =
        Step.entity "MANIFOLD_SOLID_BREP" $
          [ Step.text partName
          , Step.referenceTo closedShell
          ]
  let lengthUnits =
        Step.complexEntity
          [ Step.subEntity "LENGTH_UNIT" []
          , Step.subEntity "NAMED_UNIT" [Step.derived]
          , Step.subEntity "SI_UNIT" [Step.null, Step.enum "METRE"]
          ]
  let angleUnits =
        Step.complexEntity
          [ Step.subEntity "PLANE_ANGLE_UNIT" []
          , Step.subEntity "NAMED_UNIT" [Step.derived]
          , Step.subEntity "SI_UNIT" [Step.null, Step.enum "RADIAN"]
          ]
  let solidAngleUnits =
        Step.complexEntity
          [ Step.subEntity "SOLID_ANGLE_UNIT" []
          , Step.subEntity "NAMED_UNIT" [Step.derived]
          , Step.subEntity "SI_UNIT" [Step.null, Step.enum "STERADIAN"]
          ]
  let uncertaintyMeasureWithUnit =
        Step.entity "UNCERTAINTY_MEASURE_WITH_UNIT" $
          [ Step.numberAs "LENGTH_MEASURE" 1e-8
          , Step.referenceTo lengthUnits
          , Step.text "DISTANCE_ACCURACY_VALUE"
          , Step.text "Maximum Tolerance applied to model"
          ]
  let context =
        Step.complexEntity
          [ Step.subEntity "GEOMETRIC_REPRESENTATION_CONTEXT" [Step.int 3]
          , Step.subEntity "GLOBAL_UNCERTAINTY_ASSIGNED_CONTEXT" $
              [Step.list Step.referenceTo [uncertaintyMeasureWithUnit]]
          , Step.subEntity "GLOBAL_UNIT_ASSIGNED_CONTEXT" $
              [Step.list Step.referenceTo [lengthUnits, angleUnits, solidAngleUnits]]
          , Step.subEntity "REPRESENTATION_CONTEXT" $
              [Step.text partName, Step.text "TOP_LEVEL_ASSEMBLY_PART"]
          ]
  let advancedBRepShapeRepresentation =
        Step.entity "ADVANCED_BREP_SHAPE_REPRESENTATION" $
          [ Step.text ""
          , Step.list Step.referenceTo [manifoldSolidBRep]
          , Step.referenceTo context
          ]
  let shapeRepresentation =
        Step.entity "SHAPE_REPRESENTATION" $
          [ Step.text partName
          , Step.list Step.referenceTo [Ap242.axisPlacement3D World3D.topPlane]
          , Step.referenceTo context
          ]
  let applicationContext =
        Step.entity "APPLICATION_CONTEXT" [Step.text "managed model based 3d engineering"]
  let applicationProtocolDefinition =
        Step.entity "APPLICATION_PROTOCOL_DEFINITION" $
          [ Step.text "international standard"
          , Step.text "ap242_managed_model_based_3d_engineering"
          , Step.int 2020
          , Step.referenceTo applicationContext
          ]
  let productContext =
        Step.entity "PRODUCT_CONTEXT" $
          [Step.text "", Step.referenceTo applicationContext, Step.text "mechanical"]
  let product =
        Step.entity "PRODUCT" $
          [ Step.text partName
          , Step.text partName
          , Step.text partName
          , Step.list Step.referenceTo [productContext]
          ]
  let productDefinitionFormationWithSpecifiedSource =
        Step.entity "PRODUCT_DEFINITION_FORMATION_WITH_SPECIFIED_SOURCE" $
          [ Step.text ""
          , Step.text ""
          , Step.referenceTo product
          , Step.enum "NOT_KNOWN"
          ]
  let productDefinitionContext =
        Step.entity "PRODUCT_DEFINITION_CONTEXT" $
          [ Step.text ""
          , Step.referenceTo applicationContext
          , Step.text "design"
          ]
  let productDefinition =
        Step.entity "PRODUCT_DEFINITION" $
          [ Step.text ""
          , Step.text ""
          , Step.referenceTo productDefinitionFormationWithSpecifiedSource
          , Step.referenceTo productDefinitionContext
          ]
  let productDefinitionShape =
        Step.entity "PRODUCT_DEFINITION_SHAPE" $
          [Step.text "", Step.text "", Step.referenceTo productDefinition]
  let shapeDefinitionRepresentation =
        Step.entity "SHAPE_DEFINITION_REPRESENTATION" $
          [ Step.referenceTo productDefinitionShape
          , Step.referenceTo shapeRepresentation
          ]
  let shapeRepresentationRelationship =
        Step.entity "SHAPE_REPRESENTATION_RELATIONSHIP" $
          [ Step.text ""
          , Step.text ""
          , Step.referenceTo shapeRepresentation
          , Step.referenceTo advancedBRepShapeRepresentation
          ]
  let header =
        Step.header
          (#description ["A simple phone holder"])
          (#implementationLevel "2;1")
          (#fileName "phone-holder.step")
          (#timestamp "2026-03-25T13:00:00")
          (#author ["Ian Mackenzie"])
          (#organization ["Generative Engineering"])
          (#preprocessorVersion "")
          (#originatingSystem "OpenSolid")
          (#authorization "")
          (#schemaIdentifiers ["AUTOMOTIVE_DESIGN { 1 0 10303 442 4 1 4 }"])
  Step.write "executables/phone-holder/phone-holder.step" header $
    [ applicationProtocolDefinition
    , shapeRepresentationRelationship
    , shapeDefinitionRepresentation
    ]

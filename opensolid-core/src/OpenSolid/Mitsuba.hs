module OpenSolid.Mitsuba
  ( Lighting
  , convention
  , meshesBuilder
  , writeMeshes
  , writeFiles
  , environmentMap
  )
where

import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString qualified
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Camera3d (Camera3d)
import OpenSolid.Camera3d qualified as Camera3d
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Model3d (Model3d)
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Resolution (Resolution)
import OpenSolid.Text qualified as Text

-- | The lighting to use for a Mitsuba scene.
data Lighting space where
  EnvironmentMap :: Frame3d (space @ Meters) defines -> Text -> Lighting space

instance FFI (Lighting space) where
  representation = FFI.nestedClassRepresentation "Mitsuba" "Lighting"

{-| The Mitsuba world coordinate convention (Y up, Z backward).

Source: https://github.com/mitsuba-renderer/mitsuba-blender/discussions/47#discussioncomment-3847744
-}
convention :: Convention3d
convention =
  Convention3d.custom
    Orientation3d.rightwardDirection
    Orientation3d.upwardDirection
    Orientation3d.backwardDirection

xDirection :: Frame3d (space @ units) defines -> Direction3d space
xDirection frame = Convention3d.xDirection frame.orientation convention

yDirection :: Frame3d (space @ units) defines -> Direction3d space
yDirection frame = Convention3d.yDirection frame.orientation convention

zDirection :: Frame3d (space @ units) defines -> Direction3d space
zDirection frame = Convention3d.zDirection frame.orientation convention

{-| Write a Mitsuba scene out to an XML scene description and a file containing binary mesh data.

The scene description file will be the given path with ".xml" appended,
and the binary mesh data file with be the given path with ".serialized" appended.
The given resolution will be used when meshing all objects in the scene.

Note that calling this function does not actually render the given scene,
it just generates the necessary input files for the Mitsuba renderer.
To actually render the generated scene, you'll need to use the Mitsuba Python package
(https://mitsuba.readthedocs.io/en/stable/),
calling 'mitsuba.load_file' with the path to the generated XML file.

The generated scene will by default use 16 samples per pixel, and render an image with resolution 800x600.
However, these can be configured by setting the 'spp', 'width' and 'height' parameters when loading the scene,
for example with 'mitsuba.load_file(path_to_xml_file, spp=256, width=1920, height=1080)'.
-}
writeFiles ::
  ( "path" ::: Text
  , "model" ::: Model3d space
  , "resolution" ::: Resolution Meters
  , "camera" ::: Camera3d (space @ Meters)
  , "lighting" ::: Lighting space
  ) ->
  IO ()
writeFiles args = IO.do
  let collectedMeshes = Model3d.traverse (collectMeshes args.resolution) args.model
  let (meshes, properties) = List.unzip2 collectedMeshes
  let meshFileName = args.path <> ".serialized"
  writeMeshes meshFileName meshes
  let sceneXml = sceneDocument do
        #camera args.camera
        #lighting args.lighting
        #meshProperties properties
        #meshFileName meshFileName
  let sceneFileName = args.path <> ".xml"
  IO.writeUtf8 sceneFileName sceneXml

type Vertex space = (Point3d (space @ Meters), Direction3d space)

type Mesh space = Mesh.Mesh (Vertex space)

data Properties = Properties
  { material :: PbrMaterial
  , opacity :: Float
  , qualifiedName :: Text
  }

collectMeshes ::
  Model3d.Traversal =>
  Resolution Meters ->
  Model3d space ->
  List ((Mesh space, "name" ::: Text), Properties)
collectMeshes resolution model = case model of
  Model3d.Group children -> List.collect (collectMeshes resolution) children
  Model3d.Body body -> do
    let mesh = Body3d.toMesh resolution body
    let material = Model3d.traversal.currentPbrMaterial
    let opacity = Model3d.traversal.currentMultipliedOpacity
    let qualifiedName = case Model3d.traversal.ownName of
          Nothing -> "" -- A mesh only has a fully-qualified name if it has a name of its own
          Just ownName -> do
            let nameComponents = Model3d.traversal.parentNames <> [ownName]
            Text.join "." nameComponents
    let properties = Properties{material, opacity, qualifiedName}
    List.singleton ((mesh, #name qualifiedName), properties)

{-| Specify an environment map to be used as lighting.

You should pass a frame that defines the orientation of the environment map
(which can often just be 'Frame3d.world')
and the path to the environment map image itself.

The environment map image will typically be in OpenEXR format;
https://polyhaven.com is a good source for free ones.
-}
environmentMap :: Frame3d (space @ Meters) defines -> Text -> Lighting space
environmentMap = EnvironmentMap

{-| Write a list of named meshes out to a single Mitsuba .serialized file.

The names may all be empty if desired.
-}
writeMeshes :: Text -> List (Mesh space, "name" ::: Text) -> IO ()
writeMeshes path meshes = IO.writeBinary path (meshesBuilder meshes)

-- | Generate the binary content of a Mitsuba .serialized file containing the given meshes.
meshesBuilder :: List (Mesh space, "name" ::: Text) -> Builder
meshesBuilder meshes = do
  let (meshBuilders, meshSizes) = List.unzip2 (List.map buildSingleMesh meshes)
  Binary.concat
    [ Binary.concat meshBuilders -- Individual meshes
    , meshOffsets 0 meshSizes -- Byte offsets of individual meshes
    , Binary.uint32LE meshes.length -- Total number of meshes
    ]

meshOffsets :: Int -> List Int -> Builder
meshOffsets currentOffset meshSizes = case meshSizes of
  [] -> Binary.empty
  currentSize : remainingSizes -> do
    let nextOffset = currentOffset + currentSize
    Binary.uint64LE currentOffset <> meshOffsets nextOffset remainingSizes

buildSingleMesh :: (Mesh space, "name" ::: Text) -> (Builder, Int)
buildSingleMesh (mesh, Field name) = do
  let header =
        Binary.concat
          [ Binary.uint16LE 0x041C -- File format identifier
          , Binary.uint16LE 0x0004 -- Version identifier
          ]
  let headerSize = 4 -- 2x uint16
  let compressedData =
        meshDataBuilder mesh name
          |> Builder.toLazyByteString
          |> Zlib.compress
          |> Data.ByteString.Lazy.toStrict
  let overallBuilder = header <> Builder.byteString compressedData
  let overallSize = headerSize + Data.ByteString.length compressedData
  (overallBuilder, overallSize)

meshDataBuilder :: Mesh space -> Text -> Builder
meshDataBuilder mesh name =
  Binary.concat
    [ Binary.uint32LE 0x2001 -- Flags: per-vertex normals, double precision
    , Text.toUtf8 name <> Binary.uint8 0 -- Null-terminated, UTF-8 encoded name
    , Binary.uint64LE mesh.numVertices
    , Binary.uint64LE mesh.numFaces
    , Binary.collect pointBuilder mesh.vertices
    , Binary.collect normalBuilder mesh.vertices
    , Binary.collect faceIndicesBuilder mesh.faceIndices
    ]

pointBuilder :: Vertex space -> Builder
pointBuilder (point, _) = do
  let (px, py, pz) = Point3d.coordinates convention point
  Binary.concat
    [ Binary.float64LE (Length.inMeters px)
    , Binary.float64LE (Length.inMeters py)
    , Binary.float64LE (Length.inMeters pz)
    ]

normalBuilder :: Vertex space -> Builder
normalBuilder (_, normal) = do
  let (nx, ny, nz) = Direction3d.components convention normal
  Binary.concat
    [ Binary.float64LE nx
    , Binary.float64LE ny
    , Binary.float64LE nz
    ]

faceIndicesBuilder :: (Int, Int, Int) -> Builder
faceIndicesBuilder (i, j, k) = Binary.uint32LE i <> Binary.uint32LE j <> Binary.uint32LE k

data XmlNode = XmlNode
  { name :: Text
  , attributes :: List (Text, Text)
  , children :: List XmlNode
  }

nodeText :: XmlNode -> Text
nodeText node = do
  let openingContents = Text.join " " (node.name : List.map attributeText node.attributes)
  case node.children of
    [] -> "<" <> openingContents <> "/>"
    _ ->
      Text.multiline
        [ "<" <> openingContents <> ">"
        , Text.indent "    " (Text.multiline (List.map nodeText node.children))
        , "</" <> node.name <> ">"
        ]

attributeText :: (Text, Text) -> Text
attributeText (name, value) = name <> "=\"" <> value <> "\""

sceneDocument ::
  ( "camera" ::: Camera3d (space @ Meters)
  , "lighting" ::: Lighting space
  , "meshProperties" ::: List Properties
  , "meshFileName" ::: Text
  ) ->
  Text
sceneDocument args = do
  let shapeNodes = List.mapWithIndex (shapeNode args.meshFileName) args.meshProperties
  let integratorNode = XmlNode "integrator" [("type", "path")] []
  let documentNodes =
        defaultNode "spp" "16"
          : defaultNode "width" "800"
          : defaultNode "height" "600"
          : cameraNode args.camera
          : lightingNode args.lighting
          : integratorNode
          : shapeNodes
  let rootNode = XmlNode "scene" [("version", "3.0.0")] documentNodes
  nodeText rootNode

defaultNode :: Text -> Text -> XmlNode
defaultNode name value = XmlNode "default" [("name", name), ("value", value)] []

lightingNode :: Lighting space -> XmlNode
lightingNode lighting = case lighting of
  EnvironmentMap frame path -> do
    let (x0, y0, z0) = Point3d.coordinates convention frame.originPoint
    let px = Length.inMeters x0
    let py = Length.inMeters y0
    let pz = Length.inMeters z0
    let (ix, iy, iz) = Direction3d.components convention (xDirection frame)
    let (jx, jy, jz) = Direction3d.components convention (yDirection frame)
    let (kx, ky, kz) = Direction3d.components convention (zDirection frame)
    let transformValues = [ix, jx, kx, px, iy, jy, ky, py, iz, jz, kz, pz, 0.0, 0.0, 0.0, 1.0]
    let transformString = Text.join " " (List.map Text.float transformValues)
    let matrixNode = XmlNode "matrix" [("value", transformString)] []
    XmlNode "emitter" [("type", "envmap")] $
      [ stringNode "filename" path
      , XmlNode "transform" [("name", "to_world")] [matrixNode]
      ]

shapeNode :: Text -> Int -> Properties -> XmlNode
shapeNode fileName index properties = do
  let typeAttribute = ("type", "serialized")
  let shapeAttributes = case properties.qualifiedName of
        "" -> [typeAttribute] -- IDs should be unique, so don't write an empty name
        qualifiedName -> [typeAttribute, ("id", qualifiedName)]
  XmlNode "shape" shapeAttributes $
    [ stringNode "filename" fileName
    , integerNode "shape_index" index
    , bsdfNode properties.material properties.opacity
    ]

bsdfNode :: PbrMaterial -> Float -> XmlNode
bsdfNode material opacity = do
  let (r, g, b) = Color.rgbFloatComponents material.baseColor
  let rgbString = Text.join "," (List.map Text.float [r, g, b])
  let principledNode =
        XmlNode "bsdf" [("type", "principled")] $
          [ typedNode "rgb" "base_color" rgbString
          , floatNode "metallic" material.metallic
          , floatNode "roughness" material.roughness
          ]
  if opacity == 1.0
    then principledNode
    else
      XmlNode "bsdf" [("type", "mask")] $
        [ typedNode "rgb" "opacity" (Text.float opacity)
        , XmlNode "bsdf" [("type", "twosided")] [principledNode]
        ]

cameraNode :: Camera3d (space @ Meters) -> XmlNode
cameraNode camera = do
  let filmNode =
        XmlNode "film" [("type", "hdrfilm")] $
          [ typedNode "integer" "width" "$width"
          , typedNode "integer" "height" "$height"
          ]
  let samplerNode =
        XmlNode "sampler" [("type", "multijitter")] $
          [typedNode "integer" "sample_count" "$spp"]
  case camera.projection of
    Camera3d.Orthographic fovHeight -> do
      let scale = Length.inMeters fovHeight / 2.0
      XmlNode "sensor" [("type", "orthographic")] $
        [ XmlNode "transform" [("name", "to_world")] $
            [ XmlNode "scale" [("value", Text.float scale)] []
            , cameraTransformationNode camera
            ]
        , filmNode
        , samplerNode
        ]
    Camera3d.Perspective fovAngle -> do
      XmlNode "sensor" [("type", "perspective")] $
        [ floatNode "fov" (Angle.inDegrees fovAngle)
        , stringNode "fov_axis" "y"
        , XmlNode "transform" [("name", "to_world")] [cameraTransformationNode camera]
        , filmNode
        , samplerNode
        ]

typedNode :: Text -> Text -> Text -> XmlNode
typedNode valueType name value = XmlNode valueType [("name", name), ("value", value)] []

integerNode :: Text -> Int -> XmlNode
integerNode name value = typedNode "integer" name (Text.int value)

floatNode :: Text -> Float -> XmlNode
floatNode name value = typedNode "float" name (Text.float value)

stringNode :: Text -> Text -> XmlNode
stringNode name value = typedNode "string" name value

cameraTransformationNode :: Camera3d (space @ Meters) -> XmlNode
cameraTransformationNode camera = do
  let (x0, y0, z0) = Point3d.coordinates convention camera.eyePoint
  let px = Length.inMeters x0
  let py = Length.inMeters y0
  let pz = Length.inMeters z0
  let (ix, iy, iz) = Direction3d.components convention camera.leftwardDirection
  let (jx, jy, jz) = Direction3d.components convention camera.upwardDirection
  let (kx, ky, kz) = Direction3d.components convention camera.forwardDirection
  let floatValues = [ix, jx, kx, px, iy, jy, ky, py, iz, jz, kz, pz, 0.0, 0.0, 0.0, 1.0]
  XmlNode "matrix" [("value", Text.join " " (List.map Text.float floatValues))] []

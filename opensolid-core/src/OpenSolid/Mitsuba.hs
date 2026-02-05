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
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Camera3D (Camera3D)
import OpenSolid.Camera3D qualified as Camera3D
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3D (Convention3D)
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Model3D (Model3D)
import OpenSolid.Model3D qualified as Model3D
import OpenSolid.Orientation3D qualified as Orientation3D
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Resolution (Resolution)
import OpenSolid.SurfaceVertex3D (SurfaceVertex3D)
import OpenSolid.SurfaceVertex3D qualified as SurfaceVertex3D
import OpenSolid.Text qualified as Text

-- | The lighting to use for a Mitsuba scene.
data Lighting space where
  EnvironmentMap :: Frame3D global local -> Text -> Lighting global

instance FFI (Lighting FFI.Space) where
  representation = FFI.nestedClassRepresentation "Mitsuba" "Lighting"

{-| The Mitsuba world coordinate convention (Y up, Z backward).

Source: https://github.com/mitsuba-renderer/mitsuba-blender/discussions/47#discussioncomment-3847744
-}
convention :: Convention3D
convention =
  Convention3D.custom
    Orientation3D.rightwardDirection
    Orientation3D.upwardDirection
    Orientation3D.backwardDirection

xDirection :: Frame3D global local -> Direction3D global
xDirection frame = Convention3D.xDirection frame.orientation convention

yDirection :: Frame3D global local -> Direction3D global
yDirection frame = Convention3D.yDirection frame.orientation convention

zDirection :: Frame3D global local -> Direction3D global
zDirection frame = Convention3D.zDirection frame.orientation convention

{-| Write a Mitsuba scene out to an XML scene description and a file containing binary mesh data.

The scene description file will be the given path with ".xml" appended,
and the binary mesh data file with be the given path with ".serialized" appended.
The given resolution will be used when meshing all objects in the scene.

Note that calling this function does not actually render the given scene,
it just generates the necessary input files for the Mitsuba renderer.
To actually render the generated scene, you'll need to use the Mitsuba Python package
(https://mitsuba.readthedocs.io/en/stable/),
calling 'mitsuba.load_file' with the path to the generated XML file.

The generated scene will by default use 16 samples per pixel,
and render an image with resolution 800x600.
However, these can be configured by
setting the 'spp', 'width' and 'height' parameters when loading the scene,
for example with 'mitsuba.load_file(path_to_xml_file, spp=256, width=1920, height=1080)'.
-}
writeFiles ::
  "path" ::: Text ->
  "model" ::: Model3D space ->
  "resolution" ::: Resolution Meters ->
  "camera" ::: Camera3D space ->
  "lighting" ::: Lighting space ->
  IO ()
writeFiles (Named path) (Named model) (Named resolution) (Named camera) (Named lighting) = do
  let collectedMeshes = Model3D.inspect (collectMeshes resolution) model
  let (meshes, properties) = List.unzip2 collectedMeshes
  let meshFileName = path <> ".serialized"
  writeMeshes meshFileName meshes
  let sceneXml =
        sceneDocument
          (#camera camera)
          (#lighting lighting)
          (#meshProperties properties)
          (#meshFileName meshFileName)
  let sceneFileName = path <> ".xml"
  IO.writeUtf8 sceneFileName sceneXml

type Mesh space = Mesh.Mesh (SurfaceVertex3D space)

data Properties = Properties
  { material :: PbrMaterial
  , opacity :: Number
  , qualifiedName :: Text
  }

collectMeshes ::
  Model3D.Traversal =>
  Resolution Meters ->
  Model3D space ->
  List ((Mesh space, "name" ::: Text), Properties)
collectMeshes resolution model = case model of
  Model3D.Group children -> List.combine (collectMeshes resolution) children
  Model3D.Body body -> do
    let mesh = Body3D.toSurfaceMesh resolution body
    let material = Model3D.traversal.currentPbrMaterial
    let opacity = Model3D.traversal.currentMultipliedOpacity
    let qualifiedName = case Model3D.traversal.ownName of
          Nothing -> "" -- A mesh only has a fully-qualified name if it has a name of its own
          Just ownName -> do
            let nameComponents = Model3D.traversal.parentNames <> [ownName]
            Text.join "." nameComponents
    let properties = Properties{material, opacity, qualifiedName}
    List.singleton ((mesh, #name qualifiedName), properties)

{-| Specify an environment map to be used as lighting.

You should pass a frame that defines the orientation of the environment map
(which can often just be 'World3D.frame')
and the path to the environment map image itself.

The environment map image will typically be in OpenEXR format;
https://polyhaven.com is a good source for free ones.
-}
environmentMap :: Frame3D global local -> Text -> Lighting global
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
    , Binary.uint32LE (List.length meshes) -- Total number of meshes
    ]

meshOffsets :: Int -> List Int -> Builder
meshOffsets currentOffset meshSizes = case meshSizes of
  [] -> Binary.empty
  currentSize : remainingSizes -> do
    let nextOffset = currentOffset + currentSize
    Binary.uint64LE currentOffset <> meshOffsets nextOffset remainingSizes

buildSingleMesh :: (Mesh space, "name" ::: Text) -> (Builder, Int)
buildSingleMesh (mesh, Named name) = do
  let header =
        Binary.concat
          [ Binary.uint16LE 0x041C -- File format identifier
          , Binary.uint16LE 0x0004 -- Version identifier
          ]
  let headerSize = 4 -- 2x uint16
  let compressedData =
        meshDataBuilder mesh name
          & Builder.toLazyByteString
          & Zlib.compress
          & Data.ByteString.Lazy.toStrict
  let overallBuilder = header <> Builder.byteString compressedData
  let overallSize = headerSize + Data.ByteString.length compressedData
  (overallBuilder, overallSize)

meshDataBuilder :: Mesh space -> Text -> Builder
meshDataBuilder mesh name = do
  let meshVertices = Mesh.vertices mesh
  let meshFaceIndices = Mesh.faceIndices mesh
  Binary.concat
    [ Binary.uint32LE 0x2001 -- Flags: per-vertex normals, double precision
    , Text.toUtf8 name <> Binary.uint8 0 -- Null-terminated, UTF-8 encoded name
    , Binary.uint64LE (Mesh.numVertices mesh)
    , Binary.uint64LE (Mesh.numFaces mesh)
    , Binary.combine pointBuilder meshVertices
    , Binary.combine normalBuilder meshVertices
    , Binary.combine faceIndicesBuilder meshFaceIndices
    ]

pointBuilder :: SurfaceVertex3D space -> Builder
pointBuilder vertex = do
  let (px, py, pz) = Point3D.coordinates convention (SurfaceVertex3D.position vertex)
  Binary.concat
    [ Binary.float64LE (Length.inMeters px)
    , Binary.float64LE (Length.inMeters py)
    , Binary.float64LE (Length.inMeters pz)
    ]

normalBuilder :: SurfaceVertex3D space -> Builder
normalBuilder vertex = do
  let (nx, ny, nz) = Direction3D.components convention (SurfaceVertex3D.normal vertex)
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
  "camera" ::: Camera3D space ->
  "lighting" ::: Lighting space ->
  "meshProperties" ::: List Properties ->
  "meshFileName" ::: Text ->
  Text
sceneDocument (Named camera) (Named lighting) (Named meshProperties) (Named meshFileName) = do
  let shapeNodes = List.mapWithIndex (shapeNode meshFileName) meshProperties
  let integratorNode = XmlNode "integrator" [("type", "path")] []
  let documentNodes =
        defaultNode "spp" "16"
          : defaultNode "width" "800"
          : defaultNode "height" "600"
          : cameraNode camera
          : lightingNode lighting
          : integratorNode
          : shapeNodes
  let rootNode = XmlNode "scene" [("version", "3.0.0")] documentNodes
  nodeText rootNode

defaultNode :: Text -> Text -> XmlNode
defaultNode name value = XmlNode "default" [("name", name), ("value", value)] []

lightingNode :: Lighting space -> XmlNode
lightingNode lighting = case lighting of
  EnvironmentMap frame path -> do
    let (x0, y0, z0) = Point3D.coordinates convention frame.originPoint
    let px = Length.inMeters x0
    let py = Length.inMeters y0
    let pz = Length.inMeters z0
    let (ix, iy, iz) = Direction3D.components convention (xDirection frame)
    let (jx, jy, jz) = Direction3D.components convention (yDirection frame)
    let (kx, ky, kz) = Direction3D.components convention (zDirection frame)
    let transformValues = [ix, jx, kx, px, iy, jy, ky, py, iz, jz, kz, pz, 0.0, 0.0, 0.0, 1.0]
    let transformString = Text.join " " (List.map Text.number transformValues)
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

bsdfNode :: PbrMaterial -> Number -> XmlNode
bsdfNode material opacity = do
  let (r, g, b) = Color.toRgb1 material.baseColor
  let rgbString = Text.join "," (List.map Text.number [r, g, b])
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
        [ typedNode "rgb" "opacity" (Text.number opacity)
        , XmlNode "bsdf" [("type", "twosided")] [principledNode]
        ]

cameraNode :: Camera3D space -> XmlNode
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
    Camera3D.Orthographic fovHeight -> do
      let scale = Length.inMeters fovHeight / 2.0
      XmlNode "sensor" [("type", "orthographic")] $
        [ XmlNode "transform" [("name", "to_world")] $
            [ XmlNode "scale" [("value", Text.number scale)] []
            , cameraTransformationNode camera
            ]
        , filmNode
        , samplerNode
        ]
    Camera3D.Perspective fovAngle -> do
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

floatNode :: Text -> Number -> XmlNode
floatNode name value = typedNode "float" name (Text.number value)

stringNode :: Text -> Text -> XmlNode
stringNode name value = typedNode "string" name value

cameraTransformationNode :: Camera3D space -> XmlNode
cameraTransformationNode camera = do
  let (x0, y0, z0) = Point3D.coordinates convention camera.eyePoint
  let px = Length.inMeters x0
  let py = Length.inMeters y0
  let pz = Length.inMeters z0
  let (ix, iy, iz) = Direction3D.components convention camera.leftwardDirection
  let (jx, jy, jz) = Direction3D.components convention camera.upwardDirection
  let (kx, ky, kz) = Direction3D.components convention camera.forwardDirection
  let matrixComponents = [ix, jx, kx, px, iy, jy, ky, py, iz, jz, kz, pz, 0.0, 0.0, 0.0, 1.0]
  XmlNode "matrix" [("value", Text.join " " (List.map Text.number matrixComponents))] []

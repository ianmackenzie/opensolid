from pathlib import Path

from opensolid import (
    Angle,
    Axis2D,
    Body3D,
    Bounds2D,
    Camera3D,
    Color,
    Curve2D,
    Gltf,
    Length,
    LengthBounds,
    Mitsuba,
    Model3D,
    PbrMaterial,
    Point2D,
    Point3D,
    Region2D,
    Resolution,
    Tolerance,
    World3D,
)

with Tolerance(Length.nanometers(1)):
    p1 = Point2D.centimeters(0, 1)
    p2 = Point2D.centimeters(1, 1)
    p3 = Point2D.centimeters(1, 0)
    p4 = Point2D.centimeters(2, 0)
    p5 = Point2D.centimeters(2, 2)
    p6 = Point2D.centimeters(1, 2)
    p7 = Point2D.centimeters(0, 3)

    right_curves = [
        Curve2D.line_from(p1, p2),
        Curve2D.line_from(p2, p3),
        Curve2D.line_from(p3, p4),
        Curve2D.line_from(p4, p5),
        Curve2D.line_from(p5, p6),
        Curve2D.arc_from(p6, p7, Angle.quarter_turn),
    ]
    left_curves = [curve.mirror_across(Axis2D.y) for curve in right_curves]
    base_region = Region2D.bounded_by(left_curves + right_curves)
    left_fillet_points = [p2, p6, p5]
    right_fillet_points = [p.mirror_across(Axis2D.y) for p in left_fillet_points]
    fillet_points = left_fillet_points + right_fillet_points
    filleted_region = base_region.fillet(fillet_points, radius=Length.millimeters(4))

    thickness = Length.centimeters(2)
    body = Body3D.extruded(
        World3D.front_plane,
        filleted_region,
        -thickness / 2,
        thickness / 2,
    )

    material = PbrMaterial.nonmetal(Color.blue, roughness=0.3)
    model = Model3D.body(body).with_pbr_material(material).with_name("Body")
    ground_limits = LengthBounds.symmetric(width=Length.meters(2))
    ground_body = Body3D.extruded(
        World3D.bottom_plane,
        Region2D.rectangle(Bounds2D(ground_limits, ground_limits)),
        Length.millimeters(2),
        Length.centimeters(1),
    )
    ground_material = PbrMaterial.nonmetal(Color.dark_charcoal, roughness=0.5)
    ground_model = (
        Model3D.body(ground_body).with_pbr_material(ground_material).with_name("Ground")
    )
    resolution = Resolution.max_error(Length.millimeters(0.01))
    gltf = Gltf(Model3D.group([model, ground_model]))
    gltf.write_binary("fillet.glb", resolution)

    camera = Camera3D.orbit(
        focal_point=Point3D.z_up(Length.zero, Length.zero, Length.centimeters(1)),
        azimuth=Angle.degrees(30),
        elevation=Angle.degrees(30),
        distance=Length.centimeters(12),
        projection=Camera3D.perspective(vertical_fov=Angle.degrees(30)),
    )
    hdris_path = Path("/home/ian/Downloads/HDRIs")
    lighting_image = "kloppenheim_07_puresky_4k.exr"

    lighting = Mitsuba.environment_map(World3D.frame, str(hdris_path / lighting_image))
    scene = Mitsuba(Model3D.group([model, ground_model]), camera, lighting)
    scene.write_files("fillet", resolution)

from opensolid import (
    Angle,
    Axis2d,
    Body3d,
    Color,
    Curve2d,
    Length,
    LengthBounds,
    Frame3d,
    Resolution,
    Bounds2d,
    PbrMaterial,
    Point2d,
    Region2d,
    Model3d,
    Gltf,
    Tolerance,
    Camera3d,
    Mitsuba,
)
from pathlib import Path

with Tolerance(Length.nanometers(1)):
    world = Frame3d.world

    p1 = Point2d.centimeters(0, 1)
    p2 = Point2d.centimeters(1, 1)
    p3 = Point2d.centimeters(1, 0)
    p4 = Point2d.centimeters(2, 0)
    p5 = Point2d.centimeters(2, 2)
    p6 = Point2d.centimeters(1, 2)
    p7 = Point2d.centimeters(0, 3)

    right_curves = [
        Curve2d.line(p1, p2),
        Curve2d.line(p2, p3),
        Curve2d.line(p3, p4),
        Curve2d.line(p4, p5),
        Curve2d.line(p5, p6),
        Curve2d.arc(p6, p7, Angle.quarter_turn),
    ]
    left_curves = [curve.mirror_across(Axis2d.y) for curve in right_curves]
    base_region = Region2d.bounded_by(left_curves + right_curves)
    left_fillet_points = [p2, p6, p5]
    right_fillet_points = [p.mirror_across(Axis2d.y) for p in left_fillet_points]
    fillet_points = left_fillet_points + right_fillet_points
    filleted_region = base_region.fillet(fillet_points, radius=Length.millimeters(4))

    thickness = Length.centimeters(2)
    extrusion_bounds = LengthBounds.symmetric(width=thickness)
    body = Body3d.extruded(world.front_plane, filleted_region, extrusion_bounds)

    material = PbrMaterial.nonmetal(Color.blue, roughness=0.3)
    model = Model3d.body(body).with_pbr_material(material).with_name("Body")
    ground_limits = LengthBounds.symmetric(width=Length.meters(2))
    ground_body = Body3d.extruded(
        world.bottom_plane,
        Region2d.rectangle(Bounds2d(ground_limits, ground_limits)),
        LengthBounds.zero_to(Length.centimeters(1)),
    )
    ground_material = PbrMaterial.nonmetal(Color.dark_charcoal, roughness=0.5)
    ground_model = (
        Model3d.body(ground_body).with_pbr_material(ground_material).with_name("Ground")
    )
    resolution = Resolution.max_error(Length.millimeters(0.01))
    Gltf(Model3d.group([model, ground_model])).write_binary("fillet.glb", resolution)

    focal_point = world.origin_point.translate_in(
        world.upward_direction,
        Length.centimeters(1),
    )
    camera = Camera3d.orbit(
        focal_point=focal_point,
        azimuth=Angle.degrees(30),
        elevation=Angle.degrees(30),
        distance=Length.centimeters(12),
        projection=Camera3d.perspective(vertical_fov=Angle.degrees(30)),
    )
    hdris_path = Path("/home/ian/Downloads/HDRIs")
    lighting_image = "kloppenheim_07_puresky_4k.exr"

    lighting = Mitsuba.environment_map(world, str(hdris_path / lighting_image))
    scene = Mitsuba(Model3d.group([model, ground_model]), camera, lighting)
    scene.write_files("fillet", resolution)

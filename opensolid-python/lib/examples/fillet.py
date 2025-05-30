from opensolid import (
    Angle,
    Axis2d,
    Body3d,
    Color,
    Curve2d,
    LengthBounds,
    Length,
    Mesh,
    Point2d,
    Region2d,
    Scene3d,
    Tolerance,
    World3d,
)

with Tolerance(Length.nanometers(1)):
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
    body = Body3d.extruded(
        World3d.front_plane,
        filleted_region,
        LengthBounds.symmetric(width=thickness),
    )
    mesh_constraints = [Mesh.max_error(Length.millimeters(0.05))]
    material = Scene3d.nonmetal(Color.blue, roughness=0.3)
    scene = [Scene3d.body(mesh_constraints, material, body)]
    Scene3d.write_glb("fillet.glb", scene)

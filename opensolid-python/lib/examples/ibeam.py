from opensolid import (
    Length,
    Point2d,
    Curve2d,
    Direction2d,
    Axis2d,
    Region2d,
    Body3d,
    Plane3d,
    Mesh,
    LengthRange,
    Scene3d,
    Tolerance,
    Color,
)

with Tolerance(Length.nanometers(1)):
    # Define dimensions
    length = Length.centimeters(30)
    width = Length.centimeters(10)
    height = Length.centimeters(15)
    thickness = Length.centimeters(2)
    fillet_radius = Length.millimeters(10)

    # Define key X/Y coordinates
    web_right_x = thickness / 2
    flange_right_x = width / 2
    top_flange_top_y = height / 2
    top_flange_bottom_y = top_flange_top_y - thickness

    # Define key points on sketch profile
    p1 = Point2d.x(web_right_x)
    p2 = Point2d.xy(web_right_x, top_flange_bottom_y)
    p3 = Point2d.xy(flange_right_x, top_flange_bottom_y)
    p4 = Point2d.xy(flange_right_x, top_flange_top_y)
    p5 = Point2d.y(top_flange_top_y)

    # Create the sketch profile
    fillet = Curve2d.corner_arc(p2, Direction2d.y, Direction2d.x, fillet_radius)
    template = [
        Curve2d.line(p1, fillet.start_point()),
        fillet,
        Curve2d.line(fillet.end_point(), p3),
        Curve2d.line(p3, p4),
        Curve2d.line(p4, p5),
    ]
    top_curves = template + [curve.mirror_across(Axis2d.y) for curve in template]
    curves = top_curves + [curve.mirror_across(Axis2d.x) for curve in top_curves]
    profile = Region2d.bounded_by(curves)

    # Extrude the profile to create a solid body
    extrusion_limits = LengthRange(-0.5 * length, 0.5 * length)
    body = Body3d.extruded(Plane3d.yz, profile, extrusion_limits)

    # Create a 3D scene containing the body and write to GLB file
    mesh_constraints = [Mesh.max_error(Length.millimeters(1))]
    material = Scene3d.metal(Color.rgb(0.913, 0.921, 0.925), roughness=0.3)
    entity = Scene3d.body(mesh_constraints, material, body)
    Scene3d.write_glb("i-beam.glb", Plane3d.xy, [entity])

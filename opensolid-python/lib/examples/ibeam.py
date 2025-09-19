from opensolid import (
    Axis2d,
    Body3d,
    Color,
    Curve2d,
    Direction2d,
    Length,
    Resolution,
    PbrMaterial,
    Point2d,
    Region2d,
    Frame3d,
    Model3d,
    Gltf,
    Tolerance,
)

with Tolerance(Length.meters(1e-9)):
    world = Frame3d.world

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
    p2 = Point2d(web_right_x, top_flange_bottom_y)
    p3 = Point2d(flange_right_x, top_flange_bottom_y)
    p4 = Point2d(flange_right_x, top_flange_top_y)
    p5 = Point2d.y(top_flange_top_y)

    # Create the sketch profile
    fillet = Curve2d.corner_arc(
        p2,
        incoming=Direction2d.y,
        outgoing=Direction2d.x,
        radius=fillet_radius,
    )
    template = [
        Curve2d.line(p1, fillet.start_point),
        fillet,
        Curve2d.line(fillet.end_point, p3),
        Curve2d.line(p3, p4),
        Curve2d.line(p4, p5),
    ]
    top_curves = template + [curve.mirror_across(Axis2d.y) for curve in template]
    curves = top_curves + [curve.mirror_across(Axis2d.x) for curve in top_curves]
    profile = Region2d.bounded_by(curves)

    # Extrude the profile to create a solid body
    body = Body3d.extruded(world.front_plane, profile, -length / 2, length / 2)

    # Create a 3D model containing the body and write to GLB file
    material = PbrMaterial.metal(Color.rgb_float(0.913, 0.921, 0.925), roughness=0.3)
    model = Model3d.body(body).with_pbr_material(material)
    resolution = Resolution.max_error(Length.millimeters(0.1))
    Gltf(model).write_binary("ibeam.glb", resolution)

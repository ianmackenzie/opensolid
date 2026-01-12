from opensolid import (
    Axis2D,
    Body3D,
    Color,
    Curve2D,
    Direction2D,
    Gltf,
    Length,
    Model3D,
    PbrMaterial,
    Point2D,
    Region2D,
    Resolution,
    Tolerance,
    World3D,
)

with Tolerance(Length.meters(1e-9)):
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
    p1 = Point2D.x(web_right_x)
    p2 = Point2D(web_right_x, top_flange_bottom_y)
    p3 = Point2D(flange_right_x, top_flange_bottom_y)
    p4 = Point2D(flange_right_x, top_flange_top_y)
    p5 = Point2D.y(top_flange_top_y)

    # Create the sketch profile
    fillet = Curve2D.corner_arc(
        p2,
        incoming=Direction2D.y,
        outgoing=Direction2D.x,
        radius=fillet_radius,
    )
    template = [
        Curve2D.line_from(p1, fillet.start_point),
        fillet,
        Curve2D.line_from(fillet.end_point, p3),
        Curve2D.line_from(p3, p4),
        Curve2D.line_from(p4, p5),
    ]
    top_curves = template + [curve.mirror_across(Axis2D.y) for curve in template]
    curves = top_curves + [curve.mirror_across(Axis2D.x) for curve in top_curves]
    profile = Region2D.bounded_by(curves)

    # Extrude the profile to create a solid body
    body = Body3D.extruded(World3D.front_plane, profile, -length / 2, length / 2)

    # Create a 3D model containing the body and write to GLB file
    material = PbrMaterial.metal(Color.rgb1(0.913, 0.921, 0.925), roughness=0.3)
    model = Model3D.body(body).with_pbr_material(material)
    resolution = Resolution.max_error(Length.millimeters(0.1))
    Gltf(model).write_binary("ibeam.glb", resolution)

from opensolid import Body3D, Convention3D, Length, Resolution, Tolerance, World3D

with Tolerance(Length.nanometers(1)):
    body = Body3D.sphere(
        center_point=World3D.origin_point,
        diameter=Length.centimeters(10),
    )
    body.write_stl(
        "sphere.stl",
        Convention3D.y_up,
        Resolution.max_error(Length.millimeters(1)),
    )

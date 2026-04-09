from opensolid import Body3D, Convention3D, Length, Resolution, World3D

body = Body3D.sphere(center_point=World3D.origin_point, diameter=Length.centimeters(10))
resolution = Resolution.max_error(Length.millimeters(1))
body.write_stl("sphere.stl", Convention3D.y_up, resolution)

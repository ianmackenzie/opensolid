from opensolid import (
    Length,
    Body3d,
    Point2d,
    Angle,
    Axis2d,
    Region2d,
    Plane3d,
    Scene3d,
    Color,
    Mesh,
)


class Motor:
    def shaft_diameter(self):
        return self._shaft_diameter

    def body_diameter(self):
        return self._body_diameter

    def shaft_length(self):
        return self._shaft_length

    def shaft_axis(self):
        return self._mating_plane.normal_axis().reverse()

    def shaft_tip_plane(self):
        return self._mating_plane.flip_x().offset_by(self.shaft_length())

    def free_speed_rpm(self):
        return 5330

    def stall_torque_nm(self):
        return 2.41

    def scene_entity(self, resolution: Length) -> Scene3d.Entity:
        mesh_constraints = [Mesh.max_error(resolution)]
        cap_material = Scene3d.aluminum(0.3)
        housing_material = Scene3d.nonmetal(Color.black, 0.2)
        shaft_material = Scene3d.silver(0.2)
        return Scene3d.group(
            [
                Scene3d.body(mesh_constraints, cap_material, self._back_cap),
                Scene3d.body(mesh_constraints, housing_material, self._housing),
                Scene3d.body(mesh_constraints, cap_material, self._front_cap),
                Scene3d.body(mesh_constraints, shaft_material, self._shaft),
            ]
        )

    def __init__(self, mating_plane: Plane3d):
        self._mating_plane = mating_plane
        self._shaft_diameter = Length.millimeters(5)
        self._shaft_length = Length.millimeters(20)

        self._body_diameter = Length.centimeters(4)
        body_length = Length.centimeters(8)
        cap_thickness = Length.millimeters(10)

        body_fillet_radius = Length.millimeters(1)
        shaft_fillet_radius = Length.millimeters(0.5)

        body_radius = self.body_diameter() / 2
        shaft_radius = self.shaft_diameter() / 2

        axis = mating_plane.normal_axis().reverse()
        sketch_plane = Plane3d.from_x_axis(axis)

        def body_cylinder(x1: Length, x2: Length) -> Body3d:
            p1 = Point2d.x(x1)
            p2 = Point2d.x(x2)
            p3 = Point2d.xy(x2, body_radius)
            p4 = Point2d.xy(x1, body_radius)
            rectangle = Region2d.polygon([p1, p2, p3, p4])
            profile = rectangle.fillet([p3, p4], radius=body_fillet_radius)
            return Body3d.revolved(sketch_plane, profile, Axis2d.x, Angle.two_pi)

        def shaft_cylinder() -> Body3d:
            p1 = Point2d.origin
            p2 = Point2d.x(self.shaft_length())
            p3 = Point2d.xy(self.shaft_length(), shaft_radius)
            p4 = Point2d.y(shaft_radius)
            profile = Region2d.polygon([p1, p2, p3, p4]).fillet(
                [p3],
                radius=shaft_fillet_radius,
            )
            return Body3d.revolved(sketch_plane, profile, Axis2d.x, Angle.two_pi)

        self._back_cap = body_cylinder(-body_length, -body_length + cap_thickness)
        self._housing = body_cylinder(-body_length + cap_thickness, -cap_thickness)
        self._front_cap = body_cylinder(-cap_thickness, Length.zero)
        self._shaft = shaft_cylinder()

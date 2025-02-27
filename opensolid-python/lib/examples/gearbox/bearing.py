from opensolid import (
    Length,
    Plane3d,
    Region2d,
    Body3d,
    Point2d,
    Curve2d,
    Angle,
    Axis2d,
    Scene3d,
    Mesh,
)
import math


class Bearing:
    def __init__(
        self,
        *,
        mating_plane: Plane3d,
        bore_diameter: Length,
        width: Length,
        outer_diameter: Length,
        flange_thickness: Length,
        flange_width: Length,
    ):
        self._mating_plane = mating_plane
        self._width = width
        self._flange_thickness = flange_thickness
        front_plane = mating_plane.offset_by(flange_thickness)
        back_plane = front_plane.offset_by(-width)
        sketch_plane = back_plane.ny_plane()

        y_outer = outer_diameter / 2
        y_flange = y_outer + flange_width
        y_bore = bore_diameter / 2
        t = (y_outer - y_bore) / 3
        y_race_outer = y_outer - t
        y_race_inner = y_bore + t
        ball_radius = t
        y_ball_center = (y_race_inner + y_race_outer) / 2
        x_mid = width / 2
        ball_center = Point2d.xy(x_mid, y_ball_center)

        i1 = Point2d.y(y_bore)
        i2 = Point2d.xy(width, y_bore)
        i3 = Point2d.xy(width, y_race_inner)
        i4 = Point2d.y(y_race_inner)
        inner_groove = Curve2d.polar_arc(
            ball_center, ball_radius, Angle.degrees(330), Angle.degrees(210)
        )
        inner_race_curves = [
            Curve2d.line(i1, i2),
            Curve2d.line(i2, i3),
            Curve2d.line(i3, inner_groove.start_point()),
            inner_groove,
            Curve2d.line(inner_groove.end_point(), i4),
            Curve2d.line(i4, i1),
        ]
        inner_race_profile = Region2d.bounded_by(inner_race_curves)

        o1 = Point2d.y(y_race_outer)
        o2 = Point2d.xy(width, y_race_outer)
        o3 = Point2d.xy(width, y_flange)
        o4 = Point2d.xy(width - flange_thickness, y_flange)
        o5 = Point2d.xy(width - flange_thickness, y_outer)
        o6 = Point2d.y(y_outer)
        outer_groove = Curve2d.polar_arc(
            ball_center, ball_radius, Angle.degrees(150), Angle.degrees(30)
        )
        outer_race_curves = [
            Curve2d.line(o1, outer_groove.start_point()),
            outer_groove,
            Curve2d.line(outer_groove.end_point(), o2),
            Curve2d.line(o2, o3),
            Curve2d.line(o3, o4),
            Curve2d.line(o4, o5),
            Curve2d.line(o5, o6),
            Curve2d.line(o6, o1),
        ]
        outer_race_profile = Region2d.bounded_by(outer_race_curves)

        self._inner_race = Body3d.revolved(
            sketch_plane, inner_race_profile, Axis2d.x, Angle.full_turn
        )
        self._outer_race = Body3d.revolved(
            sketch_plane, outer_race_profile, Axis2d.x, Angle.full_turn
        )
        self._first_ball = Body3d.sphere(
            ball_center.place_on(sketch_plane), ball_radius
        )
        self._num_balls = math.floor(2 * math.pi * y_ball_center / (3 * ball_radius))

    def scene_entity(self, resolution: Length) -> Scene3d.Entity:
        mesh_constraints = [Mesh.max_error(resolution)]
        race_material = Scene3d.silver(0.2)
        ball_material = Scene3d.chromium(0.1)
        first_ball = Scene3d.body(mesh_constraints, ball_material, self._first_ball)
        balls = Scene3d.group(
            [
                first_ball.rotate_around(
                    self._mating_plane.normal_axis(),
                    Angle.degrees(360 * index / self._num_balls),
                )
                for index in range(self._num_balls)
            ]
        )
        return Scene3d.group(
            [
                Scene3d.body(mesh_constraints, race_material, self._inner_race),
                Scene3d.body(mesh_constraints, race_material, self._outer_race),
                balls,
            ]
        )

    def front_plane(self) -> Plane3d:
        return self._mating_plane.offset_by(self._flange_thickness)

    def back_plane(self) -> Plane3d:
        return self.front_plane().flip_x().offset_by(self._width)

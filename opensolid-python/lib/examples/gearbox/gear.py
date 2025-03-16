from opensolid import (
    Length,
    Plane3d,
    Axis3d,
    Body3d,
    Scene3d,
    Angle,
    AngleCurve,
    Axis2d,
    Point2d,
    Region2d,
    Mesh,
    LengthRange,
    Curve2d,
    Direction2d,
)
import math


class Gear:
    def module(self) -> Length:
        return self._module

    def num_teeth(self) -> int:
        return self._num_teeth

    def pitch_diameter(self) -> Length:
        return self.module() * self.num_teeth()

    def outer_diameter(self) -> Length:
        return self.pitch_diameter() + 2 * self.module()

    def width(self) -> Length:
        return self._width

    def midplane(self) -> Plane3d:
        return self._midplane

    def axis(self) -> Axis3d:
        return self.midplane().normal_axis()

    def scene_entity(self, resolution: Length) -> Scene3d.Entity:
        mesh_constraints = [Mesh.max_error(resolution)]
        return Scene3d.body(mesh_constraints, Scene3d.iron(0.3), self._body)

    def __init__(
        self,
        *,
        midplane: Plane3d,
        module: Length,
        num_teeth: int,
        width: Length,
        bore_diameter: Length,
    ):
        self._midplane = midplane
        self._module = module
        self._num_teeth = num_teeth
        self._width = width
        self._bore_diameter = bore_diameter

        phi = Angle.degrees(20)  # Pressure angle

        r0 = (module * num_teeth) / 2  # pitch radius
        rb = r0 * phi.cos()  # involute tooth profile base radius

        rd = r0 - 1.25 * module  # dedendum radius
        ra = r0 + module  # addendum radius

        theta1 = Angle.radians(math.sqrt((rd / rb) ** 2 - 1)) if rd > rb else Angle.zero
        theta2 = Angle.radians(math.sqrt((ra / rb) ** 2 - 1))
        theta = AngleCurve.line(theta1, theta2)
        alpha = Angle.radians(phi.tan() - phi.in_radians() + math.pi / (2 * num_teeth))
        x = rb * (
            (theta - alpha).sin() - (theta / Angle.radian) * (theta - alpha).cos()
        )
        y = rb * (
            (theta - alpha).cos() + (theta / Angle.radian) * (theta - alpha).sin()
        )
        involute_left = Curve2d.xy(x, y)
        left_start = involute_left.evaluate(0.0)
        left_end = involute_left.evaluate(1.0)
        involute_left_derivative = involute_left.derivative()
        left_start_tangent = (
            involute_left_derivative.evaluate(0.0).direction()
            if rd > rb
            else Direction2d.from_angle(Angle.quarter_turn + alpha)
        )
        left_end_tangent = involute_left_derivative.evaluate(1.0).direction()
        left_derivative_magnitude = left_start.distance_to(left_end)
        left_approximation = Curve2d.hermite(
            left_start,
            [left_derivative_magnitude * left_start_tangent],
            left_end,
            [left_derivative_magnitude * left_end_tangent],
        )

        right_approximation = left_approximation.mirror_across(Axis2d.y)
        tip = Curve2d.line(
            left_approximation.end_point(),
            right_approximation.end_point(),
        )
        angular_spacing = Angle.two_pi / num_teeth
        next_tooth_start = right_approximation.start_point().rotate_around(
            Point2d.origin,
            angular_spacing,
        )
        connector = (
            Curve2d.line(left_start, next_tooth_start)
            if rd > rb
            else Curve2d.arc(left_start, next_tooth_start, -Angle.pi)
        )
        tooth_profile = [
            left_approximation,
            right_approximation,
            tip,
            connector,
        ]

        def rotated_profile(index: int) -> list[Curve2d]:
            angle = index * angular_spacing
            return [
                curve.rotate_around(Point2d.origin, angle) for curve in tooth_profile
            ]

        outer_profile = [
            curve for i in range(num_teeth) for curve in rotated_profile(i)
        ]
        hole = Curve2d.circle(center_point=Point2d.origin, diameter=bore_diameter)
        profile_curves = [*outer_profile, hole]

        profile = Region2d.bounded_by(profile_curves)

        body = Body3d.extruded(midplane, profile, LengthRange(-width / 2, width / 2))
        self._body = body

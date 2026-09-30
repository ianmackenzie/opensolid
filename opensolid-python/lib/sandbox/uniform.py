from opensolid import Curve2D, Length, Parameter, Point2D, Resolution, Svg


def dot(point: Point2D) -> Svg:
    return Svg.circle_with(
        center_point=point, diameter=Length.millimeters(3)
    ).with_attributes([Svg.white_fill])


p1 = Point2D.origin
p2 = Point2D.origin
p3 = Point2D.centimeters(10, 10)
p4 = Point2D.centimeters(20, 0)
curve = Curve2D.bezier([p1, p2, p3, p4])
resolution = Resolution.max_error(Length.millimeters(0.1))
dots = Svg.group([dot(curve.at_uniform(t)) for t in Parameter.steps(20)])
drawing = Svg.group([Svg.curve(resolution, curve), dots])
drawing.write("uniform.svg", Svg.padding(Length.centimeters(1)))

from opensolid import Circle2D, Curve2D, Length, Parameter, Point2D, Resolution, Svg

if __name__ == "__main__":
    p1 = Point2D.origin
    p2 = Point2D.origin
    p3 = Point2D.centimeters(10, 10)
    p4 = Point2D.centimeters(20, 0)
    curve = Curve2D.bezier([p1, p2, p3, p4])

    def dot(point: Point2D) -> Svg:
        circle = Circle2D(center_point=point, diameter=Length.millimeters(3))
        attributes = [Svg.white_fill]
        return Svg.circle(circle).with_attributes(attributes)

    resolution = Resolution.max_error(Length.millimeters(0.1))
    points = [curve.at_uniform(t) for t in Parameter.steps(20)]
    dots = [dot(point) for point in points]
    drawing = Svg.group([Svg.curve(resolution, curve), *dots])
    drawing.write("uniform.svg", Svg.padding(Length.centimeters(1)))

    print([curve.parameter_value.at_uniform(t) for t in Parameter.steps(10)])

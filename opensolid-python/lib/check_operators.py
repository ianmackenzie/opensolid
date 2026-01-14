from dataclasses import dataclass

from opensolid import (
    Angle,
    AngleCurve,
    AngleInterval,
    Area,
    AreaCurve,
    AreaInterval,
    AreaVector2D,
    Bounds2D,
    Curve,
    Direction2D,
    Displacement2D,
    Interval,
    Length,
    LengthCurve,
    LengthInterval,
    Point2D,
    Tolerance,
    UvBounds,
    UvPoint,
    Vector2D,
)

types = [
    float,
    Length,
    Area,
    Angle,
    Interval,
    LengthInterval,
    AreaInterval,
    AngleInterval,
    Vector2D,
    Displacement2D,
    AreaVector2D,
    Direction2D,
    Point2D,
    UvPoint,
    Bounds2D,
    UvBounds,
    Curve,
    LengthCurve,
    AreaCurve,
    AngleCurve,
]

dimension = {
    float: 1,
    Length: 1,
    Area: 1,
    Angle: 1,
    Interval: 1,
    LengthInterval: 1,
    AreaInterval: 1,
    AngleInterval: 1,
    Vector2D: 2,
    Displacement2D: 2,
    AreaVector2D: 2,
    Direction2D: 2,
    Point2D: 2,
    UvPoint: 2,
    Bounds2D: 2,
    UvBounds: 2,
    Curve: 1,
    LengthCurve: 1,
    AreaCurve: 1,
    AngleCurve: 1,
}

space = {
    float: None,
    Length: None,
    Area: None,
    Angle: None,
    Interval: None,
    LengthInterval: None,
    AreaInterval: None,
    AngleInterval: None,
    Vector2D: "Space",
    Displacement2D: "Space",
    AreaVector2D: "Space",
    Direction2D: "Space",
    Point2D: "Space",
    UvPoint: "UvSpace",
    Bounds2D: "Space",
    UvBounds: "UvSpace",
    Curve: None,
    LengthCurve: None,
    AreaCurve: None,
    AngleCurve: None,
}

behaviour = {
    float: "scalar",
    Length: "scalar",
    Area: "scalar",
    Angle: "scalar",
    Interval: "scalar",
    LengthInterval: "scalar",
    AreaInterval: "scalar",
    AngleInterval: "scalar",
    Vector2D: "vector",
    Displacement2D: "vector",
    AreaVector2D: "vector",
    Direction2D: "direction",
    Point2D: "point",
    UvPoint: "point",
    Bounds2D: "point",
    UvBounds: "point",
    Curve: "scalar",
    LengthCurve: "scalar",
    AreaCurve: "scalar",
    AngleCurve: "scalar",
}


@dataclass
class Units:
    rad: int = 0
    m: int = 0

    def __mul__(self, other):
        return Units(rad=self.rad + other.rad, m=self.m + other.m)

    def __truediv__(self, other):
        return Units(rad=self.rad - other.rad, m=self.m - other.m)


unitless = Units()
length_units = Units(m=1)
angle_units = Units(rad=1)
area_units = Units(m=2)

units = {
    float: unitless,
    Length: length_units,
    Area: area_units,
    Angle: angle_units,
    Interval: unitless,
    LengthInterval: length_units,
    AreaInterval: area_units,
    AngleInterval: angle_units,
    Vector2D: unitless,
    Displacement2D: length_units,
    AreaVector2D: area_units,
    Direction2D: unitless,
    Point2D: length_units,
    UvPoint: unitless,
    Bounds2D: length_units,
    UvBounds: unitless,
    Curve: unitless,
    LengthCurve: length_units,
    AreaCurve: area_units,
    AngleCurve: angle_units,
}

topology = {
    float: "value",
    Length: "value",
    Area: "value",
    Angle: "value",
    Interval: "bounds",
    LengthInterval: "bounds",
    AreaInterval: "bounds",
    AngleInterval: "bounds",
    Vector2D: "value",
    Displacement2D: "value",
    AreaVector2D: "value",
    Direction2D: "value",
    Point2D: "value",
    UvPoint: "value",
    Bounds2D: "bounds",
    UvBounds: "bounds",
    Curve: "curve",
    LengthCurve: "curve",
    AreaCurve: "curve",
    AngleCurve: "curve",
}

dummy_value = {
    float: 1.0,
    Length: Length.meters(1),
    Area: Area.square_meters(1),
    Angle: Angle.radian,
    Interval: Interval.constant(1),
    LengthInterval: LengthInterval.constant(Length.meters(1)),
    AreaInterval: AreaInterval.constant(Area.square_meters(1)),
    AngleInterval: AngleInterval.constant(Angle.radian),
    Vector2D: Vector2D.zero,
    Displacement2D: Displacement2D.zero,
    AreaVector2D: AreaVector2D.zero,
    Direction2D: Direction2D.x,
    Point2D: Point2D.origin,
    UvPoint: UvPoint.origin,
    Bounds2D: Bounds2D.constant(Point2D.origin),
    UvBounds: UvBounds.constant(UvPoint.origin),
    Curve: Curve.constant(1),
    LengthCurve: LengthCurve.constant(Length.meters(1)),
    AreaCurve: AreaCurve.constant(Area.square_meters(1)),
    AngleCurve: AngleCurve.constant(Angle.radian),
}


def get_output_topology(t1, t2):
    match (topology[t1], topology[t2]):
        case ("value", "value"):
            return "value"
        case ("value", "bounds") | ("bounds", "value") | ("bounds", "bounds"):
            return "bounds"
        case ("value", "curve") | ("curve", "value") | ("curve", "curve"):
            return "curve"
        case ("curve", "bounds") | ("bounds", "curve"):
            return None
        case _:
            assert False, (
                "Unexpected topologies " + topology[t1] + " and " + topology[t2]
            )


def find_output_type(
    output_behaviour, output_dimension, output_topology, output_units, output_space
):
    valid_output_types = [
        t
        for t in types
        if behaviour[t] == output_behaviour
        and dimension[t] == output_dimension
        and topology[t] == output_topology
        and units[t] == output_units
        and space[t] == output_space
        and behaviour[t] != "direction"
    ]
    match valid_output_types:
        case []:
            return None
        case [output_type]:
            return output_type
        case _:
            assert False, "Found more than one possible product type: " + ",".join(
                [t.__name__ for t in valid_output_types]
            )


def tolerance_value(units):
    if units == unitless:
        return 1e-9
    elif units == length_units:
        return Length.meters(1e-9)
    elif units == area_units:
        return Area.square_meters(1e-9)
    elif units == angle_units:
        return Angle.radians(1e-9)
    else:
        assert False, "Unrecognized units type: " + str(units)


if __name__ == "__main__":
    # Check addition
    for t1 in types:
        for t2 in types:
            # Determine output 'behaviour'
            match (behaviour[t1], behaviour[t2]):
                case ("scalar", "scalar"):
                    output_behaviour = "scalar"
                case ("vector", "vector"):
                    output_behaviour = "vector"
                case ("point", "vector"):
                    output_behaviour = "point"
                case ("vector", "point"):
                    continue  # Can't add a point to a vector
                case ("point", "point"):
                    continue  # Can't add two points
                case ("scalar", "vector") | ("vector", "scalar"):
                    continue  # Can't add scalars and vectors
                case ("scalar", "point") | ("point", "scalar"):
                    continue  # Can't add scalars and points
                case ("direction", _) | (_, "direction"):
                    continue  # Can't add directions
                case _:
                    assert False, (
                        "Unexpected behaviours "
                        + behaviour[t1]
                        + " and "
                        + behaviour[t2]
                    )

            # Check if dimensions are equal
            if dimension[t1] == dimension[t2]:
                output_dimension = dimension[t1]
            else:
                continue

            # Determine output topology
            output_topology = get_output_topology(t1, t2)
            if output_topology is None:
                continue

            # Check if units are equal
            if units[t1] == units[t2]:
                output_units = units[t1]
            else:
                continue

            # Check if spaces are equal
            if space[t1] == space[t2]:
                output_space = space[t1]
            else:
                print("Skipping addition of " + str(t1) + " and " + str(t2))
                continue

            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
                output_space=output_space,
            )
            if output_type is None:
                continue
            dummy1 = dummy_value[t1]
            dummy2 = dummy_value[t2]
            try:
                print("Checking addition of " + t1.__name__ + " and " + t2.__name__)
                sum = dummy1 + dummy2
            except Exception:
                print("Failed to add " + t1.__name__ + " and " + t2.__name__)
                continue
            if not isinstance(sum, output_type):
                print(
                    "Product of "
                    + t1.__name__
                    + " and "
                    + t2.__name__
                    + " has unexpected type "
                    + type(sum).__name__
                    + " (expected "
                    + output_type.__name__
                    + ")"
                )

    # Check subtraction
    for t1 in types:
        for t2 in types:
            # Determine output 'behaviour'
            match (behaviour[t1], behaviour[t2]):
                case ("scalar", "scalar"):
                    output_behaviour = "scalar"
                case ("vector", "vector"):
                    output_behaviour = "vector"
                case ("point", "vector"):
                    output_behaviour = "point"
                case ("vector", "point"):
                    continue  # Can't subtract a point from a vector
                case ("point", "point"):
                    output_behaviour = "vector"
                case ("scalar", "vector") | ("vector", "scalar"):
                    continue  # Can't subtract scalars and vectors
                case ("scalar", "point") | ("point", "scalar"):
                    continue  # Can't subtract scalars and points
                case ("direction", _) | (_, "direction"):
                    continue  # Can't subtract directions
                case _:
                    assert False, (
                        "Unexpected behaviours "
                        + behaviour[t1]
                        + " and "
                        + behaviour[t2]
                    )

            # Check if dimensions are equal
            if dimension[t1] == dimension[t2]:
                output_dimension = dimension[t1]
            else:
                continue

            # Determine output topology
            output_topology = get_output_topology(t1, t2)
            if output_topology is None:
                continue

            # Check if units are equal
            if units[t1] == units[t2]:
                output_units = units[t1]
            else:
                continue

            # Check if spaces are equal
            if space[t1] == space[t2]:
                output_space = space[t1]
            else:
                print("Skipping subtraction of " + str(t1) + " and " + str(t2))
                continue

            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
                output_space=output_space,
            )
            if output_type is None:
                continue
            dummy1 = dummy_value[t1]
            dummy2 = dummy_value[t2]
            try:
                print("Checking subtraction of " + t1.__name__ + " and " + t2.__name__)
                difference = dummy1 - dummy2
            except Exception:
                print("Failed to subtract " + t1.__name__ + " and " + t2.__name__)
                continue
            if not isinstance(difference, output_type):
                print(
                    "Product of "
                    + t1.__name__
                    + " and "
                    + t2.__name__
                    + " has unexpected type "
                    + type(difference).__name__
                    + " (expected "
                    + output_type.__name__
                    + ")"
                )

    # Check multiplication
    for t1 in types:
        for t2 in types:
            # Determine output 'behaviour'
            match (behaviour[t1], behaviour[t2]):
                case ("point", _) | (_, "point"):
                    continue  # Can't multiply point-like things
                case ("scalar", "scalar"):
                    output_behaviour = "scalar"
                case (
                    ("scalar", "vector")
                    | ("vector", "scalar")
                    | ("scalar", "direction")
                    | ("direction", "scalar")
                ):
                    output_behaviour = "vector"
                case (
                    ("vector", "vector")
                    | ("vector", "direction")
                    | ("direction", "vector")
                    | ("direction", "direction")
                ):
                    continue  # Can't multiply two vector-like things
                case _:
                    assert False, "Unexpected behaviours"

            # Determine output dimension, as long as at least one input dimension is 1
            if dimension[t1] == 1 or dimension[t2] == 1:
                output_dimension = dimension[t1] * dimension[t2]
            else:
                continue

            # Determine output topology
            output_topology = get_output_topology(t1, t2)
            if output_topology is None:
                continue

            # Determine output units
            output_units = units[t1] * units[t2]

            if space[t1] is None or space[t2] is None:
                output_space = space[t1] or space[t2]
            else:
                assert False, "Expected at least one product operand to be non-spatial"

            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
                output_space=output_space,
            )
            if output_type is None:
                continue
            dummy1 = dummy_value[t1]
            dummy2 = dummy_value[t2]
            try:
                print("Checking product of " + t1.__name__ + " and " + t2.__name__)
                product = dummy1 * dummy2
            except Exception:
                print("Failed to multiply " + t1.__name__ + " and " + t2.__name__)
                continue
            if not isinstance(product, output_type):
                print(
                    "Product of "
                    + t1.__name__
                    + " and "
                    + t2.__name__
                    + " has unexpected type "
                    + type(product).__name__
                    + " (expected "
                    + output_type.__name__
                    + ")"
                )

    # Check division
    for t1 in types:
        for t2 in types:
            # Determine output 'behaviour'
            match (behaviour[t1], behaviour[t2]):
                case ("direction", _) | (_, "direction"):
                    continue  # Can't divide directions
                case ("point", _) | (_, "point"):
                    continue  # Can't divide point-like things
                case ("scalar", "scalar"):
                    output_behaviour = "scalar"
                case ("vector", "scalar"):
                    output_behaviour = "vector"
                case ("scalar", "vector"):
                    continue  # Can't divide scalar by vector
                case ("vector", "vector"):
                    continue  # Can't divide two vector-like things
                case _:
                    assert False, "Unexpected behaviours"

            # Determine output dimension
            if dimension[t2] == 1:
                output_dimension = dimension[t1]
            else:
                continue  # Can only divide if second dimension is 1

            # Determine output topology
            output_topology = get_output_topology(t1, t2)
            if output_topology is None:
                continue

            # Determine output units
            output_units = units[t1] / units[t2]

            assert space[t2] is None, "Expected quotient denominator to be non-spatial"

            # Check if there actually is a valid output type
            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
                output_space=space[t1],
            )
            if output_type is None:
                continue
            dummy1 = dummy_value[t1]
            dummy2 = dummy_value[t2]
            try:
                print("Checking quotient of " + t1.__name__ + " and " + t2.__name__)
                with Tolerance(tolerance_value(units[t2])):
                    quotient = dummy1 / dummy2
            except Exception:
                print("Failed to divide " + t1.__name__ + " and " + t2.__name__)
                continue
            if not isinstance(quotient, output_type):
                print(
                    "Quotient of "
                    + t1.__name__
                    + " and "
                    + t2.__name__
                    + " has unexpected type "
                    + type(quotient).__name__
                    + " (expected "
                    + output_type.__name__
                    + ")"
                )

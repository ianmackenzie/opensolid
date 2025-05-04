from opensolid import (
    Length,
    Area,
    Angle,
    Bounds,
    LengthBounds,
    AreaBounds,
    AngleBounds,
    Vector2d,
    Displacement2d,
    AreaVector2d,
    Direction2d,
    Point2d,
    UvPoint,
    Bounds2d,
    UvBounds,
    Curve,
    LengthCurve,
    AreaCurve,
    AngleCurve,
)
from dataclasses import dataclass

types = [
    float,
    Length,
    Area,
    Angle,
    Bounds,
    LengthBounds,
    AreaBounds,
    AngleBounds,
    Vector2d,
    Displacement2d,
    AreaVector2d,
    Direction2d,
    Point2d,
    UvPoint,
    Bounds2d,
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
    Bounds: 1,
    LengthBounds: 1,
    AreaBounds: 1,
    AngleBounds: 1,
    Vector2d: 2,
    Displacement2d: 2,
    AreaVector2d: 2,
    Direction2d: 2,
    Point2d: 2,
    UvPoint: 2,
    Bounds2d: 2,
    UvBounds: 2,
    Curve: 1,
    LengthCurve: 1,
    AreaCurve: 1,
    AngleCurve: 1,
}

behaviour = {
    float: "scalar",
    Length: "scalar",
    Area: "scalar",
    Angle: "scalar",
    Bounds: "scalar",
    LengthBounds: "scalar",
    AreaBounds: "scalar",
    AngleBounds: "scalar",
    Vector2d: "vector",
    Displacement2d: "vector",
    AreaVector2d: "vector",
    Direction2d: "direction",
    Point2d: "point",
    UvPoint: "point",
    Bounds2d: "point",
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
    Bounds: unitless,
    LengthBounds: length_units,
    AreaBounds: area_units,
    AngleBounds: angle_units,
    Vector2d: unitless,
    Displacement2d: length_units,
    AreaVector2d: area_units,
    Direction2d: unitless,
    Point2d: length_units,
    UvPoint: unitless,
    Bounds2d: length_units,
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
    Bounds: "bounds",
    LengthBounds: "bounds",
    AreaBounds: "bounds",
    AngleBounds: "bounds",
    Vector2d: "value",
    Displacement2d: "value",
    AreaVector2d: "value",
    Direction2d: "value",
    Point2d: "value",
    UvPoint: "value",
    Bounds2d: "bounds",
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
    Bounds: Bounds.constant(1),
    LengthBounds: LengthBounds.constant(Length.meters(1)),
    AreaBounds: AreaBounds.constant(Area.square_meters(1)),
    AngleBounds: AngleBounds.constant(Angle.radian),
    Vector2d: Vector2d.zero,
    Displacement2d: Displacement2d.zero,
    AreaVector2d: AreaVector2d.zero,
    Direction2d: Direction2d.x,
    Point2d: Point2d.origin,
    UvPoint: UvPoint.origin,
    Bounds2d: Bounds2d.constant(Point2d.origin),
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


def find_output_type(output_behaviour, output_dimension, output_topology, output_units):
    valid_output_types = [
        t
        for t in types
        if behaviour[t] == output_behaviour
        and dimension[t] == output_dimension
        and topology[t] == output_topology
        and units[t] == output_units
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

            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
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

            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
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

            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
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

            # Check if there actually is a valid output type
            # Check if there actually is a valid output type
            output_type = find_output_type(
                output_behaviour=output_behaviour,
                output_dimension=output_dimension,
                output_topology=output_topology,
                output_units=output_units,
            )
            if output_type is None:
                continue
            dummy1 = dummy_value[t1]
            dummy2 = dummy_value[t2]
            try:
                print("Checking quotient of " + t1.__name__ + " and " + t2.__name__)
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

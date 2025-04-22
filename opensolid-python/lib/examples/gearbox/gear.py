from opensolid import (
    Length,
    Plane3d,
    Axis3d,
    Body3d,
    Scene3d,
    Point2d,
    Region2d,
    Mesh,
    LengthRange,
    Curve2d,
    SpurGear,
)


class Gear:
    _gear: SpurGear
    _midplane: Plane3d
    _width: Length
    _bore_diameter: Length

    def __init__(
        self,
        *,
        midplane: Plane3d,
        module: Length,
        num_teeth: int,
        width: Length,
        bore_diameter: Length,
    ):
        spur_gear = SpurGear.metric(module=module, num_teeth=num_teeth)

        self._gear = spur_gear
        self._midplane = midplane
        self._width = width
        self._bore_diameter = bore_diameter

        outer_profile = spur_gear.profile()
        hole = Curve2d.circle(center_point=Point2d.origin, diameter=bore_diameter)
        profile_curves = [*outer_profile, hole]

        profile = Region2d.bounded_by(profile_curves)

        body = Body3d.extruded(midplane, profile, LengthRange(-width / 2, width / 2))
        self._body = body

    def module(self) -> Length:
        return self._gear.module()

    def num_teeth(self) -> int:
        return self._gear.num_teeth()

    def pitch_diameter(self) -> Length:
        return self._gear.pitch_diameter()

    def outer_diameter(self) -> Length:
        return self._gear.outer_diameter()

    def width(self) -> Length:
        return self._width

    def midplane(self) -> Plane3d:
        return self._midplane

    def axis(self) -> Axis3d:
        return self.midplane().normal_axis()

    def body(self) -> Body3d:
        return self._body

    def scene_entity(self, resolution: Length) -> Scene3d.Entity:
        mesh_constraints = [Mesh.max_error(resolution)]
        return Scene3d.body(mesh_constraints, Scene3d.iron(roughness=0.3), self._body)

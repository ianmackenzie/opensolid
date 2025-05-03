from opensolid import (
    Length,
    Body3d,
    Point2d,
    Curve2d,
    Displacement2d,
    Direction3d,
    Angle,
    Region2d,
    Plane3d,
    Mesh,
    LengthBounds,
    Scene3d,
    Tolerance,
    Color,
)
from motor import Motor
from gear import Gear
from bearing import Bearing

gear_width = Length.millimeters(8)
resolution = Length.millimeters(0.1)
gear_module = Length.millimeters(1)
gear_pressure_angle = Angle.degrees(20)
motor_gear_teeth = 16
mating_gear_teeth = 48
output_shaft_diameter = Length.millimeters(6)
plate_thickness = Length.millimeters(4)
bearing_width = Length.millimeters(8)
bearing_outer_diameter = 3 * output_shaft_diameter
bearing_flange_thickness = Length.millimeters(1)
bearing_flange_width = Length.millimeters(1)
plate_distance = Length.centimeters(2.5)
standoff_diameter = Length.millimeters(6)
standoff_clearance = Length.millimeters(4)
plate_ear_diameter = Length.millimeters(10)
plate_ear_fillet_radius = Length.millimeters(5)
screw_diameter = Length.millimeters(4)
output_shaft_back_extension = Length.millimeters(2)
output_shaft_front_extension = Length.centimeters(2)
mesh_constraints = [Mesh.max_error(resolution)]


with Tolerance(Length.nanometers(1)):
    back_plate_back_plane = Plane3d.yz.flip_x()
    back_plate_front_plane = Plane3d.yz.offset_by(plate_thickness)

    motor = Motor(mating_plane=back_plate_back_plane)
    gear_midplane = motor.shaft_tip_plane().offset_by(
        -(Length.millimeters(1) + gear_width / 2)
    )
    motor_gear = Gear(
        midplane=gear_midplane,
        num_teeth=motor_gear_teeth,
        bore_diameter=motor.shaft_diameter(),
        module=gear_module,
        width=gear_width,
    )
    gear_center_distance = (motor_gear_teeth + mating_gear_teeth) * gear_module / 2
    mating_gear_axis = motor_gear.axis().translate_in(
        -Direction3d.z,
        gear_center_distance,
    )
    mating_gear_midplane = gear_midplane.translate_in(
        -Direction3d.z,
        gear_center_distance,
    ).rotate_around(
        mating_gear_axis,
        0.5 * Angle.two_pi / mating_gear_teeth,
    )
    mating_gear = Gear(
        midplane=mating_gear_midplane,
        num_teeth=mating_gear_teeth,
        bore_diameter=output_shaft_diameter,
        module=motor_gear.module(),
        width=motor_gear.width(),
    )
    back_bearing = Bearing(
        mating_plane=back_plate_front_plane.translate_in(
            -Direction3d.z,
            gear_center_distance,
        ),
        bore_diameter=output_shaft_diameter,
        width=bearing_width,
        outer_diameter=bearing_outer_diameter,
        flange_thickness=bearing_flange_thickness,
        flange_width=bearing_flange_width,
    )
    front_plate_back_plane = back_plate_front_plane.offset_by(plate_distance).flip_x()
    front_plate_front_plane = front_plate_back_plane.flip_x().offset_by(plate_thickness)

    front_bearing = Bearing(
        mating_plane=front_plate_back_plane.translate_in(
            -Direction3d.z,
            gear_center_distance,
        ),
        bore_diameter=output_shaft_diameter,
        width=bearing_width,
        outer_diameter=bearing_outer_diameter,
        flange_thickness=bearing_flange_thickness,
        flange_width=bearing_flange_width,
    )

    output_shaft = Body3d.cylinder_along(
        back_bearing.back_plane().normal_axis().reverse(),
        LengthBounds(
            -output_shaft_back_extension,
            back_bearing.back_plane()
            .origin_point()
            .distance_to(front_bearing.back_plane().origin_point())
            + output_shaft_front_extension,
        ),
        diameter=output_shaft_diameter,
    )

    motor_shaft_hole = Curve2d.circle(
        center_point=Point2d.origin,
        diameter=motor.shaft_diameter() + Length.millimeters(2),
    )
    output_shaft_center = Point2d.y(-gear_center_distance)
    bearing_hole = Curve2d.circle(
        center_point=output_shaft_center,
        diameter=bearing_outer_diameter,
    )
    motor_shaft_screw_distance = (
        motor.body_diameter() / 2 + standoff_clearance + standoff_diameter / 2
    )
    top_left_screw_center = Point2d.origin + Displacement2d.polar(
        motor_shaft_screw_distance,
        Angle.degrees(135),
    )
    top_right_screw_center = Point2d.origin + Displacement2d.polar(
        motor_shaft_screw_distance,
        Angle.degrees(45),
    )
    output_shaft_screw_distance = (
        mating_gear.outer_diameter() / 2 + standoff_clearance + standoff_diameter / 2
    )
    bottom_left_screw_center = output_shaft_center + Displacement2d.polar(
        output_shaft_screw_distance,
        Angle.degrees(225),
    )
    bottom_right_screw_center = output_shaft_center + Displacement2d.polar(
        output_shaft_screw_distance,
        Angle.degrees(315),
    )
    ear_radius = plate_ear_diameter / 2
    top_left_ear = Curve2d.polar_arc(
        center_point=top_left_screw_center,
        radius=ear_radius,
        start_angle=Angle.degrees(45),
        end_angle=Angle.degrees(225),
    )
    top_right_ear = Curve2d.polar_arc(
        center_point=top_right_screw_center,
        radius=ear_radius,
        start_angle=Angle.degrees(-45),
        end_angle=Angle.degrees(135),
    )
    bottom_left_ear = Curve2d.polar_arc(
        center_point=bottom_left_screw_center,
        radius=ear_radius,
        start_angle=Angle.degrees(135),
        end_angle=Angle.degrees(315),
    )
    bottom_right_ear = Curve2d.polar_arc(
        center_point=bottom_right_screw_center,
        radius=ear_radius,
        start_angle=Angle.degrees(-135),
        end_angle=Angle.degrees(45),
    )

    def screw_hole(point) -> Curve2d:
        return Curve2d.circle(center_point=point, diameter=screw_diameter)

    common_curves = [
        top_left_ear,
        top_right_ear,
        bottom_left_ear,
        bottom_right_ear,
        Curve2d.line(top_right_ear.end_point(), top_left_ear.start_point()),
        Curve2d.line(top_left_ear.end_point(), bottom_left_ear.start_point()),
        Curve2d.line(bottom_left_ear.end_point(), bottom_right_ear.start_point()),
        Curve2d.line(bottom_right_ear.end_point(), top_right_ear.start_point()),
        bearing_hole,
        screw_hole(top_left_screw_center),
        screw_hole(top_right_screw_center),
        screw_hole(bottom_left_screw_center),
        screw_hole(bottom_right_screw_center),
    ]

    fillet_points = [
        top_left_ear.start_point(),
        top_left_ear.end_point(),
        top_right_ear.start_point(),
        top_right_ear.end_point(),
        bottom_left_ear.start_point(),
        bottom_left_ear.end_point(),
        bottom_right_ear.start_point(),
        bottom_right_ear.end_point(),
    ]

    back_plate_profile = Region2d.bounded_by(
        [*common_curves, motor_shaft_hole],
    ).fillet(
        fillet_points,
        radius=plate_ear_fillet_radius,
    )

    front_plate_profile = Region2d.bounded_by(
        common_curves,
    ).fillet(
        fillet_points,
        radius=plate_ear_fillet_radius,
    )

    back_plate = Body3d.extruded(
        back_plate_front_plane,
        back_plate_profile,
        LengthBounds.zero_to(-plate_thickness),
    )
    front_plate = Body3d.extruded(
        front_plate_back_plane,
        front_plate_profile,
        LengthBounds.zero_to(-plate_thickness),
    )
    plate_material = Scene3d.aluminum(roughness=0.3)

    standoff_profile = Region2d.polygon(
        [
            Point2d.origin
            + Displacement2d.polar(standoff_diameter / 2, Angle.two_pi * i / 6)
            for i in range(6)
        ]
    )
    standoff_sketch_planes = [
        back_plate_front_plane.move_to(point.place_on(back_plate_front_plane))
        for point in [
            top_left_screw_center,
            top_right_screw_center,
            bottom_left_screw_center,
            bottom_right_screw_center,
        ]
    ]
    standoff_material = Scene3d.aluminum(roughness=0.3)
    standoffs = [
        Body3d.extruded(plane, standoff_profile, LengthBounds.zero_to(plate_distance))
        for plane in standoff_sketch_planes
    ]

    def screwhead(plane: Plane3d, point: Point2d) -> Scene3d.Entity:
        profile = Region2d.circle(center_point=point, diameter=Length.millimeters(5))
        body = Body3d.extruded(
            plane,
            profile,
            LengthBounds.zero_to(Length.millimeters(3)),
        )
        return Scene3d.body(
            mesh_constraints,
            Scene3d.nonmetal(Color.black, roughness=0.5),
            body,
        )

    scene = [
        motor.scene_entity(resolution),
        motor_gear.scene_entity(resolution),
        mating_gear.scene_entity(resolution),
        back_bearing.scene_entity(resolution),
        front_bearing.scene_entity(resolution),
        Scene3d.body(mesh_constraints, Scene3d.chromium(roughness=0.2), output_shaft),
        Scene3d.body(mesh_constraints, plate_material, back_plate),
        Scene3d.body(mesh_constraints, plate_material, front_plate),
        Scene3d.group(
            [
                Scene3d.body(mesh_constraints, standoff_material, standoff)
                for standoff in standoffs
            ]
        ),
        screwhead(front_plate_front_plane, top_left_screw_center),
        screwhead(front_plate_front_plane, top_right_screw_center),
        screwhead(front_plate_front_plane, bottom_left_screw_center),
        screwhead(front_plate_front_plane, bottom_right_screw_center),
        screwhead(back_plate_back_plane, top_left_screw_center),
        screwhead(back_plate_back_plane, top_right_screw_center),
        screwhead(back_plate_back_plane, bottom_left_screw_center),
        screwhead(back_plate_back_plane, bottom_right_screw_center),
    ]
    Scene3d.write_glb("gearbox.glb", Plane3d.xy, scene)

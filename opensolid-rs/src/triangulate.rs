use poly2tri_rs::{Point, SweeperBuilder};
use std::cmp::{Eq, PartialEq};
use std::{collections::HashMap, hash::Hash};

struct HashablePoint(Point);

impl PartialEq for HashablePoint {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&(*other).0)
    }
}

impl Eq for HashablePoint {}

impl Hash for HashablePoint {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.x.to_bits().hash(state);
        self.0.y.to_bits().hash(state);
    }
}

#[no_mangle]
pub extern "C" fn opensolid_polygon2d_triangulate(
    point_count: usize,
    point_data: *mut Point,
    hole_count: usize,
    hole_data: *mut usize,
    triangle_count: usize,
    triangle_data: *mut usize,
) -> usize {
    let points = unsafe { std::slice::from_raw_parts(point_data, point_count) };
    let hole_indices = unsafe { std::slice::from_raw_parts(hole_data, hole_count) };
    let outer_loop_end_index = if hole_count == 0 {
        point_count
    } else {
        hole_indices[0]
    };
    let outer_loop_points = Vec::from(&points[0..outer_loop_end_index]);
    let mut builder = SweeperBuilder::new(outer_loop_points);

    for i in 0..hole_count {
        let hole_index = hole_indices[i];
        let hole_end_index = if i < hole_count - 1 {
            hole_indices[i + 1]
        } else {
            point_count
        };
        if hole_end_index == hole_index + 1 {
            builder = builder.add_steiner_point(points[hole_index]);
        } else {
            let hole_vertices = Vec::from(&points[hole_index..hole_end_index]);
            builder = builder.add_hole(hole_vertices);
        }
    }

    let sweeper = builder.build();
    let triangulation = sweeper.triangulate();
    let mut index_map = HashMap::new();
    for i in 0..point_count {
        index_map.insert(HashablePoint(points[i]), i);
    }
    let triangle_indices =
        unsafe { std::slice::from_raw_parts_mut(triangle_data, 3 * triangle_count) };
    let mut triangle_index = 0;
    for triangle in triangulation {
        assert!(triangle_index < triangle_count);
        for i in 0..3 {
            triangle_indices[3 * triangle_index + i] =
                index_map[&HashablePoint(triangle.points[i])];
        }
        triangle_index += 1;
    }
    assert!(triangle_index == triangle_count);
    triangle_count
}

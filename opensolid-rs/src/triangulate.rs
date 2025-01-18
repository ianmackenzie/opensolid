use spade::{
    handles::FixedFaceHandle, ConstrainedDelaunayTriangulation, Point2, RefinementParameters,
    Triangulation,
};
use std::collections::{HashMap, HashSet};

type Point = Point2<f64>;

unsafe fn make_vec<T: Clone>(count: usize, data: *mut T) -> Vec<T> {
    Vec::from(std::slice::from_raw_parts(data, count))
}

#[no_mangle]
pub extern "C" fn opensolid_cdt(
    input_point_count: usize,
    input_point_data: *mut Point,
    input_edge_count: usize,
    input_edge_data: *mut [usize; 2],
    max_refinement_points: usize,
    output_point_count: *mut usize,
    output_point_data: *mut Point,
    output_triangle_count: *mut usize,
    output_triangle_data: *mut [usize; 3],
) {
    let input_points = unsafe { make_vec(input_point_count, input_point_data) };
    let input_edges = unsafe { make_vec(input_edge_count, input_edge_data) };
    match ConstrainedDelaunayTriangulation::<Point>::bulk_load_cdt(input_points, input_edges) {
        Ok(mut cdt) => {
            let refinement_parameters = RefinementParameters::<f64>::new()
                .exclude_outer_faces(true)
                .keep_constraint_edges()
                .with_max_additional_vertices(max_refinement_points);
            let refinement_result = cdt.refine(refinement_parameters);
            let excluded_face_ids: HashSet<usize> = refinement_result
                .excluded_faces
                .iter()
                .map(FixedFaceHandle::index)
                .collect();
            let mut triangle_count: usize = 0;
            let mut point_count: usize = 0;
            let mut point_indices: HashMap<usize, usize> = HashMap::new();
            for face_handle in cdt.inner_faces() {
                if excluded_face_ids.contains(&face_handle.index()) {
                    continue;
                };
                let triangle_data = unsafe { output_triangle_data.offset(triangle_count as isize) };
                for (i, vertex_handle) in face_handle.vertices().iter().enumerate() {
                    let vertex_index = vertex_handle.index();
                    let point_index = match point_indices.get(&vertex_index) {
                        Some(existing_index) => *existing_index,
                        None => {
                            let new_index = point_count;
                            point_indices.insert(vertex_index, new_index);
                            let point = cdt.vertex(vertex_handle.fix()).position();
                            unsafe { *output_point_data.offset(new_index as isize) = point };
                            point_count += 1;
                            new_index
                        }
                    };
                    unsafe { (*triangle_data)[i] = point_index };
                }
                triangle_count += 1;
            }
            unsafe { *output_point_count = point_count };
            unsafe { *output_triangle_count = triangle_count };
        }
        Err(_) => {
            unsafe { *output_point_count = 0 };
            unsafe { *output_triangle_count = 0 };
        }
    }
}

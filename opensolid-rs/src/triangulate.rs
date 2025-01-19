use spade::{
    handles::{FixedFaceHandle, FixedVertexHandle},
    ConstrainedDelaunayTriangulation, Point2, RefinementParameters, Triangulation,
};
use std::collections::HashSet;

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
    input_max_refinement_point_count: usize,
    output_refinement_point_count: *mut usize,
    output_refinement_point_data: *mut Point,
    output_triangle_count: *mut usize,
    output_triangle_data: *mut [usize; 3],
) {
    let input_points = unsafe { make_vec(input_point_count, input_point_data) };
    let input_edges = unsafe { make_vec(input_edge_count, input_edge_data) };
    match ConstrainedDelaunayTriangulation::<Point>::bulk_load_cdt_stable(input_points, input_edges)
    {
        Ok(mut cdt) => {
            let refinement_parameters = RefinementParameters::<f64>::new()
                .exclude_outer_faces(true)
                .keep_constraint_edges()
                .with_max_additional_vertices(input_max_refinement_point_count);
            let excluded_face_ids: HashSet<usize> = cdt
                .refine(refinement_parameters)
                .excluded_faces
                .iter()
                .map(FixedFaceHandle::index)
                .collect();
            let mut triangle_count: usize = 0;
            for face_handle in cdt
                .inner_faces()
                .filter(|handle| !(excluded_face_ids.contains(&handle.index())))
            {
                let triangle_ptr = unsafe { output_triangle_data.offset(triangle_count as isize) };
                let vertex_handles = face_handle.vertices();
                for i in 0..3 {
                    unsafe { (*triangle_ptr)[i] = vertex_handles[i].index() };
                }
                triangle_count += 1;
            }
            unsafe { *output_triangle_count = triangle_count };
            for i in input_point_count..cdt.num_vertices() {
                let vertex_handle = FixedVertexHandle::from_index(i);
                let point = cdt.vertex(vertex_handle).position();
                let output_offset = (i - input_point_count) as isize;
                let output_ptr = unsafe { output_refinement_point_data.offset(output_offset) };
                unsafe { *output_ptr = point };
            }
            unsafe { *output_refinement_point_count = cdt.num_vertices() - input_point_count };
        }
        Err(_) => {
            unsafe { *output_refinement_point_count = 0 };
            unsafe { *output_triangle_count = 0 };
        }
    }
}

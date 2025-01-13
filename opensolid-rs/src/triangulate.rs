use earcutr::earcut;

#[no_mangle]
pub extern "C" fn opensolid_polygon2d_triangulate(
    vertex_count: usize,
    vertex_data: *mut f64,
    hole_count: usize,
    hole_indices: *mut usize,
    face_indices_ptr: *mut usize,
) -> usize {
    let vertices = unsafe { std::slice::from_raw_parts(vertex_data, 2 * vertex_count) };
    let holes = unsafe { std::slice::from_raw_parts(hole_indices, hole_count) };
    match earcut(&vertices, &holes, 2) {
        Ok(face_indices) => {
            let face_count = vertex_count + 2 * hole_count - 2;
            assert!(face_indices.len() == 3 * face_count);
            unsafe { std::ptr::copy(face_indices.as_ptr(), face_indices_ptr, face_indices.len()) };
            face_count
        }
        Err(_) => 0,
    }
}

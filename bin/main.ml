open Raytracer.Vector
open Raytracer.Ray

let hit_sphere center r radius =
  let oc = r.orig -- center in
  let a = dot_product r.dir r.dir in
  let b = 2.0 *. dot_product oc r.dir in
  let c = (dot_product oc oc) -. radius*.radius in
  let discriminant = b*.b -. 4.*.a*.c in  

  if discriminant < 0. then (-.1.0)
  else (~-.b -. (sqrt discriminant) /. (2. *. a))

let ray_color r =
  let t = hit_sphere (make_vec3 0. 0. ~-.1.) r 0.5 in
  if t > 0. 
    then
      let n = unit_vector ((at r t) -- make_vec3 0. 0. ~-.1.) in
      scale_vec3 (make_vec3 (n.x+.1.) (n.y+.1.) (n.z+.1.)) 0.5
    else
      let unit_direction = unit_vector r.dir in
      let a = 0.5 *. (unit_direction.y +. 1.0) in
      (scale_vec3 (make_vec3 1.0 1.0 1.0) (1.0 -. a)) ++ (scale_vec3 (make_vec3 0.5 0.7 1.0) a)

let () =
  let aspect_ratio = 16.0 /. 9.0 in
  let image_width = 760 in
  let image_height = int_of_float (float image_width /. aspect_ratio) in

  let focal_length = 1.0 in
  let viewport_height = 2.0 in
  let viewport_width = viewport_height *. (float image_width /. float image_height) in
  let camera_center = make_vec3 0. 0. 0. in

  (* Calculate the vectors across the horizontal and down the vertical viewport edges. *)
  let viewport_u = make_vec3 viewport_width 0. 0. in
  let viewport_v = make_vec3 0. (-.viewport_width/.aspect_ratio) 0. in

  (*  Calculate the horizontal and vertical delta vectors from pixel to pixel. *)
  let pixel_delta_u = div_vec3 viewport_u (float image_width) in
  let pixel_delta_v = div_vec3 viewport_v (float image_height) in

  (* Calculate the location of the upper left pixel. *)
  let viewport_upper_left = 
    camera_center 
    -- (make_vec3 0. 0. focal_length) 
    -- (div_vec3 viewport_u 2.) 
    -- (div_vec3 viewport_v 2.) in
  
  let pixel00_loc = viewport_upper_left ++ (scale_vec3 (pixel_delta_u ++ pixel_delta_v) 0.5) in

  (*write_ppm "output.ppm" image_width image_height;*)
  let oc = open_out "output.ppm" in
  (* Write PPM header *)
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;
  
  (* Write pixel data *)
  for j = 0 to image_height - 1 do
    (* Printf.printf "Scanlines remaining: %n\n" (image_height - j); *)
    for i = 0 to image_width - 1 do
      let pixel_center = pixel00_loc ++ (scale_vec3 pixel_delta_u (float i)) ++ (scale_vec3 pixel_delta_v (float j)) in
      let ray_direction = pixel_center -- camera_center in
      let r = {orig=camera_center; dir=ray_direction} in
      let pixel_color = ray_color r in

      Printf.fprintf oc "%d %d %d "
        (int_of_float (255. *. pixel_color.x))
        (int_of_float (255. *. pixel_color.y))
        (int_of_float (255. *. pixel_color.z));
    done;
    output_string oc "\n";
  done;

  print_endline "PPM file written."
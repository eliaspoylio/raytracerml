open Vector
open Ray

type shape = Circle | Square

type hit_record = { 
  mutable p: vec3;
  mutable normal: vec3;
  mutable t: float;
  mutable front_face: bool;
}

type sphere = { center: vec3; radius: float }

let empty_record = { p = empty_vec3; normal = empty_vec3; t = 0.; front_face = false }

let set_face_normal record ray outward_normal =
  record.front_face <- dot_product ray.dir outward_normal < 0.;
  record.normal <- if record.front_face then outward_normal else neg_vec3 outward_normal

let sphere_hit s r ray_tmin ray_tmax record =
  let oc = r.orig -- s.center in
  let a = length_squared r.dir in
  let half_b = dot_product oc r.dir in
  let c = length_squared oc -. s.radius*.s.radius in
  let discriminant = half_b*.half_b -. a*.c in

  match discriminant < 0.  with
  | true -> false
  | false ->
    let sqrtd = sqrt discriminant in
    let root1 = (~-.half_b -. sqrtd) /. a in
    let root2 = (~-.half_b +. sqrtd) /. a in
    match (root1 <= ray_tmin || ray_tmax <= root1) && (root2 <= ray_tmin || ray_tmax <= root2) with
    | true -> false
    | false ->
      record.t <- root1;
      record.p <- at r record.t;
      let outward_normal = div_vec3 (record.p -- s.center) s.radius in
      set_face_normal record r outward_normal;
      true

let hit shapes r ray_tmin ray_tmax record = 
  let temp_rec = empty_record in
  let hit_anything = ref false in
  let closest_so_far = ref ray_tmax in

  shapes
  |> List.iter (fun x ->
    match sphere_hit x r ray_tmin ray_tmax temp_rec with
    | true -> 
      hit_anything := true;
      closest_so_far := temp_rec.t;
      record.p <- temp_rec.p;
      record.normal <- temp_rec.normal;
      record.t <- temp_rec.t;
      record.front_face <- temp_rec.front_face;
    | false -> ()
    )
  |> ignore;

  hit_anything
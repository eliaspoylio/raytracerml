type vec3 = { x : float; y : float; z : float }

let make_vec3 x y z = { x; y; z }

let add_vec3 v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let (--) (v1: vec3) (v2: vec3) : vec3 =
  { x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z }

let (++) (v1: vec3) (v2: vec3) : vec3 =
  { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let scale_vec3 v scalar = { x = v.x *. scalar; y = v.y *. scalar; z = v.z *. scalar }

let div_vec3 v div = { x = v.x /. div; y = v.y /. div; z = v.z /. div }

let (//) (v1: vec3) (v2: vec3) : vec3 =
  { x = v1.x /. v2.x; y = v1.y /. v2.y; z = v1.z /. v2.z }

let dot_product v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let cross_product v1 v2 =
  {
    x = v1.y *. v2.z -. v1.z *. v2.y;
    y = v1.z *. v2.x -. v1.x *. v2.z;
    z = v1.x *. v2.y -. v1.y *. v2.x;
  }

let length_squared v =
  v.x *. v.x +. v.y *. v.y +. v.z *. v.z

let length v =
  sqrt (length_squared v)

let unit_vector v = div_vec3 v (length v)
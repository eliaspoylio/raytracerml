open Vector

type ray = { orig: vec3; dir: vec3 }

let at r t =
  r.orig ++ scale_vec3 r.dir t
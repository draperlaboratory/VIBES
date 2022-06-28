open Core_kernel

let (let-) x f = Result.bind x ~f

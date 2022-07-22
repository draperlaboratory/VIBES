open Core

let (let-) x f = Result.bind x ~f

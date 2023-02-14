(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open Core
open Bap.Std
open Helpers

module Prog1 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 + var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog2 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 + (var v3 + var v1)] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog3 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 lsl var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog4 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 lsr var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog5 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 land var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog6 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 lor var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog9 = struct

  let prog () : sub term =
    let tgt = Bap.Std.Tid.for_name "tgt" in
    let bil = [] in
    let prog = Bap_wp.Bil_to_bir.bil_to_sub bil in
    add_goto prog tgt

end

module Prog10 = struct

  let prog () : sub term =
    let bil = Bil.[mem := store ~mem:(var mem) ~addr:(var v1) (var v2) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog11 = struct

  let prog () : sub term =
    let bil = Bil.[v2 := load ~mem:(var mem) ~addr:(var v1) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog12 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (var v) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog13 = struct

  let prog () : sub term =
    let bil = Bil.[v2 := load ~mem:(var mem) ~addr:(var v1) BigEndian `r16] in
    let res = Bap_wp.Bil_to_bir.bil_to_sub bil in
    res

end

module Prog14 = struct

  let prog () =
    let bil = [] in
    let prog = Bap_wp.Bil_to_bir.bil_to_sub bil in
    add_call prog @@ func ()

end

module Prog15 = struct


  let prog () : sub term =
    let bil = Bil.[v1 := var v2 + !!42] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog16 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog17 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := !!5000] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog18 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (var v <> !!0) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog19 = struct

  let prog () : sub term =
    let bil = Bil.[mem := store ~mem:(var mem) ~addr:(var v1 + !!8) (var v2) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog20 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 * !!5] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog21 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 * !!8] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog22 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := !!0x5FFFF] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog23 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := lnot @@ var v2; v2 := unop neg @@ var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog24 = struct

  let prog () : sub term =
    let bil = Bil.[
        v1 := !!1 lsl !!3;
        v2 := !!1 lsl var v1;
        v3 := var v2 lor !!6;
      ] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog25 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (var v1 > !!42) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog26 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (lnot (var v)) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog27 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := !!(-1)] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog28 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := load ~mem:(var mem) ~addr:(!!0x1234) BigEndian `r8] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog29 = struct

  let prog () : sub term =
    let bil = Bil.[mem := store ~mem:(var mem) ~addr:(!!0x1234) !!5 BigEndian `r8] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog30 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 >= !!42] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog31 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := cast SIGNED 32 (load ~mem:(var mem) ~addr:(var v2 + !!8) BigEndian `r8)] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog32 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := cast SIGNED 32 (load ~mem:(var mem) ~addr:(var v2 + !!8) BigEndian `r16)] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end


module Prog33 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := load ~mem:(var mem) ~addr:(var v2 + (var v3 * !!4)) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

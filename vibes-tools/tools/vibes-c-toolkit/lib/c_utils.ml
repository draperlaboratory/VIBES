open Core

let print_c (pp : 'a -> unit) (data : 'a) : string =
  let tmp_file, chan = Stdlib.Filename.open_temp_file "frontc" ".out" in
  let old_chan = !Cprint.out in
  Cprint.out := chan;
  pp data;
  Cprint.flush ();
  Out_channel.close chan;
  let res = In_channel.read_lines tmp_file |> String.concat ~sep:"\n" in
  Cprint.out := old_chan;
  Sys_unix.remove tmp_file;
  res

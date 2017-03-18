(* Http_stats *)
open Threadext
exception Unknown_stat of string

let m = Mutex.create ()
let samples : (string, float list list) Hashtbl.t = Hashtbl.create 100

let external_sample_fn : (string -> float -> unit) option ref = ref None

let sample name vs =
  Mutex.execute m (fun () ->
    if Hashtbl.mem samples name
    then begin
      let cur = Hashtbl.find samples name in
      Hashtbl.replace samples name (vs::cur)
    end else begin
      Hashtbl.replace samples name [vs]
    end);
  match !external_sample_fn with | Some f -> f name (List.hd vs) | None -> ()

let time_this : string -> (unit -> float list) -> (unit -> 'a) -> 'a = fun name vs fn ->
  let before = Unix.gettimeofday () in
  let result = fn () in
  let after = Unix.gettimeofday () in
  sample name ((after -. before)::(vs ()));
  result

let reset name =
  Mutex.execute m (fun () -> Hashtbl.remove samples name)

let reset_all () =
  Mutex.execute m (fun () -> Hashtbl.clear samples)

let means name =
  try
    let samples = Mutex.execute m (fun () -> Hashtbl.find samples name) in
    let len = List.length samples |> float_of_int in
    let sums = List.fold_left (List.map2 (+.)) (List.hd samples) (List.tl samples) in
    List.map (fun v -> v /. len) sums
  with
  | Not_found -> raise (Unknown_stat name)

let get_all () =
  Mutex.execute m (fun () -> Hashtbl.fold (fun a b c -> (a,b)::c) samples [])

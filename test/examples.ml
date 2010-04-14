(* This code is in the public domain. *)

(* Time stamps on a periodic schedule *)

let periodic ?max ?stop ?start l p = match max with
| None ->
    let occ start t = t, (start +. ceil ((t -. start) /. p) *. p) in
    Rtime.stamps ?stop ?start occ l
| Some max ->
    if max <= 0 then React.E.never else
    let occ max start t =
      let i = ceil ((t -. start) /. p) in
      if i >= max then t, t (* stop *) else t, (start +. i *. p)
    in
    Rtime.stamps ?stop ?start (occ (float max)) l

(* Normalized linear time interval *)

let ninterval ?stop ?start freq l d =
  let max = floor (freq *. d) in
  let p = 1. /. freq in
  let np = 1. /. max in
  let first = p +. match start with None -> Rtime.now l | Some s -> s in
  let occ first t =
    let i = ceil ((t -. first) /. p) in
    if i >= max then 1., t (* stop *) else (i *. np), (first +. i *. p)
  in
  React.S.hold 0. (Rtime.stamps ?stop ~start:first occ l) 

(* Running a UNIX timeline *)

(* Single threaded program with Unix.select *)

let rec run l = match Rtime.wakeup l with
| None -> -1. (* unbounded wait *)
| Some d when d > 0. -> d
| Some _ -> Rtime.progress l; run l

let main () = 
  let unix_timeline = Rtime.create Unix.gettimeofday in
  let r, w, e = ref [], ref [], ref [] in
  while true do 
    let delay = run unix_timeline in 
    let r', w', e' = Unix.select !r !w !e delay in 
    ignore (r', w', e')
  done

(* Single threaded program with interval timers *)

let rec run l = match Rtime.wakeup l with 
  | None -> ()
  | Some d when d > 0. ->
    let s = { Unix.it_interval = 0.; it_value = d } in
    ignore (Unix.setitimer Unix.ITIMER_REAL s)
  | Some _ -> Rtime.progress l; run l

let timer_message = ()
let send_timer_message () = ()
let wait_message () = ()

let main () = 
  let unblock _ = send_timer_message () in
  let unix_timeline = Rtime.create ~earlier:unblock Unix.gettimeofday in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle unblock);
  run unix_timeline;
  while true do 
    let m = wait_message () in
    if m = timer_message then run unix_timeline else 
    ignore (m);
  done



(* Multi-threaded program with interval timers *)

let mutex = 
  let m = Mutex.create () in 
  fun f v ->
      try Mutex.lock m; let r = f v in Mutex.unlock m; r with
      | e -> (Mutex.unlock m; raise e)

let e_create () = 
  let e, send = React.E.create () in 
  e, mutex send

let s_create v = 
  let s, set = React.S.create v in 
  s, mutex set


let sleep, earlier = 
  let m = Mutex.create () in
  let proceed = Condition.create () in
  let sleeping = ref false in
  let set_timer d = 
    let s = { Unix.it_interval = 0.; it_value = d } in
    ignore (Unix.setitimer Unix.ITIMER_REAL s)
  in
  let sleep d = (* if d = 0. unbounded sleep *)
    if d < 0. then invalid_arg "negative delay";
    Mutex.lock m; 
    sleeping := true;
    set_timer d;
    while !sleeping do Condition.wait proceed m done;
    Mutex.unlock m
  in
  let earlier _ = 
    Mutex.lock m;
    sleeping := false;
    set_timer 0.; 
    Condition.signal proceed;
    Mutex.unlock m;
  in
  let timer _ = sleeping := false; Condition.signal proceed;
  in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle timer);
  sleep, earlier

let run l = 
  while true do 
    try match Rtime.wakeup l with
    | None -> sleep 0. (* unbounded sleep *)
    | Some d when d > 0. -> sleep d
    | Some _ -> mutex Rtime.progress l
    with e -> () (* print or ignore exception to avoid termination *)
  done;
  assert (false)

let l = Rtime.create ~earlier Unix.gettimeofday

let main () = 
  let unix_timeline = Rtime.create ~earlier Unix.gettimeofday in
  let timeline_thread = Thread.create run unix_timeline in
  ignore (timeline_thread)

(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ----------------------------------------------------------------------------*)

open React;;

type time = float
type duration = float
type deadline = 
    { time : float;                                        (* deadline time. *)
      cancelled : bool ref;                (* true if deadline is cancelled. *)
      mutable operation : unit -> unit; }                (* deadline effect. *)

type t = 
    { mutable heap : deadline option array;      (* deadline priority queue. *)
      mutable heap_size : int;                             (* size of queue. *)
      mutable refs : unit E.t ref list;    (* keeps references to avoid g.c. *)
      now : unit -> float;                                 (* absolute time. *)
      earlier : t -> unit; }         (* called on earlier deadline addition. *)

module Heap = struct            (* classical imperative heap implementation. *)
  let swap h i i' = 
    let v = h.(i) in
    h.(i) <- h.(i');
    h.(i') <- v

  let compare h i i' = 
    let d = match h.(i) with Some d -> d | None -> assert false in 
    let d' = match h.(i') with | Some d -> d | None -> assert false in
    compare d.time d'.time

  let rec up h i =                (* returns true if ends at the heap's top. *)
    if i = 0 then true else
    let p = (i - 1) / 2 in                                  (* parent index. *)
    if compare h i p < 0 then (swap h i p; up h p) else false

  let rec down h size i =
    let last = size - 1 in
    let start = 2 * i in
    let l = start + 1 in                                (* left child index. *) 
    let r = start + 2 in                               (* right child index. *)
    if l > last then () (* no child, stop *) else
    let child =                                  (* index of smallest child. *)
      if r > last then l else (if compare h l r < 0 then l else r)
    in
    if compare h i child > 0 then (swap h i child; down h size child)

  let grow l =                                           (* grows the heap. *)
    let heap' = Array.make (2 * (l.heap_size + 1)) None in
    Array.blit l.heap 0 heap' 0 l.heap_size;
    l.heap <- heap'
	    
  let add l deadline =                                  (* adds a deadline. *)
    if l.heap_size = Array.length l.heap then grow l;
    l.heap.(l.heap_size) <- Some deadline;
    l.heap_size <- l.heap_size + 1;
    if up l.heap (l.heap_size - 1) then l.earlier l else ()

  let rec rem_last_but_not_first l =     (* N.B. drops cancelled deadlines. *)
    if l.heap_size = 1 then None else
    let i = l.heap_size - 1 in
    match l.heap.(i) with 
    | Some d as r -> 
	l.heap.(i) <- None;
	l.heap_size <- i;
	if !(d.cancelled) then rem_last_but_not_first l else r
    | None -> assert false

  let _take l = 
    if l.heap_size = 0 then None else
    let v = l.heap.(0) in
    match rem_last_but_not_first l with
    | None -> l.heap.(0) <- None; l.heap_size <- 0; v
    | Some _ as d -> l.heap.(0) <- d; down l.heap l.heap_size 0; v
    
  let rec take l = match _take l with
  | None -> None
  | Some d when !(d.cancelled) -> take l
  | d -> d

  let rec peek l = 
    if l.heap_size = 0 then None else
    match l.heap.(0) with
    | Some d when !(d.cancelled) -> ignore (_take l); peek l
    | Some d as r -> r
    | None -> assert false
end

module Refs = struct
  let add l r = l.refs <- r :: l.refs
  let rem l r = 
    let rec aux acc r = function
      | [] -> acc 
      | h :: t when r == h -> List.rev_append acc t
      | h :: t -> aux (h :: acc) r t
    in
    l.refs <- aux [] r l.refs 
end

(* Creating and running time lines *)

let create ?(earlier = fun _ -> ()) now = 
  let h = Array.make 10 None in 
  { heap = h; heap_size = 0; refs = []; now = now; earlier = earlier }

let now l = l.now ()
let wakeup l = match Heap.peek l with 
| Some d -> Some (d.time -. (now l))
| None -> None 

let progress ?(exec = true) l = match Heap.take l with
| Some d when exec -> d.operation ()
| _ -> ()

(* Time stamps events *)

let stamp ?(stop = E.never) occ l t = 
  if t < (now l) then E.never else
  let e, send_e = E.create () in 
  let cancel = ref false in
  let stop_it _ = cancel := true in
  let stopper = E.map stop_it stop in 
  let stamp () = 
    send_e (occ t (now l)); 
    E.stop stopper (* ref. for the g.c. *) 
  in
  let d = { time = t; cancelled = cancel; operation = stamp } in
  Heap.add l d;
  e
  
let stamps ?(stop = E.never) ?start occ l = 
  let current = (now l) in 
  let start = match start with None -> current | Some t -> t in
  if start < current then E.never else
  let e, send_e = E.create () in 
  let cancel = ref false in 
  let stop_it _ = cancel := true in 
  let stopper = E.map stop_it stop in 
  let rec stamp () = 
    let now = now l in 
    let v, next = occ start now in 
    send_e v;
    if next <= now then E.stop stopper (* ref. for the g.c. *) else
    Heap.add l { time = next; cancelled = cancel; operation = stamp }
  in
  let d = { time = start; cancelled = cancel; operation = stamp } in
  Heap.add l d;
  e
      
(* Delays *)

(* In contrast to time stamps events a delay event has not always a
   deadline in the queue while it is active. Storing a reference to
   the stopper event in the deadline operation is thus not sufficent
   to prevent garbage collection. This is why we need to keep a
   reference to the stopper (which keeps a reference to the delayer)
   in l.refs. *)

let delay_e ?(stop = E.never) l d e = 
  let de, send_de = E.create () in
  let cancel = ref false in
  let stopper = ref E.never in
  let delayer = ref E.never in
  let stop_it _ = cancel := true; E.stop !delayer; Refs.rem l stopper in
  let delay v = 
    let send () = send_de v in
    let d = { time = (now l) +. d; cancelled = cancel; operation = send } in
    Heap.add l d
  in  
  stopper := E.map stop_it stop;
  delayer := E.map delay e;
  Refs.add l stopper;                                 (* ref. for the g.c. *)
  de
  
let delay_s ?eq ?(stop = E.never) l d i s =
  let ds, set_ds = S.create ?eq i in
  let cancel = ref false in
  let stopper = ref E.never in
  let delayer = ref E.never in
  let stop_it _ = cancel := true; E.stop !delayer; Refs.rem l stopper in
  let delay v = 
    let set () = set_ds v in 
    let d = { time = (now l) +. d; cancelled = cancel; operation = set } in 
    Heap.add l d
  in
  stopper := E.map stop_it stop;
  delayer := E.map delay (S.changes s);
  Refs.add l stopper;                                  (* ref for the g.c. *)
  ds

(*----------------------------------------------------------------------------
  Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the Daniel C. Bünzli nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

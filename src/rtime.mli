(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Timelines for {!React}.

    Rtime manages time stamp events, delayed events and delayed
    signals along timelines. The client chooses the concrete timeline
    by providing an absolute notion of time. Running the timeline at
    the appropriate pace is left to the client.

    See {{:#ex}examples} of use.

    {e Release %%VERSION%% - %%AUTHORS%% }
  
    {1:create Creating and running timelines} *)

type time = float
(** The type for absolute times. *)

type duration = float
(** The type for durations. *)

type t
(** The type for timelines. *)

val create : ?earlier:(t -> unit) -> (unit -> time) -> t
(** [create earlier now] is a timeline whose absolute current time is
    defined by calling the function [now]. [earlier] is called with
    the timeline as an argument whenever a new deadline is scheduled
    before all the others on the line; this can be used to unblock a
    sleeping thread. 

    {b Warning} [earlier] must not perform React update cycles (because
    it may very likely be called during an update cycle). *)

val now : t -> time
(** [now l] is the current time on the timeline. *)

val wakeup : t -> duration option
(** [wakeup l] is the duration until the next deadline on the timeline
    (if any).  If [duration] is negative, the timeline is late. *)

val progress : ?exec:bool -> t -> unit
(** [progress exec l] immediatly removes the next deadline from the
    timeline and if [exec] is [true] (default) executes it.

    {b Warning.} Deadline executions usually perform React 
    update cycles. *)

(** {1:stamps Time stamp events} 

    On a time stamp event occurence we distinguish :
    {ul
    {- Its {e stamp}, the occurence's value.}
    {- Its {e schedule}, the time for which the occurence was requested.}
    {- Its {e occurence time}, the time at which it actually occurs.}}

    The schedule and the occurence time may not coincide. *)

val stamp : ?stop:'a React.event -> (time -> time -> 'b) -> t -> time -> 
  'b React.event
(** [stamp stop occ l t] is an event such that :
    {ol 
    {- A single occurence is scheduled for [t] on [l].}
    {- The occurence's stamp is [occ t t'] with t' the occurence time.}
    {- No occurence is generated after a [stop] occurs 
       (existing deadlines are removed) or if [t] is earlier than [now l] 
    when [stamp] gets executed.}}*)

val stamps : ?stop:'a React.event -> ?start:time -> 
  (time -> time -> ('b * time)) -> t -> 'b React.event
(** [stamps stop start occ l] is an event such that :
    {ol
    {- The first occurence is scheduled for [start] (defaults to [now l]).}
    {- At each occurence time [t], [occ start t] returns the stamp for the 
       current occurence and the time of the next schedule. If the latter 
       is earlier or equal to [t] no new occurence is scheduled.}
    {- No occurences are generated after a [stop] occurs 
       (existing deadlines are removed) or if [start] is earlier than 
       [now l] when [stamps] gets exectued.}} *)
    
(** {1:delays Delays} *)

val delay_e : ?stop:'a React.event -> t -> duration -> 'b React.event -> 
  'b React.event
(** [delay_e stop l d e] is an event such that : 
    {ol
    {- Occurences are those of [e] delayed by [d] units of time on [l].}
    {- No occurences are generated after [stop] occurs (existing deadlines
       are removed).}} *)

val delay_s : ?eq:('b -> 'b -> bool) -> ?stop:'a React.event -> t -> 
  duration -> 'b -> 'b React.signal -> 'b React.signal
(** [delay_s eq stop l d i s] is :
    {[S.hold ?eq i (S.delay stop l d (S.changes s))]}*)
(** {1:ex Examples}

    {2:periodic Time stamps on a periodic schedule}

    The following function returns an event with occurences stamped by their
    occurence time and scheduled on [l] at
    [start], [start + p], [start + 2p], ... 
    [start + (max - 1)p]. Occurences known to be late at schedule time 
    are dropped. If [max] is [None] the number of occurences is unbounded.

{[let periodic ?max ?stop ?start l p = match max with
| None ->
    let occ start t = t, (start +. ceil ((t -. start) /. p) *. p) in
    Rtime.stamps ?stop ?start occ l
| Some max ->
    if max <= 0 then React.E.never else
    let occ max start t =
      let i = ceil ((t -. start) /. p) in
      if i >= max then t, t (* stop *) else t, (start +. i *. p)
    in
    Rtime.stamps ?stop ?start (occ (float max)) l]}

    {2:normalized Normalized linear time interval}

    The following function returns a signal that will vary linearly
    from [0.0] to [1.0] during the time interval from [start] to
    [start + d] on [l]. The signal will {e update} at most [freq] times per
    [l] units. Updates known to be late at schedule time are dropped
    and the last update value is guaranteed to be [1].
{[let ninterval ?stop ?start freq l d =
  let max = floor (freq *. d) in
  let p = 1. /. freq in
  let np = 1. /. max in
  let first = p +. match start with None -> Rtime.now l | Some s -> s in
  let occ first t =
    let i = ceil ((t -. first) /. p) in
    if i >= max then 1., t (* stop *) else (i *. np), (first +. i *. p)
  in
  React.S.hold 0. (Rtime.stamps ?stop ~start:first occ l)
]}
    {2:unixtimeline Running a UNIX timeline}

    The following examples show different techniques to run a UNIX timeline.
    {3:select Single threaded program with Unix.select}

    The call [run l] executes all expired deadlines on the
    timeline [l] and returns the next deadline or a negative value if
    there's no deadline.
{[let rec run l = match Rtime.wakeup l with
| None -> -1. (* unbounded wait *)
| Some d when d > 0. -> d
| Some _ -> Rtime.progress l; run l
]}
    The main function creates a timeline using {!Unix.gettimeofday}
    for the absolute time, initializes input and ouput descriptors
    and enters an infinite loop that runs the timeline and waits 
    with {!Unix.select} on descriptor events or timeline deadlines.
{[let main () = 
  let unix_timeline = Rtime.create Unix.gettimeofday in
  let r, w, e = ... (* init. descriptors *) in
  while true do 
    let delay = run unix_timeline in 
    let r', w', e' = Unix.select !r !w !e delay in 
    ... (* handle descriptor events *)
  done
]}
    {3:itimer Single threaded program with interval timers}    

    The call [run l] executes all expired deadlines
    on the timeline [l] and installs a timer to send the {!Sys.sigalrm}
    signal on the next deadline or does nothing if there's no deadline.
{[let rec run l = match Rtime.wakeup l with 
  | None -> ()
  | Some d when d > 0. ->
    let s = { Unix.it_interval = 0.; it_value = d } in
    ignore (Unix.setitimer Unix.ITIMER_REAL s)
  | Some _ -> Rtime.progress l; run l
]}
    The idea is to install a handler for {!Sys.sigalrm} that notifies
    the main program that a deadline expired. It is {e very important}
    that the handler does not try to progress the timeline itself by calling
    {!progress}, because doing so may violate the mutual exclusion of
    React's update cycles if the handler is invoked during such a
    cycle.

    In our example below, we assume the main loop waits indefinitely
    for messages in a queue with a blocking call to [wait_message].
    Whenever the timer expires the [unblock] handler sends a message
    to the queue with [send_timer_message]. If the main loop is
    currently blocked on [wait_message] this will unblock it and run
    the timeline. The timeline uses {!Unix.gettimeofday} for the
    absolute time and calls [unblock] if a deadline is inserted before
    the others to readjust the timer delay.
{[let main () = 
  let unblock _ = send_timer_message () in
  let unix_timeline = Rtime.create ~earlier:unblock Unix.gettimeofday in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle unblock);
  run unix_timeline;
  while true do 
    let m = wait_message () in
    if m = timer_message then run unix_timeline else 
    ... (* handle other message *)
  done]}
    {3:threads Multi-threaded program with interval timers} 

    In this example, we run the timeline in a dedicated thread. 

    First, React's update cycles must be executed in a critical
    section. The thread dedicated to the timeline will trigger update
    cycles upon deadline execution. Hence we need to execute them in
    mutual exclusion from update cycles triggered by other
    threads. The [mutex] function applies a function to its argument
    in a critical section. The other threads will have to use this
    function to perform update cycles; to acheive this automatically
    they can use [e_create] and [s_create] to create their primitives.
{[let mutex = 
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
]}
    Next, we need a mechanism to sleep the thread for a specific 
    amount of time ([sleep]) and a mean to unblock the thread
    from another thread in case an earlier event occurrence gets created 
    ([earlier]). Unix's interval timers and condition variables are used to 
    achieve this.
{[let sleep, earlier = 
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
]}
    The call [run l] is an infinite loop that executes expired 
    deadlines on the timeline [l] (note the use of the [mutex]
    function to progress the timeline), sleeps the thread until 
    the next deadline or forever if there's no deadline.
{[let run l = 
  while true do 
    try match Rtime.wakeup l with
    | None -> sleep 0. (* unbounded sleep *)
    | Some d when d > 0. -> sleep d
    | Some _ -> mutex Rtime.progress l
    with e -> ... (* print or ignore exception to avoid termination *)
  done;
  assert (false)
]}
    The main function creates a timeline using {!Unix.gettimeofday} for the
    absolute time and [earlier] to wake up the thread on earlier deadlines 
    and runs it in a dedicated thread. 
{[let main () = 
  let unix_timeline = Rtime.create ~earlier Unix.gettimeofday in
  let timeline_thread = Thread.create run unix_timeline in
  ...
]}
*)
(*---------------------------------------------------------------------------
  Copyright %%COPYRIGHT%%
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

  3. Neither the name of Daniel C. Bünzli nor the names of
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
  ---------------------------------------------------------------------------*)

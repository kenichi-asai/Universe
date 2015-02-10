open Universe
open Const
open Ball

(* type of states *)
(* a list of pairs of a client id and its list of balls *)
type state_t = (iworld_t * Ball.t list) list

(* the initial value of the state *)
let init_state : state_t = []

(* enlarge : Ball.t -> Ball.t *)
let enlarge {center = p; vector = v; radius = r; color = c} =
  {center = p; vector = v; radius = r +. 5.; color = c}

(* send_messages : state_t -> (state_t, world_t) Universe.t *)
let send_messages state =
  let messages =
    List.map (fun (id, lob) ->
               (id,                     (* destination *)
                (List.assoc id state,   (* its list of balls *)
                 let a = List.filter (fun (id', lob) -> id <> id') state in
                 List.flatten (List.map snd a) (* others' list of balls *)
                )))
             state in
  Bundle (state, messages, [])

(* onnew : state_t -> Unix.file_descr -> (state_t, world_t) Universe.t *)
let onnew state world_id =
  let color = next_color () in                  (* color for the new client *)
  let lob = [make_random_ball radius color;     (* world for the new client *)
             make_random_ball radius color;
             make_random_ball radius color] in
  let new_state = (world_id, lob) ::            (* new client is added *)
                  List.map (fun (id, lob) -> (id, List.map enlarge lob))
                           state in (* existing balls are enlarged *)
  send_messages new_state

(* mod, but the result is always between 0 and b *)
(* mymod : float -> float -> float *)
let rec mymod a b =
  if a < 0. then mymod (a +. b) b
  else if b <= a then mymod (a -. b) b
  else a

(* add_posn : float * float -> float * float -> float * float *)
let add_posn posn vector = match (posn, vector) with
  ((x1, x2), (v1, v2)) -> (mymod (x1 +. v1) width, mymod (x2 +. v2) height)

(* move_ball_on_tick : Ball.t -> Ball.t *)
let move_ball_on_tick {center = p; vector = v; radius = r; color = c} =
  {center = add_posn p v; vector = v; radius = r; color = c} 

(* move_on_tick : state_t -> (state_t, world_t) Universe.t *)
let move_on_tick state =
  let new_state = List.map (fun (world_id, lob) ->
                             (world_id, List.map move_ball_on_tick lob))
                           state in
  send_messages new_state

(* check if (x, y) is within r from (x0, y0) *)
(* is_inside : float -> float -> float * float -> float -> bool *)
let is_inside x y (x0, y0) r =
  (x -. x0) *. (x -. x0) +. (y -. y0) *. (y -. y0) <= r *. r 

(* change_ball : Ball.t -> Ball.t *)
let change_ball ({center = p; vector = v; radius = r; color = c} as ball) =
  if r <= 5. then ball
  else make_random_ball (r -. 5.) c

(* andmap : ('a -> bool) -> 'a list -> bool *)
let rec andmap f lst = match lst with
    [] -> true
  | first :: rest -> f first && andmap f rest

(* ormap : ('a -> bool) -> 'a list -> bool *)
let rec ormap f lst = match lst with
    [] -> false
  | first :: rest -> f first || ormap f rest

(* change_lob_on_mouse : Ball.t list -> float -> float -> Ball.t list *)
let change_lob_on_mouse lob x y =
  let is_inside_ball ball = is_inside x y ball.center ball.radius in
  let change ball = if is_inside_ball ball then change_ball ball else ball in
  List.map change lob

(* change_on_mouse : state_t -> 'a -> float * float ->
                     (state_t, world_t) Universe.t *)
let change_on_mouse state world_id (x, y) =
  (* the received message (x, y) is the clicked position *)
  let new_state = List.map (fun (id, lob) ->
                             (id, change_lob_on_mouse lob x y))
                           state in
  Bundle (new_state, [], [])

(* change_on_disconnect : state_t -> Unix.file_descr ->
                          (state_t, world_t) Universe.t *)
let change_on_disconnect state world_id =
  let new_state = List.filter (fun (id, lob) -> id <> world_id) state in
  Bundle (new_state, [], [world_id])

(* server start *)
let _ = universe init_state
                 ~on_new:onnew
                 ~on_tick:move_on_tick
                 ~on_msg:change_on_mouse
                 ~on_disconnect:change_on_disconnect
                 ~rate:0.1

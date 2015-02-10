open Color
open Image
open World

(* constants *)
let radius =  30.               (* initial radius of balls *)
let width  = 500.               (* window width *)
let height = 400.               (* window height *)

(* ball_t : type of balls *)
type ball_t = {
  center : float * float;
  vector : float * float;
  radius : float;
  color : Color.t;
}

(* make_random_ball : float -> Color.t -> ball_t *)
let make_random_ball radius color = {
  center = (Random.float width, Random.float height);
  vector = (Random.float 3. -. 1., Random.float 3. -. 1.);
  radius = radius;
  color = color;
}

(* ball_image : ball_t -> Image.t *)
let ball_image ball = match ball with
  {radius = r; color = c} -> circle r c

(* ball_top_left : ball_t -> float * float *)
let ball_top_left ball = match ball with
  {center = (x, y); radius = r} -> (x -. r, y -. r)

(* type of world *)
type world_t = ball_t list      (* a list of balls *)

(* initial value of the world *)
let initial_world : world_t =
  [make_random_ball radius red;
   make_random_ball radius red;
   make_random_ball radius red]

(* background : Image.t *)
let background =
  place_image (rectangle 50. 25. (make_color 0xbb 0xbb 0xbb))
              (width /. 2. -. 25., height /. 2. -. 12.)
              (empty_scene width height)

(* draw : world_t -> Image.t *)
let draw lob =
  place_images (List.map ball_image lob)
               (List.map ball_top_left lob)
               background

(* draw_game_over : world_t -> Image.t *)
let draw_game_over world =
  place_image (text "Game Finished" ~size:30. black)
              (width /. 2., height /. 2.)
              (draw world)

(* handle mouse event *)

(* check if (x, y) is within r from (x0, y0) *)
(* is_inside : float -> float -> float * float -> float -> bool *)
let is_inside x y (x0, y0) r =
  (x -. x0) *. (x -. x0) +. (y -. y0) *. (y -. y0) <= r *. r 

(* change_ball : ball_t -> ball_t *)
let change_ball {center = p; vector = v; radius = r; color = c} =
  if r <= 5.
  then make_random_ball radius c
  else make_random_ball (r -. 5.) c

(* change_world_on_mouse : world_t -> float -> float -> string ->
                           (world_t, 'a) World.t *)
let change_world_on_mouse world x y event = match event with
    "button_down" ->
      let is_inside_ball ball = is_inside x y ball.center ball.radius in
      let change ball = if is_inside_ball ball
                        then change_ball ball
                        else ball in
      World (List.map change world)
  | _ -> World (world)

(* handle tick event *)

(* mod, but the result is always between 0 and b *)
(* mymod : float -> float -> float *)
let rec mymod a b =
  if a < 0. then mymod (a +. b) b
  else if b <= a then mymod (a -. b) b
  else a

(* add_posn : float * float -> float * float -> float * float *)
let add_posn posn vector = match (posn, vector) with
  ((x1, x2), (v1, v2)) ->
    (mymod (x1 +. v1) width, mymod (x2 +. v2) height)

(* move_ball_on_tick : ball_t -> ball_t *)
let move_ball_on_tick {center = p; vector = v; radius = r; color = c} =
  {center = add_posn p v; vector = v; radius = r; color = c} 

(* change_world_on_tick : world_t -> (world_t, 'a) World.t *)
let change_world_on_tick world =
  World (List.map move_ball_on_tick world)

(* andmap : ('a -> bool) -> 'a list -> bool *)
let rec andmap f lst = match lst with
    [] -> true
  | first :: rest -> f first && andmap f rest

(* game_finished : world_t -> bool *)
let game_finished lob =
  andmap (fun ball -> ball.radius <= 5.) lob

(* game start *)
let _ =
  big_bang initial_world
           ~name:"Ball Game"
           ~to_draw:draw
           ~width:(int_of_float width)
           ~height:(int_of_float height)
           ~on_mouse:change_world_on_mouse
           (* ~on_key_press:change_world_on_key *)
           ~on_tick:change_world_on_tick
           ~rate:0.1
           ~stop_when:game_finished
           ~to_draw_last:draw_game_over

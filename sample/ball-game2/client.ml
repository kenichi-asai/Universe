open Color
open Image
open World
open Const
open Ball

(* type of world *)
type world_t = Ball.t list      (* a list of my balls *)
             * Ball.t list      (* a list of others' balls *)

(* initial value of the world *)
let initial_world : world_t = ([], [])

(* background : Image.t *)
let background =
  place_image (rectangle 50. 25. (make_color 0xbb 0xbb 0xbb))
              (width /. 2. -. 25., height /. 2. -. 12.)
              (empty_scene width height)

(* draw : world_t -> Image.t *)
let draw (lob, lobrest) =
  let world = lob @ lobrest in
  place_images (List.map ball_image world)
               (List.map ball_top_left world)
               background

(* draw_game_over : world_t -> Image.t *)
let draw_game_over world =
  place_image (text "Game Over" ~size:30. black)
              (width /. 2., height /. 2.)
              (draw world)

(* the position last clicked *)
let last_click : (float * float) option ref = ref None

(* preserve the clicked position *)
(* handle_mouse : world_t -> float -> float -> string ->
                  (world_t, 'a) World.t *)
let handle_mouse world x y event =
  if event = "button_down" then last_click := Some (x, y);
  World (world)

(* update world as sent from the server, and send back the clicked posn *)
(* receive : world_t -> world_t -> (world_t, float * float) World.t *)
let receive world message = match !last_click with
    None -> World message
  | Some (x, y) ->
      last_click := None;
      Package (message, (x, y))

(* andmap : ('a -> bool) -> 'a list -> bool *)
let rec andmap f lst = match lst with
    [] -> true
  | first :: rest -> f first && andmap f rest

(* game_over : world_t -> bool *)
let game_over (lob, lobrest) =
  List.length lob > 0                           (* game started, and *)
  && andmap (fun ball -> ball.radius <= 5.) lob (* all my balls are small *)

(* game start *)
let _ =
  big_bang initial_world
           ~name:"Ball Game"
           ~to_draw:draw
           ~width:(int_of_float width)
           ~height:(int_of_float height)
           ~on_mouse:handle_mouse
           (* ~on_key_press:change_world_on_key *)
           (* ~on_tick:change_world_on_tick *)
           ~rate:1.0
           ~on_receive:receive
           ~stop_when:game_over
           ~to_draw_last:draw_game_over

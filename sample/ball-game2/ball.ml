open Image
open Const

(* Ball.t : type of balls *)
type t = {
  center : int * int;
  vector : int * int;
  radius : int;
  color : Color.t;
}

(* make_random_ball : int -> Image.color_t -> Ball.t *)
let make_random_ball radius color = {
  center = (Random.int width, Random.int height);
  vector = (Random.int 3 - 1, Random.int 3 - 1);
  radius = radius;
  color = color;
}

(* ball_image : Ball.t -> Image.t *)
let ball_image ball = match ball with
  {radius = r; color = c} -> circle r (*"solid"*) c

(* ball_top_left : Ball.t -> int * int *)
let ball_top_left ball = match ball with
  {center = (x, y); radius = r} -> (x - r, y - r)

open Image
open Const

(* Ball.t : type of balls *)
type t = {
  center : float * float;
  vector : float * float;
  radius : float;
  color : Color.t;
}

(* make_random_ball : float -> Image.color_t -> Ball.t *)
let make_random_ball radius color = {
  center = (Random.float width, Random.float height);
  vector = (Random.float 3. -. 1., Random.float 3. -. 1.);
  radius = radius;
  color = color;
}

(* ball_image : Ball.t -> Image.t *)
let ball_image ball = match ball with
  {radius = r; color = c} -> circle r (*"solid"*) c

(* ball_top_left : Ball.t -> float * float *)
let ball_top_left ball = match ball with
  {center = (x, y); radius = r} -> (x -. r, y -. r)

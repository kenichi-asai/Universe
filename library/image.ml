(* filename : image.ml *)

open Color

(* 四角形 *)
type rect_t = {
  color : Color.t;              (* 色 *)
  x1 : float; y1 : float;       (* 左上の座標 *)
  x2 : float; y2 : float;       (* 横幅、高さ *)
  fill : bool;                  (* 塗りつぶしの有無 *)
  outline_size : float;         (* 外枠のサイズ *)
}

(* 多角形 *)
type polygon_t = {
  color : Color.t;              (* 色 *)
  x : float; y : float;         (* 左上の位置 *)
  points : (float * float) list;(* 点の座標のリスト *)
  fill : bool;                  (* 塗りつぶしの有無 *)
  outline_size : float;         (* 外枠のサイズ *)
}


(* 円 *)
type circle_t = {
  color : Color.t;              (* 色 *)
  x1 : float; y1 : float;       (* 中心の座標 *)
  radious : float;              (* 半径 *)
  fill : bool;                  (* 塗りつぶしの有無 *)
  outline_size : float;         (* 外枠のサイズ *)
}

(* 線 *)
type line_t = {
  color : Color.t;              (* 色 *)
  size : float;                 (* 線のサイズ *)
  x : float; y : float;         (* 左上の位置 *)
  points : (float * float) list (* 点の座標のリスト *)
}

(* テキスト *)
type text_t = {
  color : Color.t;              (* 色 *)
  text : string;                (* テキスト *)
  x : float; y : float;         (* 左下の座標 *)
  size : float;                 (* フォントのサイズ *)
}

(* 画像 *)
type picture_t = {
  picture : Cairo.Surface.t;    (* 画像 *)
  x : float; y : float;         (* 左上の座標 *)
}

type t =
    RECT of rect_t
  | POLYGON of polygon_t
  | CIRCLE of circle_t
  | LINE of line_t
  | TEXT of text_t
  | PICTURE of picture_t
  | IMAGE of t list

(* 相対位置の座標のリストを絶対位置の座標のリストに変える *)
(* rel_to_ab : (float * float) list -> float * float -> (float * float) list *)
let rec rel_to_ab lst (prex, prey) = match lst with
    [] -> []
  | (x, y) :: rest ->
      let new_posn = (x +. prex, y +. prey) in
      new_posn :: rel_to_ab rest new_posn

(* 座標のリストを受け取り, 左下の座標を返す *)
(* get_min : (float * float) list -> (float * float) *)
let rec get_min lst (minx, miny) = match lst with
    [] -> (minx, miny)
  | (x, y) :: rest ->
      let nextminx = if minx <= x then minx else x in
      let nextminy = if miny <= y then miny else y in
      get_min rest (nextminx, nextminy)

(* テキストとサイズを受け取ったら画像の横と縦の組を返す *)
(* text_wh : string -> float -> (int * int) *)
let text_wh str size =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~width:0 ~height:0 in
  let context = Cairo.create surface in
  begin
    Cairo.set_font_size context size;
    let text_extends = Cairo.text_extents context str in
    match text_extends with
      {Cairo.width = w; Cairo.height = h} -> (w, h)
  end

(* line, polygon 関数で使う *)
(* x と y それぞれの最小値が負の時のみ座標を動かすが, その値を返す *)
(* move_xy : (float * float) list -> (float : float) *)
let move_xy lst =
  let ab_points = rel_to_ab lst (0., 0.) in (*(0, 0) からの絶対位置 *)
  let min_posn = get_min ab_points (0., 0.) in (* 左下の座標 *)
  let min_x = fst min_posn in
  let min_y = snd min_posn in
  let move_x = if min_x >= 0. then 0. else min_x in
  let move_y = if min_y >= 0. then 0. else min_y in
  (move_x, move_y)

(* rectangle : float -> float ->
               ?fill:bool -> ?outline_size:float -> Color.t -> Image.t *)
let rectangle w h ?(fill = true) ?(outline_size = 0.) c =
  RECT {color = c; x1 = 0.; y1 = 0.; x2 = w; y2 = h;
        fill = fill; outline_size = outline_size}

(* empty_scene : float -> float -> Image.t *)
let empty_scene w h = rectangle w h Color.white ~fill:true ~outline_size:0.

(* circle : float -> ?fill:bool -> ?outline_size:float -> Color.t -> Image.t *)
let circle r ?(fill = true) ?(outline_size = 0.) c =
  CIRCLE {color = c; x1 = r; y1 = r; radious = r;
          fill = fill; outline_size = outline_size}

(* line : (float * float) list -> ?size:float -> Color.t -> Image.t *)
let line lst ?(size = 0.5) c =
  let movexy = move_xy lst in 
  (* x と y それぞれの最小値が負の時のみ座標を動かす *)
  (* line の左下を画像の (0, 0) にするため *)
  LINE {color = c; size = size; x = 0. -. fst movexy; y = 0. -. snd movexy;
        points = lst}

(* polygon : (float * float) list ->
             ?fill:bool -> ?outline_size:float -> Color.t -> Image.t *)
let polygon lst ?(fill = true) ?(outline_size = 0.) c =
  let movexy = move_xy lst in 
  (* x と y それぞれの最小値が負の時のみ座標を動かす *)
  (* line の左下を画像の (0, 0) にするため *)
  POLYGON {color = c; x = 0. -. fst movexy; y = 0. -. snd movexy;
           points = lst; fill = fill; outline_size = outline_size}

(* text : string -> ?size:float -> Color.t -> Image.t *)
let text str ?(size = 20.) c =
  let wh = text_wh str size in
  TEXT {color = c; text = str; x = 0.; y = snd wh; size = size}

(* read_image : string -> Image.t *)
let read_image name =
  let pic =
    try
      Cairo.PNG.create name
    with Out_of_memory ->
           failwith ("The file " ^ name ^ " is not PNG.") in
  PICTURE {picture = pic; x = 0.; y = 0.}

(* move_one *)
let rec move_one dx dy pic = match pic with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h;
          fill = fill; outline_size = outline_size} ->
    RECT {color = c; x1 = x +. dx; y1 = y +. dy; x2 = w; y2 = h;
          fill = fill; outline_size = outline_size}
  | POLYGON {color = c; x = x; y = y; points = lst;
             fill = fill; outline_size = outline_size} ->
    POLYGON {color = c; x = x +. dx; y = y +. dy; points = lst;
             fill = fill; outline_size = outline_size}
  | CIRCLE {color = c; x1 = x; y1 = y; radious = r;
            fill = fill; outline_size = outline_size} ->
    CIRCLE {color = c; x1 = x +. dx; y1 = y +. dy; radious = r;
            fill = fill; outline_size = outline_size}
  | LINE {color = c; size = s; x = x; y = y; points = lst} ->
    LINE {color = c; size = s; x = x +. dx; y = y +. dy; points = lst}
  | TEXT {color = c; text = t; x = x; y = y; size = s} ->
    TEXT {color = c; text = t; x = x +. dx; y = y +. dy; size = s}
  | PICTURE {picture = p; x = x; y = y} ->
    PICTURE {picture = p; x = x +. dx; y = y +. dy}
  | IMAGE lst -> IMAGE (List.map (move_one dx dy) lst)

(* place_images : Image.t list -> (float * float) list -> Image.t -> Image.t *)
let rec place_images pics posns background = match (pics, posns) with
    ([], []) -> background
  | ([], _ :: _) -> failwith "place_images: too many posns."
  | (_ :: _, []) -> failwith "place_images: too many pics."
  | (pic :: pics, (dx, dy) :: posns) ->
    IMAGE (move_one dx dy pic ::
           let background = place_images pics posns background in
           begin match background with
               IMAGE lst -> lst
             | _ -> [background]
           end)

(* place_image : Image.t -> int * int -> Image.t -> Image.t *)
let rec place_image pic posn background =
  place_images [pic] [posn] background

(* rev_iter *)
let rec rev_iter f lst = match lst with
    [] -> ()
  | first :: rest -> rev_iter f rest; f first

(* パスの外枠を塗る *)
(* draw_outline : Cairo.context -> float -> unit *)
let draw_outline context outline_size =
  Cairo.set_line_width context outline_size;
  Cairo.set_line_cap context Cairo.ROUND; (* 線の端を丸くする *)
  Cairo.set_line_join context Cairo.JOIN_ROUND; (* 線の角を丸くする *)
  Cairo.stroke context

(* context に line のパスを作成する *)
(* draw_line : (float * float) list -> Cairo.context -> unit *)
let rec draw_line plst context = match plst with
    [] -> ()
  | (x, y) :: rest ->
      Cairo.rel_line_to context ~x:x ~y:y;
      draw_line rest context

let ratio_of_int x = float_of_int x /. 255.

(* image を canvas に描画する *)
(* draw : Cairo.context -> t -> unit *)
let rec draw context image = match image with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h;
          fill = fill; outline_size = outline_size} ->
      let (r, g, b, a) = Color.to_rgba c in
      Cairo.set_source_rgba context (ratio_of_int r) (ratio_of_int g)
                                    (ratio_of_int b) (ratio_of_int a);
      Cairo.rectangle context ~x:x ~y:y ~w:w ~h:h;
      if fill then Cairo.fill_preserve context;(* 塗りつぶす場合 *)
      draw_outline context outline_size
  | POLYGON {color = c; x = x; y = y; points = lst;
             fill = fill; outline_size = outline_size} ->
      let (r, g, b, a) = Color.to_rgba c in
      Cairo.set_source_rgba context (ratio_of_int r) (ratio_of_int g)
                                    (ratio_of_int b) (ratio_of_int a);
      Cairo.move_to context x y;
      draw_line lst context;
      Cairo.Path.close context; (* 最初と最後の点を結ぶ *)
      if fill then Cairo.fill_preserve context;(* 塗りつぶす場合 *)
      draw_outline context outline_size
  | CIRCLE {color = c; x1 = x; y1 = y; radious = radius;
            fill = fill; outline_size = outline_size} ->
      let pi2 = 8. *. atan 1. in
      let (r, g, b, a) = Color.to_rgba c in
      Cairo.set_source_rgba context (ratio_of_int r) (ratio_of_int g)
                                    (ratio_of_int b) (ratio_of_int a);
      Cairo.arc context ~x:x ~y:y ~r:radius ~a1:0. ~a2:pi2;
      if fill then Cairo.fill_preserve context;(* 塗りつぶす場合 *)
      draw_outline context outline_size
  | LINE {color = c; size = s; x = x; y = y; points = lst} ->
      let (r, g, b, a) = Color.to_rgba c in
      Cairo.set_source_rgba context (ratio_of_int r) (ratio_of_int g)
                                    (ratio_of_int b) (ratio_of_int a);
      Cairo.set_line_width context s;
      Cairo.move_to context x y;
      draw_line lst context;
      Cairo.set_line_cap context Cairo.ROUND; (* 線の端を丸くする *)
      Cairo.set_line_join context Cairo.JOIN_ROUND; (* 線の角を丸くする *)
      Cairo.stroke context
  | TEXT {color = c; text = t; x = x; y = y; size = s} ->
      let (r, g, b, a) = Color.to_rgba c in
      Cairo.set_source_rgba context (ratio_of_int r) (ratio_of_int g)
                                    (ratio_of_int b) (ratio_of_int a);
      Cairo.set_font_size context s;
      Cairo.move_to context x y;
      Cairo.show_text context t;
      Cairo.fill context
  | PICTURE {picture = p; x = x; y = y} ->
      Cairo.set_source_surface context p ~x:x ~y:y;
      Cairo.paint context
  | IMAGE lst -> (* 後ろから描画 *)
      rev_iter (draw context) lst

(* 左下と右上の座標のリストを受け取り, 左下の座標と右上の座標を返す *)
(* get_corner_posn : ((float * float) * (float * float)) list
                     -> (float * float) * (float * float) *)
let rec get_corner_posn lst ((minx, miny), (maxx, maxy)) = match lst with
    [] -> ((minx, miny), (maxx, maxy))
  | ((x1, y1), (x2, y2)) :: rest ->
      let nextminx = if minx <= x1 then minx else x1 in
      let nextminy = if miny <= y1 then miny else y1 in
      let nextmaxx = if maxx >= x2 then maxx else x2 in
      let nextmaxy = if maxy >= y2 then maxy else y2 in
      get_corner_posn rest ((nextminx, nextminy), (nextmaxx, nextmaxy))

(* 座標のリストを受け取り, 左下の座標と右上の座標を返す *)
(* get_min_max : (float * float) list -> ((float * float) * (float * float)) *)
let rec get_min_max lst ((minx, miny), (maxx, maxy)) = match lst with
    [] -> ((minx, miny), (maxx, maxy))
  | (x, y) :: rest ->
      let nextminx = if minx <= x then minx else x in
      let nextminy = if miny <= y then miny else y in
      let nextmaxx = if maxx >= x then maxx else x in
      let nextmaxy = if maxy >= y then maxy else y in
      get_min_max rest ((nextminx, nextminy), (nextmaxx, nextmaxy))

(* 左下の座標を得る *)
(* lower_left : (float * float) -> float -> (float * float) *)
let lower_left (x, y) distance =
  (x -. distance, y -. distance)

(* 右上の座標を得る *)
(* upper_right : (float * float) -> float -> (float * float) *)
let upper_right (x, y) distance =
  (x +. distance, y +. distance)

(* 外枠がある図形は線の太さの分, corner_lst 関数内で左下を動かす必要がある *)
(* outline_move : bool -> float-> float *)
let outline_move fill outline_size =
  let half_size = outline_size /. 2. in half_size

(* 相対位置の polygon と line の 左下の座標と右上の座標の組を返す *)
(* make_corner_posn : (float * float) -> (float * float) list ->
                      ((float * float) * (float * float)) list *)
let make_corner_posn (x, y) lst =
  let ab_lst = rel_to_ab lst (x, y) in (* 絶対位置に変換 *)
  get_min_max ab_lst ((x, y), (x, y))

(* 外枠のpolygon と line が corner_lst 関数で返すべき値を作る *)
(* 線の太さ分, 左下と右上の座標を動かす *)
(* make_corners : (float * float) * (float * float) -> float ->
                  (((float * float) * float) * (float * float)) list *)
let make_corners (min_posn, max_posn) outline_size =
  let half_size = outline_size /. 2. in
  ((lower_left min_posn half_size, half_size),
   upper_right max_posn half_size)

(* Image.t 型の, 中に入っている画像の左下と右上の座標のリストを返す *)
(* 左下の座標が line か外枠によるものだと太さの半分が返される *)
(* Surface に描く際に線の太さ分右上に動かすため *)
(* corner_lst : Image.t -> (((float * float) * float) * (float * float)) list *)
let rec corner_lst image =
  match image with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h;
          fill = fill; outline_size = outline_size} :: rest ->
      let move = outline_move fill outline_size in
      (((x -. move, y -. move), move), (x +. w +. move, y +. h +. move))
      :: corner_lst rest
  | POLYGON {color = c; x = x; y = y; points = lst;
             fill = fill; outline_size = outline_size} :: rest ->
      let min_max = make_corner_posn (x, y) lst in
      make_corners min_max outline_size :: corner_lst rest
      (* 線の太さの分, 左下と右上の座標を動かす *)
  | CIRCLE {color = c; x1 = x; y1 = y; radious = r;
            fill = fill; outline_size = outline_size} :: rest ->
      let move = outline_move fill outline_size in
      (((x -. r -. move, y -. r -. move), move),
       (x +. r +. move, y +. r +. move)) :: corner_lst rest
  | LINE {color = c; size = s; x = x; y = y; points = lst} :: rest ->
      let min_max = make_corner_posn (x, y) lst in
      (* 左下の座標と右上の座標の組を得る *)
      make_corners min_max s :: corner_lst rest
      (* 線の太さの分, 左下と右上の座標を動かす *)
  | TEXT {color = c; text = t; x = x; y = y; size = s} :: rest ->
      let wh = text_wh t s in
      (((x, y), 0.), (x +. fst wh, y +. snd wh)) :: corner_lst rest
  | PICTURE {picture = p; x = x; y = y} :: rest ->
      (((x, y), 0.), (x +. float_of_int (Cairo.Image.get_width p),
                      y +. float_of_int (Cairo.Image.get_height p)))
      :: corner_lst rest
  | (IMAGE lst) :: _ -> corner_lst lst
      (* IMAGE は [IMAGE] というリストにして渡される*)
  | [] -> []

(* Image.t をSurface.t にすると共に, 配置するときに動かす距離を返す *)
(* line は太さの分大きく画像を作るため, 配置する時にマイナスしてあげるから *)
(* image_to_surface : Image.t -> (Cairo.Surface.t * float) *)
let image_to_surface image =
  let posnlst = corner_lst [image] in
  let corners = get_corner_posn (List.tl posnlst) (List.hd posnlst) in
  (* image の左下と右上の座標の組を得る *)
  let minx = fst (fst (fst corners)) in
  let miny = snd (fst (fst corners)) in
  let maxx = fst (snd corners) in
  let maxy = snd (snd corners) in
  let surface = Cairo.Image.create Cairo.Image.ARGB32
                                   ~width:(int_of_float (maxx -. minx))
                                   ~height:(int_of_float (maxy -. miny)) in
  let context = Cairo.create surface in
  let move_distance = snd (fst corners) in
  (* 左下の座標が line のものだとこれに0.以外の値が入る*)
  draw context (move_one move_distance move_distance image);
  (surface, move_distance)

(* to_bitmap : Image.t -> Cairo.Image.data32 *)
let to_bitmap image =
  Cairo.Image.get_data32 (fst (image_to_surface image))

(* from_bitmap : Cairo.Image.data32 -> Image.t *)
let from_bitmap data =
  PICTURE {picture = Cairo.Image.create_for_data32 data; x = 0.; y = 0.}

(* freeze : Image.t -> Image.t *)
let freeze image =
  let surface_pair = image_to_surface image in
  let move_distance = 0. -. snd surface_pair in
  PICTURE {picture = fst surface_pair; x = move_distance; y = move_distance}
  (* 左下が line の座標だとその分後ろに下がって配置する必要がある *)

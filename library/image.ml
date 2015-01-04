(* filename : image.ml *)

open Color

(* 四角形 *)
type gnorect = {
  color : Color.t;              (* 色 *)
  x1 : float; y1 : float;       (* 左上の座標 *)
  x2 : float; y2 : float;       (* 横幅、高さ *)
}

(* 多角形 *)
type gnopolygon = {
  color : Color.t;              (* 色 *)
  points : (int * int) list;    (* 点の座標のリスト *)
}

(* 円 *)
type gnocircle = {
  color : Color.t;              (* 色 *)
  x1 : float; y1 : float;       (* 中心の座標 *)
  x2 : float; y2 : float;       (* 横幅、高さ *)
}

(* 線 *)
type gnoline = {
  color : Color.t;              (* 色 *)
  points : (int * int) list;    (* 点の座標のリスト *)
}

(* テキスト *)
type gnotext = {
  color : Color.t;              (* 色 *)
  text : string;                (* テキスト *)
  x : float; y : float;         (* 左上の座標 *)
  size : float;                 (* フォントのサイズ *)
}

(* 画像 *)
type gnopixbuf = {
  pixbuf : GdkPixbuf.pixbuf;    (* 画像 *)
  x : float; y : float;         (* 左上の座標 *)
  width : float; height : float;(* 横幅、高さ *)
}

type t = 
    RECT of gnorect
  | CIRCLE of gnocircle
  | POLYGON of gnopolygon
  | LINE of gnoline
  | TEXT of gnotext
  | PIXBUF of gnopixbuf
  | IMAGE of t list

(* empty_scene : int -> int -> Image.t *)
let empty_scene w h =
  RECT {color = white; x1 = 0.; y1 = 0.; 
        x2 = (float_of_int w); y2 = (float_of_int h)}
  
(* rectangle : int -> int -> Color.t -> Image.t *)
let rectangle w h c =
  RECT {color = c; x1 = 0.; y1 = 0.; 
        x2 = (float_of_int w); y2 = (float_of_int h)}

(* polygon : (int * int) list -> Color.t -> Image.t *)
let polygon lst c =  
  POLYGON {color = c; points = lst}

(* circle : int -> Color.t -> Image.t *)
let circle radious c =
  CIRCLE {color = c; x1 = 0.; y1 = 0.; 
          x2 = float_of_int (2 * radious); 
          y2 = float_of_int (2 * radious) }

(* line : (int * int) list -> Color.t -> Image.t *)
let line lst c =
   LINE {color = c; points = lst}

(* text : string -> int -> Color.t -> Image.t *)
let text str tsize c =
  TEXT {color = c; text = str; x = 0.; y = 0.; size = float_of_int tsize}

(* read_image : string -> Image.t *)
let read_image name = 
  let buf = GdkPixbuf.from_file name in
  PIXBUF {pixbuf = buf; x = 0.; y = 0.;
          width = float_of_int (GdkPixbuf.get_width buf);
          height = float_of_int (GdkPixbuf.get_height buf)}

(* move_one *)
let rec move_one dx dy pic = match pic with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      let dx = float_of_int dx in
      let dy = float_of_int dy in
      RECT {color = c; x1 = x +. dx; y1 = y +. dy; x2 = w; y2 = h}
  | POLYGON {color = c; points = lst} ->
      let lst = List.map (fun (x, y) -> (x + dx, y + dy)) lst in
      POLYGON {color = c; points = lst}
  | CIRCLE {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      let dx = float_of_int dx in
      let dy = float_of_int dy in
      CIRCLE {color = c; x1 = x +. dx; y1 = y +. dy; x2 = w; y2 = h}
  | LINE {color = c; points = lst} ->
      let lst = List.map (fun (x, y) -> (x + dx, y + dy)) lst in
      LINE {color = c; points = lst}
  | TEXT {color = c; text = t; x = x; y = y; size = s} ->
      let dx = float_of_int dx in
      let dy = float_of_int dy in
      TEXT {color = c; text = t; x = x +. dx; y = y +. dy; size = s}
  | PIXBUF {pixbuf = p; x = x; y = y; width = w; height = h} ->
      let dx = float_of_int dx in
      let dy = float_of_int dy in
      PIXBUF {pixbuf = p; x = x +. dx; y = y +. dy; width = w; height = h}
  | IMAGE lst -> IMAGE (List.map (move_one dx dy) lst)

(* place_images : Image.t list -> (int * int) list -> Image.t -> Image.t *)
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

(* image を canvas に描画する *)
(* draw : #GnoCanvas.group -> Image.t -> unit *)
let rec draw canvas image = match image with
    RECT {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      ignore (GnoCanvas.rect ~props:[`FILL_COLOR_RGBA (to_int32 c);
                                     `X1 x;
                                     `Y1 y;
                                     `X2 (x +. w);
                                     `Y2 (y +. h)]
                             canvas)
  | POLYGON {color = c; points = lst} ->
      let pointlst = List.flatten (List.map (fun (x, y) -> [x; y]) lst) in
      let p = Array.of_list (List.map float_of_int pointlst) in
      ignore (GnoCanvas.polygon ~props:[`FILL_COLOR_RGBA (to_int32 c);
                                        `POINTS p]
                                canvas)
  | CIRCLE {color = c; x1 = x; y1 = y; x2 = w; y2 = h} ->
      ignore (GnoCanvas.ellipse ~props:[`FILL_COLOR_RGBA (to_int32 c);
                                        `X1 x;
                                        `Y1 y;
                                        `X2 (x +. w);
                                        `Y2 (y +. h)]
                                canvas)
  | LINE {color = c; points = lst} ->
     let pointlst = List.flatten (List.map (fun (x, y) -> [x; y]) lst) in
     let p = Array.of_list (List.map float_of_int pointlst) in
     ignore (GnoCanvas.line ~props:[`FILL_COLOR_RGBA (to_int32 c);
                                    `POINTS p]
                                canvas)
  | TEXT {color = c; text = t; x = x; y = y; size = s} ->
      ignore (GnoCanvas.text ~props:[`FILL_COLOR_RGBA (to_int32 c);
                                     `SIZE_POINTS s;
                                     `TEXT t;
                                     `X x;
                                     `Y y]
                             canvas)
  | PIXBUF {pixbuf = p; x = x; y = y; width = w; height = h} ->
      ignore (GnoCanvas.pixbuf ~x:x
                               ~y:y
                               ~pixbuf:p
                               canvas)
  | IMAGE lst -> (* 後ろから描画 *)
      rev_iter (draw canvas) lst

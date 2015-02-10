(**     World module 
    @ja World モジュール
    @see <http://docs.racket-lang.org/teachpack/2htdpuniverse.html> Racket Documentation *)

type ('a, 'b) t =
    World of 'a (**     ['a] is the type of world states
                    @ja 世界の型が ['a] *)
  | Package of 'a * 'b
        (**     ['a] is the type of world states,
                ['b] is the type of messages
            @ja 世界の型が ['a]、メッセージの型が ['b] *)

val big_bang :
  ?name:string ->
  ?width:int ->
  ?height:int ->
  ?to_draw:('a -> Image.t) ->
  ?on_tick:('a -> ('a, 'b) t) ->
  ?on_mouse:('a -> float -> float -> string -> ('a, 'c) t) ->
  ?on_key_press:('a -> string -> ('a, 'd) t) ->
  ?on_key_release:('a -> string -> ('a, 'e) t) ->
  ?rate:float ->
  ?stop_when:('a -> bool) ->
  ?to_draw_last:('a -> Image.t) ->
  ?register:string * string ->
  ?on_receive:('a -> 'f -> ('a, 'g) t) -> 'a -> unit
  (** @en starts a game after receiving the following arguments
          (the last argument of type ['a] is mandatory)
        - [name] : a name appearing at the top of a window
        - [width] [height] : width and height of a window
        - [to_draw] : receives a [world];
                      returns an image to be drawn on a window
        - [on_tick] : receives a [world]; returns a [world] after 1 tick
        - [on_mouse] : receives a [world], a mouse coordinate, and a name of a
                       mouse event; returns a new [world]
        - [on_key_press] : receives a [world] and a name of a pressed key;
                           returns a new [world]
        - [on_key_release] : receives a [world] and a name of a released key;
                           returns a new [world]
        - [rate] : an interval of 1 tick in sec
        - [stop_when] : receives a [world];
                        returns true if the game is over
        - [to_draw_last] : receives a [world];
                           returns an image to be drawn when the game is over
        - [register] : the ip address and the port number of a server
        - [on_receive] : receives a [world] and a message sent from the server;
                         returns a new [world]
        - an initial value of [world] (mandatory)
      @ja 以下の引数を受け取ってゲームを開始する（最後の引数のみ必須）
        - [name] : ゲームのウィンドウの名前
        - [width] [height] : ウィンドウの横幅、縦幅
        - [to_draw] : 世界を受け取ったら画像を返す関数
        - [on_tick] : 世界を受け取ったら 1 tick 後の世界を返す関数
        - [on_mouse] : 世界とマウス座標とマウスイベント名を受け取ったら、
                        新しい世界を返す関数
        - [on_key_press] : 世界と押されたキー名を受け取ったら、
                        新しい世界を返す関数
        - [on_key_release] : 世界と離されたキー名を受け取ったら、
                        新しい世界を返す関数
        - [rate] : 1 tick の間隔（秒）
        - [stop_when] : 世界を受け取り、ゲーム終了ならtrueを返す関数
        - [to_draw_last] : 世界を受け取り、
                        ゲーム終了の際に描かれる画面を返す関数
        - [register] : サーバのIPアドレスとポート番号
        - [on_receive] : 世界と新しいメッセージを受け取ったら
                        新しい世界を返す関数
        - ['a] : 世界、つまり world の初期値（必須） *)

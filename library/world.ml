(* filename : world.ml *)

(* 監視 (add_watch) するソケットの id *)
let wid_ref = ref None

(* add_watch されていたソケットの監視を解除する *)
let remove_watch () = match !wid_ref with
    None -> ()
  | Some (wid) -> wid_ref := None; GMain.Io.remove wid

(* 引数から IP, ポート番号を得る *)
(* でも単独ゲームの場合、引数を別用途に使いたいかも *)
let obtain_addrinfo () = match Array.length Sys.argv with
    2 -> Some ("localhost", Sys.argv.(1))
  | 3 -> Some (Sys.argv.(1), Sys.argv.(2))
  | _ -> None

(* 'a は world の型、'b は mail の型 *)
type ('a, 'b) t =
    World of 'a
  | Package of 'a * 'b

(* on_tick の初期値 *)
(* initial_tick : 'a -> ('a, 'b) t *)
let initial_tick w = World w

(* on_mouse の初期値 *)
(* initial_mouse : 'a -> 'b -> 'c -> 'd -> ('a, 'e) t *)
let initial_mouse w x y ev = World w

(* on_key の初期値 *)
(* initial_key : 'a -> 'b -> ('a, 'c) t *)
let initial_key w k = World w

(* on_receive の初期値 *)
(* initial_receive : 'a -> 'b -> ('a, 'c) t *)
let initial_receive w m = World w

(* stop_when の初期値。常に false を返すことで終わらなくする *)
(* initial_stop : 'a -> bool *)
let initial_stop w = false

(* big_bang *)
let big_bang ?(name="My Game")
             ?(width=300)
             ?(height=300)
             ?to_draw
             ?(on_tick=initial_tick)
             ?(on_mouse=initial_mouse)
             ?(on_key_press=initial_key)
             ?(on_key_release=initial_key)
             ?(rate=1.0)
             ?(stop_when=initial_stop)
             ?to_draw_last
             ?register
             ?(on_receive=initial_receive)
             initial_world
             =

  (* 以下が big_bang の本体 *)

  (* 引数の処理 *)

  (* draw の初期値 *)
  (* width, height が必要なので big_bang の外では定義できない *)
  (* initial_draw : 'a -> Image.t *)
  let initial_draw w = Image.empty_scene (float_of_int width) 
					 (float_of_int height) in

  (* draw : 'a -> Image.t *)
  let draw = match to_draw with
      None -> initial_draw
    | Some (f) -> f
  in

  (* draw_last : 'a -> Image.t *)
  let draw_last = match to_draw_last with
      None -> initial_draw
    | Some (f) -> f
  in

  (* big_bang 内の大域変数と window の作成 *)

  (* rate : int, 単位は ms *)
  let rate = int_of_float (1000. *. rate) in

  (* サーバ用ソケット作成 *)
  (* 通信をしなくても作ってしまうのがいまひとつ *)
  let server_sockfd = Socket.make_sockfd () in

  let border = 0. in
  (*マウスイベントは、ボーダーが入っていようといまいとwindowの左上を原点にする
    が、wigetはボーダー分進んだ左上を原点にするので、マウスイベントの座標マイナ
    スボーダーをしてあげないと座標の数え方があわない*)
  (*でもボーダーがゼロでない限りボーダー引いてもちょっと反応がにぶいから、ボー
    ダー はゼロにしとくのがよさそう。*)

  (*windowのshowは、最初falseにしておかないと、何も表示されるものが登録されてい
    ないので、灰色の画面が出てしまう*)
  let window = GWindow.window ~border_width:(int_of_float border)
                              ~title:name
                              ~show:false
                              ~resizable:false
                              ~width:width
                              ~height:height
                              () in
  
  (* 世界 *)
  let world = ref initial_world in

  (* big_bang 内で使う各種関数群 *)

  (* 画面をクリアする *)
  let clear window = try
      window#remove window#child (* 描かれているものを消去 *)
    with Gpointer.Null -> ()     (* まだ何も描かれていなかった *)
  in

  (* 画像を描く *)
  let draw_window draw =
    begin
      clear window;
      let drawing_area = 
	GMisc.drawing_area ~width:width ~height:height ~packing:window#add () in
      let expose drawingarea ev = 
	let context = Cairo_gtk.create drawingarea#misc#window in 
	begin
	  Image.draw context (draw !world);
	  true 
	end in
      ignore(drawing_area#event#connect#expose (expose drawing_area));
      window#show ()
    end
  in

  (* stop_when !world が成立したら false になる *)
  let running = ref true in

  (* update_and_draw : t -> unit *)
  (* t を受け取り、World w なら画像を出して unit を返す。
     Packageなら、画像を出してメールを送信して unit を返す。
     ここが唯一の world を変更する場所 *)
  let update_and_draw result =
    if !running then begin (* 走っていたら world を更新して描画 *)
      begin match result with
          World w -> world := w;
        | Package (w, m) -> (world := w; Socket.send (Some m) server_sockfd )
      end;
      if stop_when !world then begin
        if !running then begin
          running := false;
          remove_watch ();
          try
            Socket.send None server_sockfd;
            (* サーバに通信から出るよっていう合図として None を送る *)
            Socket.tcflush server_sockfd;
            (* まだ読んでいないメッセージがあったら捨てる *)
            (* これをしないとエラー *)
            Socket.close server_sockfd;
            (* add_watchを解除してからserver_sockfdをcloseする *)
            (* closeして解除だとその間にreadが起きてエラーになる可能性あり *)
          with _ -> () (* 単独ゲームだとエラーになるので *)
          (* 本当は、socket.ml の方ですべて面倒を見たい *)
        end;
        draw_window draw_last(* 最後の画面を表示 *)
        (* さらに各種のイベントを受け取れないようにする *)
        (* ここで他のイベントが２度と起きないことを保証できれば
           running は不要か *)
      end else begin
        draw_window draw (* 普通の画面を表示 *)
      end
    end else () (* 走っていなかったら world は更新されない *)
  in

  (* マウスイベントの処理 *)
  let mouse_event str ev =
    let nowx = GdkEvent.Button.x ev -. border in
    let nowy = GdkEvent.Button.y ev -. border in
    let result = on_mouse !world nowx nowy str in
    update_and_draw result;
    true
  in

  let mousepressed ev = mouse_event "button_down" ev in
  let mousereleased ev = mouse_event "button_up" ev in

  (* キーイベントの処理 *)
  let key_event on_key ev =
    let value = GdkEvent.Key.keyval ev in
    let keystr = if value = GdkKeysyms._Up then "up"
                 else if value = GdkKeysyms._Down then "down"
                 else if value = GdkKeysyms._Left then "left"
                 else if value = GdkKeysyms._Right then "right"
                 else if value = GdkKeysyms._Return then "\r"
                 else if value = GdkKeysyms._Tab then "\t"
                 else if value = GdkKeysyms._BackSpace then "\b"
                 else GdkEvent.Key.string ev
    (*match value with
        GdkKeysyms._Up -> "up"
      | GdkKeysyms._Down -> "down"
      | GdkKeysyms._Left -> "left"
      | GdkKeysyms._Right -> "right"
      | GdkKeysyms._Return -> "\r"
      | GdkKeysyms._Tab -> "\t"
      | GdkKeysyms._BackSpace -> "\b"
      
      | GdkKyesyms. -> ""
       
      | _ -> GdkEvent.Key.string ev
             *)
    in
    let result = on_key !world keystr in
    update_and_draw result;
    true
  in

  let keypressed ev = key_event on_key_press ev in
  let keyreleased ev = key_event on_key_release ev in

  (* tick イベントの処理 *)
  let time_event _ =
    let result = on_tick !world in
    update_and_draw result;
    true (* falseにすると、time_eventは一回しか呼ばれない *)
  in

  (* ウィンドウのクローズボックスを押したときの処理 *)
  let delete_event ev =
    if !running then begin
      running := false; 
      remove_watch ();
      try
        Socket.send None server_sockfd;
        (* サーバに通信から出るよっていう合図として None を送る *)
        Socket.tcflush server_sockfd;
        (* まだ読んでいないメッセージがあったら捨てる *)
        (* これをしないとエラー *)
        Socket.close server_sockfd;
      with _ -> ()
    end;
    GMain.Main.quit ();
    false
  in
  


  (* メッセージを受け取ったときの処理 *)
  (* もしrunningがfalseならserver_sockfdは通信から抜けている *)
  let receive_event _ = 
   if !running then 
      begin
        let message = try Socket.receive server_sockfd with _ -> None in
        (* クライアントがメッセージを読む前にサーバがcloseしたらエラー *)
        (* その場合Noneが渡されたものとみなす*)
        match message with 
          Some message ->
          let result = on_receive !world message in
          (* メッセージを読んでon_receive !world (メッセージ)した結果 *)
          update_and_draw result; 
          true 
         |_->
           begin
             (try (
               (* Noneが送られてきたらもうサーバとの通信は切られた *)
               Socket.tcflush server_sockfd;
               (* まだ読んでいないメッセージがあったら捨てる *)
               (* これをしないとエラー *)
               Socket.close server_sockfd)
             with _ -> ());
               (* もしmessageがtry文のせいでNoneになった時にcloseが二回起きる *)
                       false
           end 
      end else 
      false
  in
 
  (* big_bang のメイン *)

  window#event#add [`BUTTON_PRESS; `BUTTON_RELEASE; `KEY_PRESS; `KEY_RELEASE];
  (* これがないとマウスを押しても反応なし *)

  (* callback の登録 *)
  ignore (window#event#connect#button_press ~callback:mousepressed);
  ignore (window#event#connect#button_release ~callback:mousereleased);
  ignore (window#event#connect#key_press ~callback:keypressed);
  ignore (window#event#connect#key_release ~callback:keyreleased);
  ignore (GMain.Timeout.add ~ms:rate ~callback:time_event);
  ignore (window#event#connect#delete ~callback:delete_event);

  (*まずサーバーに登録*)
  (*クライアントのソケットとサーバのソケットの接続*)
  let register' = match register with
    None -> obtain_addrinfo ()
  | Some (_, _) -> register
  in
  (* ここの match 文の連続はもう少しきれいにならないかと思うが... *)
  begin match register' with 
    None -> ()
  | Some (ipaddress, portnumber) -> 
      Socket.connect (ipaddress, portnumber) server_sockfd;
      (*ignore (GMain.Io.add_watch ~cond:[`IN]
                                 ~callback:receive_event
                                 ~prio:(Glib.int_of_priority `LOW)
                                 (GMain.Io.channel_of_descr server_sockfd));*)
      wid_ref := Some (GMain.Io.add_watch ~cond:[`IN]
                                 ~callback:receive_event
                                 ~prio:(Glib.int_of_priority `LOW)
                                 (GMain.Io.channel_of_descr server_sockfd));
  end;

  (* ここで最初の画面の表示 *)
  update_and_draw (World !world);

  GMain.Main.main ()
  (* big_bang の本体ここまで *)

(* GTK 起動 *)
let locale = GtkMain.Main.init ()

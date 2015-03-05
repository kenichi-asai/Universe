(* filename : universe.ml *)

(* サーバ、クライアントを表す型 *)
type iworld_t = Unix.file_descr

(* 通信に参加している client_sockfd と add_watch した時の id の組のリスト *)
let clientlst = ref []

type ('a, 'b) t =
    State of 'a (* 状態の型 *)
  | Bundle of 'a * (iworld_t * 'b) list * iworld_t list
        (* 状態の型、送り先と ['b] 型のメッセージのリスト、
           disconnect するクライアントのリスト *)

(* 宛先とメッセージの組のリストをもらって送る *)
let rec sendmail lst1 = match lst1 with
    [] -> ()
  | (first1, first2) :: rest -> begin
      if List.mem_assoc first1 !clientlst then 
        Socket.send (Some first2) first1;
      (* メッセージfirst2を送り先first1に送る *)
      sendmail rest
    end

(* on_newの初期関数 *)
let initial_new state iworld = State state

(* on_msgの初期関数 *)
let initial_msg state iworld sexp = State state

(* on_tickの初期関数 *)
let initial_tick state = State state

(* on_disconnectの初期関数 *)
let initial_disconnect state iworld = Bundle (state,[],[iworld])

(* universe *)
let universe ?(on_new=initial_new)
             ?(on_msg=initial_msg)
             ?(rate=1.0)
             ?(on_tick=initial_tick)
             ?(on_disconnect=initial_disconnect)
             initial_state
             =

  (* 以下が universe の本体 *)

  (* universe 内の大域変数 *)

  (* rate : int, 単位は ms *)
  let rate = int_of_float (1000. *. rate) in

  (* state の初期化 *)
  let state = ref initial_state in

  (* サーバー用ソケット作成 *)
  let server_sockfd = Socket.make_sockfd () in

  (* universe 内で使う各種関数群 *)

  let reset_flag = ref false in 
  (* trueになったらstateの初期値を返してサーバをリセット *)


  (* client_sockfdをもらったらそれを通信からはずす *)
  (* client_sockfdをcloseする *)
  let cut_communication client_sockfd =
    if List.mem_assoc client_sockfd !clientlst then 
      begin
        clientlst := List.remove_assoc client_sockfd !clientlst;
        Socket.close client_sockfd;
        if !clientlst = [] then 
          reset_flag := true
      end
  (* もしもともと通信から抜けていたら何もしない *)
  in
  (* update_and_send : 'a * 'b list * iworld_t list -> unit *)
  (* 状態を更新し、メールを送る。ここが唯一の state を変更する場所 *)
  (* iworldlstは通信から外したいクライアントのリスト。これが空でないとループ *)
  let rec update_and_send result =
    match result with 
      State newstate ->state := newstate (* stateを変更 *)
    | Bundle (newstate, maillst, iworldlst) ->
       state := newstate; (* stateを変更 *)
       match iworldlst with first :: rest ->
                            Socket.send None first;
                            (* クライアントにもう送らないでねって合図を送る *)
                            GMain.Io.remove (List.assoc first !clientlst);
                            (* add_watchの登録を解除 *)
                            cut_communication first; 
                            (* 通信を切る処理 *)
                            if !reset_flag 
                            then 
                              begin
                                state := initial_state;
                                reset_flag := false
                              end
                            else
                              update_and_send (Bundle (!state, maillst, rest))
                           (* restとmailの処理 *)
                           |_-> sendmail maillst (* メールを送る処理 *)
  in

  (* makemail : メッセージが来ているクライアントを見てメッセージを受信して
     bundle を作りそれに従いメッセージを送る *)
  let makemail client_sockfd = 
    if (List.mem_assoc client_sockfd !clientlst) 
    (* client_sockfd が通信に参加していることを確かめる *)
    then 
      let result = 
        let message = 
          try Socket.receive client_sockfd with _ -> None in
        (* 通信が切れていたらNoneが、そうでなければSome messageが返ってくる *)
        (* メッセージを送ってサーバが読む前にクライアントがcloseしたらエラー *)
        (* その場合はNoneが来たと考える *)
        match message with 
          None -> on_disconnect !state client_sockfd 
         |Some m -> on_msg !state client_sockfd m   
      in update_and_send result;

  in

  (* 送られてきたデータの受信 *)
  let msg_event _ = 
    let readlst = Socket.select_read (fst (List.split !clientlst)) in
    (* メッセージが来ていたfile_descrのリスト *)
    List.iter makemail readlst;
    true
  in

  (* 新しいworldが通信に参加するたびにこれが呼ばれる *)
  let new_event _ =
    (* 通信に参加した新しいクライアントの file_descr を作る *)
    let client_sockfd = Socket.new_client_sockfd server_sockfd in
    (* 監視する client_sockfd を登録 *)
    let wid = (GMain.Io.add_watch ~cond:[`IN]
                               ~callback:msg_event
                               (GMain.Io.channel_of_descr client_sockfd)) in
    (* clientlstに追加 *)
    clientlst := (client_sockfd,wid) :: !clientlst;
    (* on_new を実行して状態を更新 *)
    let result = on_new !state client_sockfd in
    update_and_send result;
    true
  in

  (* rate ごとに呼ばれる *)
  let time_event _ =
    let result = on_tick !state in
    update_and_send result;
    true
  in

  Socket.listen server_sockfd;
  let port_number = Socket.portnum server_sockfd in
  (* サーバのポート番号 *)
  (* universe のメイン *)

  (* ポート番号を標準出力で表示 *)
  print_endline (string_of_int port_number);
  flush stdout;

  ignore (GMain.Io.add_watch ~cond:[`IN]
                             ~callback:new_event
                             (GMain.Io.channel_of_descr server_sockfd));
  ignore (GMain.Timeout.add ~ms:rate ~callback:time_event);
  GMain.Main.main ()

  (* universe の本体ここまで *)

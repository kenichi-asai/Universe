(**     Universe module 
    @ja Universe モジュール
    @see <http://docs.racket-lang.org/teachpack/2htdpuniverse.html> Racket Documentation *)

(** @en the type of clients
    @ja クライアントを表す型 *)
type iworld_t

type ('a, 'b) t =
    State of 'a (**     ['a] is the type of states 
                    @ja 状態の型が ['a] *)
  | Bundle of 'a * (iworld_t * 'b) list * iworld_t list
        (**     ['a] is the type of states,
                a list of pairs of a receiver and a message of type ['b],
                a list of clients to be disconnected
            @ja 状態の型が ['a]、送り先と ['b] 型のメッセージのリスト、
                disconnect するクライアントのリスト *)

val universe :
  ?on_new:('a -> iworld_t -> ('a, 'b) t)  ->
  ?on_msg:('a -> iworld_t -> 'c -> ('a, 'b) t) ->
  ?rate: float ->
  ?on_tick:('a -> ('a, 'b) t) ->
  ?on_disconnect:('a -> iworld_t -> ('a, 'b) t) ->
  'a -> unit
  (** @en starts a game after receiving the following arguments
          (the last argument of type ['a] is mandatory)
        - [on_new] : receives a state and a newly participating client;
                     returns a new state
        - [on_msg] : receives a state, a sender, and a message;
                     returns a new state
        - [rate] : an interval of 1 tick in sec
        - [on_tick] : receives a state; returns the next state after 1 tick
        - [on_disconnect] : receives a state and a leaving client;
                            returns a new state
        - ['a] : an initial state of universe (mandatory)
      @ja 以下の引数を受け取ってゲームを開始する（最後の ['a] 型の引数のみ必須）
        - [on_new] : state と新しく通信に参加したクライアントを受け取り
                     新しい state を返す
        - [on_msg] : state と送信者とメッセージを受け取り
                     新しい state を返す
        - [rate] : 1 tick の間隔（秒）
        - [on_tick] : state を受け取り 1 tick 後の state を返す
        - [on_disconnect] : state と通信から抜けたクライアントを受け取り
                            新しい state を返す
        - ['a] : universe の初期状態（必須）
  *)

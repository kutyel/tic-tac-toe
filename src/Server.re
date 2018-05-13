module Server = BsSocket.Server.Make(Common);

open Common;

let conns = ref(0);

let store = ref(initialState(X));

/* TODO: refactor this into varient with player 1, 2 and spectator */
let getPlayer = x => x mod 2 == 0 ? O : X;

let startSocketIOServer = http => {
  let io = Server.createWithHttp(http);
  Server.onConnect(
    io,
    socket => {
      incr(conns);
      open Server;
      print_endline("Connected!");
      Socket.emit(
        socket,
        Message,
        NewState({...store^, you: getPlayer(conns^)}),
      );
      Socket.on(
        socket,
        Message,
        action => {
          store := updateState(action, store^);
          Socket.broadcast(socket, Message, action);
        },
      );
    },
  );
};
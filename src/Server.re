module Server = BsSocket.Server.Make(Common);

open Common;

let conns = ref(0);

let store = ref(initialState(X));

let getPlayer = x =>
  switch (x) {
  | 0 => X
  | 1 => O
  | _ => Spectator(x - 1)
  };

let startSocketIOServer = http => {
  let io = Server.createWithHttp(http);
  Server.onConnect(
    io,
    socket => {
      open Server;
      print_endline("Connected!");
      Socket.emit(
        socket,
        Message,
        NewState({...store^, you: getPlayer(conns^)}),
      );
      incr(conns);
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
module MyServer = BsSocket.Server.Make(Common);

open Common;

let connections = ref(0);

let initialState: Common.state = {
  grid: [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  turn: X,
  you: X,
  winner: None,
};

let playerFromConnections = x => x mod 2 == 0 ? X : O;

let startSocketIOServer = http => {
  let io = MyServer.createWithHttp(http);
  MyServer.onConnect(
    io,
    socket => {
      incr(connections);
      open MyServer;
      print_endline("Got a connection!");
      Socket.emit(
        socket,
        Common.State,
        {...initialState, you: playerFromConnections(connections^)},
      );
      /*
       let socket = Socket.join(socket, "someRoom", e => print_endline(e));
       let pipe = (typ, data) => {
         Socket.broadcast(socket, typ, data);
         Socket.emit(socket, typ, data);
         Socket.emit(socket, Common.UnusedMessageType, data);
       };
       Socket.on(socket, Common.State, pipe(Common.State));
       Socket.on(socket, Common.Movement, pipe(Common.Movement));
       */
    },
  );
};
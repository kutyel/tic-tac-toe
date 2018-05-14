module Client = BsSocket.Client.Make(Common);

let socket = Client.create();

open Common;

let component = ReasonReact.reducerComponent("Game");

let px = x => string_of_int(x) ++ "px";

let make = _children => {
  ...component,
  initialState: () => initialState(Empty),
  reducer: (action, state) =>
    switch (action) {
    | Restart => ReasonReact.Update(updateState(Restart, state))
    | Click(i) => ReasonReact.Update(updateState(Click(i), state))
    | NewState(s) => ReasonReact.Update(updateState(NewState(s), state))
    },
  didMount: ({send}) =>
    Client.on(socket, Message, data =>
      switch (data) {
      | Restart => send(Restart)
      | Click(cell) => send(Click(cell))
      | NewState(state) => send(NewState(state))
      }
    ),
  render: ({send, state: {you, turn, grid, winner}}) => {
    let yourTurn = you == turn;
    let message =
      switch (winner) {
      | None =>
        switch (you) {
        | X => yourTurn ? "Your turn (X)" : "Their turn (O)"
        | O => yourTurn ? "Your turn (O)" : "Their turn (X)"
        | Empty => "Game has not started yet..."
        | Spectator(num) => "Spectating (" ++ string_of_int(num) ++ ")"
        }
      | Some([i, ..._]) => List.nth(grid, i) == X ? "X wins!" : "O wins"
      | _ => assert false
      };
    ReasonReact.(
      <div
        style=(
          ReactDOMRe.Style.make(
            ~display="flex",
            ~width=px(439),
            ~alignItems="center",
            ~flexDirection="column",
            (),
          )
        )>
        <div style=(ReactDOMRe.Style.make(~fontSize=px(45), ()))>
          (string(message))
        </div>
        <button
          style=(
            ReactDOMRe.Style.make(
              ~fontSize=px(20),
              ~marginTop=px(8),
              ~marginBottom=px(16),
              ~border="1px solid #AAAAAA",
              ~backgroundColor="#EEEEEE",
              ~cursor="pointer",
              (),
            )
          )
          onClick=(
            _event => {
              Client.emit(socket, Message, Restart);
              send(Restart);
            }
          )>
          (string("Restart"))
        </button>
        <div
          style=(
            ReactDOMRe.Style.make(
              ~display="flex",
              ~width=px(443),
              ~height=px(443),
              ~flexWrap="wrap",
              ~justifyContent="left",
              (),
            )
          )>
          (
            /* Iterate over our grid and create the cells, with their contents and background color
               if there's a winner.*/
            array(
              Array.of_list(
                List.mapi(
                  (i, piece) => {
                    let (txt, canClick) =
                      switch (piece) {
                      | Empty
                      | Spectator(_) => (" ", true)
                      | X => ("X", false)
                      | O => ("O", false)
                      };
                    let backgroundColor =
                      switch (winner) {
                      | None => "white"
                      | Some(winner) =>
                        let isCurrentCellWinner = List.mem(i, winner);
                        if (isCurrentCellWinner && List.nth(grid, i) == you) {
                          "green";
                        } else if (isCurrentCellWinner) {
                          "red";
                        } else {
                          "white";
                        };
                      };
                    /* We check if the user can click here so we can hide the cursor: pointer. */
                    let canClick = canClick && yourTurn && winner == None;
                    <div
                      key=(string_of_int(i))
                      onClick=(
                        _event =>
                          if (canClick) {
                            Client.emit(socket, Message, Click(i));
                            send(Click(i));
                          }
                      )
                      style=(
                        ReactDOMRe.Style.make(
                          ~display="flex",
                          ~width=px(145),
                          ~height=px(145),
                          ~fontSize=px(45),
                          ~marginLeft=px(-1),
                          ~paddingTop=px(2),
                          ~marginBottom=px(-1),
                          ~justifyContent="center",
                          ~alignItems="center",
                          ~backgroundColor,
                          ~border="1px solid black",
                          ~cursor=canClick ? "pointer" : "",
                          (),
                        )
                      )>
                      <span> (string(txt)) </span>
                    </div>;
                  },
                  grid,
                ),
              ),
            )
          )
        </div>
      </div>
    );
  },
};

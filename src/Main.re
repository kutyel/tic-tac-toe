module Client = BsSocket.Client.Make(Common);

let socket = Client.create();

open Common;

/* Action declaration */
type action =
  | Restart
  | NewState(state)
  | Click(int);

let component = ReasonReact.reducerComponent("Game");

/* Helper functions for CSS properties. */
let px = x => string_of_int(x) ++ "px";

let make = _children => {
  ...component,
  initialState: () => initialState(Empty),
  reducer: (action, state) =>
    switch (action) {
    | Restart => ReasonReact.Update(updateState(Restart, state))
    | Click(i) => ReasonReact.Update(updateState(Movement(i), state))
    | NewState(s) => ReasonReact.Update(updateState(State(s), state))
    },
  didMount: self =>
    Client.on(socket, data =>
      switch (data) {
      | Restart => self.send(Restart)
      | Click(cell) => self.send(Click(cell))
      | NewState(state) => self.send(NewState(state))
      }
    ),
  render: self => {
    let yourTurn = self.state.you == self.state.turn;
    let message =
      switch (self.state.winner) {
      | None => yourTurn ? "Your turn" : "Their turn"
      | Some([i, ..._]) =>
        List.nth(self.state.grid, i) == X ? "X wins!" : "O wins"
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
              Client.emit(socket, Restart, ());
              self.send(Restart);
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
                      | Empty => (" ", true)
                      | X => ("X", false)
                      | O => ("O", false)
                      };
                    let backgroundColor =
                      switch (self.state.winner) {
                      | None => "white"
                      | Some(winner) =>
                        let isCurrentCellWinner = List.mem(i, winner);
                        if (isCurrentCellWinner
                            && List.nth(self.state.grid, i) == self.state.you) {
                          "green";
                        } else if (isCurrentCellWinner) {
                          "red";
                        } else {
                          "white";
                        };
                      };
                    /* We check if the user can click here so we can hide the cursor: pointer. */
                    let canClick =
                      canClick
                      && yourTurn
                      && self.state.winner == None
                      && self.state.you !== Empty;
                    <div
                      key=(string_of_int(i))
                      onClick=(
                        _event => {
                          Client.emit(socket, Movement(i), ());
                          canClick ? self.send(Click(i)) : ();
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
                  self.state.grid,
                ),
              ),
            )
          )
        </div>
      </div>
    );
  },
};
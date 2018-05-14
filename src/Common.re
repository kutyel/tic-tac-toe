type player =
  | X
  | O
  | Empty
  | Spectator(int);

/* State declaration.
   The grid is a simple linear list.
   The turn uses a gridCellT to figure out whether it's X or O's turn.
   The winner will be a list of indices which we'll use to highlight the grid when someone won. */
type state = {
  grid: list(player),
  turn: player,
  you: player,
  winner: option(list(int)),
};

let initialState = you => {
  grid: [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  turn: X,
  winner: None,
  you,
};

/* Action declaration */
type action =
  | Restart
  | NewState(state)
  | Click(int);

/* GADT here! */
type t('a) =
  | Message: t(action)
  | State: t(state);

let stringify = (type a, t: t(a)) =>
  switch (t) {
  | State => "State"
  | Message => "Message"
  };

let updateState = (action, state) =>
  switch (state, action) {
  | ({turn, grid}, Click(cell)) =>
    /* Apply the action to the grid first, then we check if this new grid is in a winning state.*/
    let newGrid =
      List.mapi(
        (i, el) =>
          if (cell === i) {
            turn;
          } else {
            el;
          },
        grid,
      );
    let arrGrid = Array.of_list(newGrid);
    /* Military grade, Machine Learning based, winning-condition checking algorithm:
       just list all the possible options one by one.
       */
    let winner =
      if (arrGrid[0] != Empty
          && arrGrid[0] == arrGrid[1]
          && arrGrid[1] == arrGrid[2]) {
        Some([0, 1, 2]);
      } else if (arrGrid[3] != Empty
                 && arrGrid[3] == arrGrid[4]
                 && arrGrid[4] == arrGrid[5]) {
        Some([3, 4, 5]);
      } else if (arrGrid[6] != Empty
                 && arrGrid[6] == arrGrid[7]
                 && arrGrid[7] == arrGrid[8]) {
        Some([6, 7, 8]);
      } else if (arrGrid[0] != Empty
                 && arrGrid[0] == arrGrid[3]
                 && arrGrid[3] == arrGrid[6]) {
        Some([0, 3, 6]);
      } else if (arrGrid[1] != Empty
                 && arrGrid[1] == arrGrid[4]
                 && arrGrid[4] == arrGrid[7]) {
        Some([1, 4, 7]);
      } else if (arrGrid[2] != Empty
                 && arrGrid[2] == arrGrid[5]
                 && arrGrid[5] == arrGrid[8]) {
        Some([2, 5, 8]);
      } else if (arrGrid[0] != Empty
                 && arrGrid[0] == arrGrid[4]
                 && arrGrid[4] == arrGrid[8]) {
        Some([0, 4, 8]);
      } else if (arrGrid[2] != Empty
                 && arrGrid[2] == arrGrid[4]
                 && arrGrid[4] == arrGrid[6]) {
        Some([2, 4, 6]);
      } else {
        None;
      };
    /* Return new winner, new turn and new grid. */
    {...state, winner, turn: turn === X ? O : X, grid: newGrid};
  | (_, Restart) => initialState(state.you)
  | (_, NewState(newState)) => newState
  };
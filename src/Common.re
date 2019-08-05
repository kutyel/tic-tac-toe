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

let lines = [
  (0, 1, 2),
  (3, 4, 5),
  (6, 7, 8),
  (0, 3, 6),
  (1, 4, 7),
  (2, 5, 8),
  (0, 4, 8),
  (2, 4, 6),
];

let calcWinner = squares =>
  switch (
    List.find(
      ((a, b, c)) =>
        List.nth(squares, a) != Empty
        && List.nth(squares, a) == List.nth(squares, b)
        && List.nth(squares, a) == List.nth(squares, c),
      lines,
    )
  ) {
  | (a, b, c) => Some([a, b, c])
  | exception Not_found => None
  };

let updateState = (action, state) =>
  switch (state, action) {
  | ({turn, grid}, Click(cell)) =>
    /* Apply the action to the grid first, then we check if this new grid is in a winning state. */
    let newGrid = List.mapi((i, el) => cell === i ? turn : el, grid);
    /* Military grade, Machine Learning based, winning-condition checking algorithm */
    let winner = calcWinner(newGrid);
    /* Return new winner, new turn and new grid. */
    {...state, winner, turn: turn === X ? O : X, grid: newGrid};
  | (_, Restart) => initialState(state.you)
  | (_, NewState(newState)) => newState
  };

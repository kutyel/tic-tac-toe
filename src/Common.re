/* Type representing a grid cell */
type gridCellT =
  | X
  | O
  | Empty;

type movement = (gridCellT, int);

/* State declaration.
   The grid is a simple linear list.
   The turn uses a gridCellT to figure out whether it's X or O's turn.
   The winner will be a list of indices which we'll use to highlight the grid when someone won. */
type state = {
  grid: list(gridCellT),
  turn: gridCellT,
  you: gridCellT,
  winner: option(list(int)),
};

type t('a) =
  | State: t(state)
  | Movement: t(movement)
  | UnusedMessageType: t('a);

let stringify = (type a, t: t(a)) =>
  switch (t) {
  | State => "State"
  | Movement => "Movement"
  | UnusedMessageType => "UnusedMessageType"
  };
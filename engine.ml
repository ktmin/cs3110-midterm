let roll_dice : int =
  QCheck.Gen.(generate1 (oneofl [1;2;3;4;5;6]))

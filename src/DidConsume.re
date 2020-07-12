type t =
  | Consumed(Pos.t, Pos.t)
  | NoConsume;

let append = (first, second) =>
  switch (first, second) {
  | (NoConsume, NoConsume) => NoConsume
  | (Consumed(_, _) as consumed, NoConsume)
  | (NoConsume, Consumed(_, _) as consumed) => consumed
  | (Consumed(firstStart, firstEnd), Consumed(secondStart, secondEnd)) =>
    Consumed(min(firstStart, secondStart), max(firstEnd, secondEnd))
  };

let (<@>) = append;
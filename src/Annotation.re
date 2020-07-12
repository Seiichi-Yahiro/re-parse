type t =
  | Lookahead(option(Pos.t))
  | Note(string, option(Pos.t));

let toString = annotation =>
  switch (annotation) {
  | Lookahead(optPos) =>
    [
      "successfull lookahead at",
      optPos
      ->Belt.Option.map(Pos.toString)
      ->Belt.Option.getWithDefault("end of stream"),
    ]
    ->Utils.unwords
  | Note(msg, optPos) =>
    switch (optPos) {
    | None => msg
    | Some(pos) => [msg, "at", pos->Pos.toString]->Utils.unwords
    }
  };
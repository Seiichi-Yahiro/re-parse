type t =
  | IncompleteParse(option(Pos.t))
  | UnexpectedEOF(Token.t)
  | UnexpectedToken(Token.t, option(Token.t), Pos.t)
  | UnexpectedSatisfy(Token.t, string, Pos.t)
  | UnexpectedSuccess(option(Pos.t));

let toString = error =>
  switch (error) {
  | IncompleteParse(optPos) =>
    let msg = "expected to consume the entire stream";
    switch (optPos) {
    | None => msg
    | Some(pos) =>
      [msg, "but characters remain at position", pos->Pos.toString]
      ->Utils.unwords
    };
  | UnexpectedEOF(token) => {j|expected $token but reached end of stream|j}
  | UnexpectedToken(token, optExpected, pos) =>
    let pos = pos->Pos.toString;
    switch (optExpected) {
    | Some(expected) => {j|expected $expected but got $token at $pos|j}
    | None => {j|expected end of stream but got $token at $pos|j}
    };
  | UnexpectedSatisfy(token, msg, pos) =>
    ["expected", msg, "but got", token, "at", pos->Pos.toString]
    ->Utils.unwords
  | UnexpectedSuccess(optPos) =>
    switch (optPos) {
    | Some(pos) => ["unexpected success at", pos->Pos.toString]->Utils.unwords
    | None => "unexpected success"
    }
  };
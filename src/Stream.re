type t = {
  pos: Pos.t,
  data: list(Token.t),
};

let create = str => {data: str->Tablecloth.String.split(~on=""), pos: 1};

let pop = stream =>
  switch (stream.data) {
  | [] => None
  | [x, ...xs] => (x, stream.pos, {data: xs, pos: stream.pos + 1})->Some
  };

let pointer = stream =>
  switch (stream.data) {
  | [] => None
  | _ => stream.pos->Some
  };

let isEmpty = stream => stream.data->Tablecloth.List.isEmpty;
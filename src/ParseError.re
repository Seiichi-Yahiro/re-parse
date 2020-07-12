type t =
  | Simply(Error.t)
  | OneOf(list(t))
  | Because(Annotation.t, t);

let append = (first, second) =>
  switch (first, second) {
  | (OneOf(es1), OneOf(es2)) => OneOf(es1 @ es2)
  | (OneOf(es1), b) =>
    switch (es1) {
    | [] => b
    | _ => OneOf(es1 @ [b])
    }
  | (a, OneOf(es2)) =>
    switch (es2) {
    | [] => a
    | _ => OneOf([a] @ es2)
    }
  | (a, b) => OneOf([a, b])
  };

let (<@>) = append;

let rec toString = (~depth=1, parseError) =>
  switch (parseError) {
  | Simply(error) => error->Error.toString
  | OneOf(errors) =>
    "one of the following:\n"
    ++ errors
       ->Belt.List.map(toString(~depth=depth + 1))
       ->Belt.List.map((++)("  "->Tablecloth.String.repeat(~count=depth)))
       ->Tablecloth.String.join(~sep="\n")
  | Because(annotation, error) =>
    [
      "expected",
      annotation->Annotation.toString,
      "which failed due to",
      toString(~depth=depth + 1, error),
    ]
    ->Utils.unwords
  };
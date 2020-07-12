open Jest;
open Expect;
open DidConsume;
open Parser;

describe("ReParse", () => {
  let pp = ((consumption, result): ParseOutput.t(_)) => {
    let c =
      switch (consumption) {
      | NoConsume => "NoConsume"
      | Consumed(from, to_) => {j|Consumed($(from), $(to_))|j}
      };

    let streamToString = ({pos, data}: Stream.t) => {
      let data = data->Belt.List.toArray;
      {j|{pos: $pos, data: $data}|j};
    };

    let optPosToString = optPos =>
      switch (optPos) {
      | Some(pos) => {j|Some($pos)|j}
      | None => "None"
      };

    let errorToString = error =>
      switch (error) {
      | Error.IncompleteParse(optPos) =>
        let optPos = optPos->optPosToString;
        {j|IncompleteParse($optPos)|j};
      | UnexpectedEOF(token) => {j|UnexpectedEOF($token)|j}
      | UnexpectedToken(token, optToken, pos) =>
        let optToken =
          switch (optToken) {
          | Some(token) => {j|Some($token)|j}
          | None => "None"
          };
        {j|UnexpectedToken($token, $optToken, $pos)|j};
      | UnexpectedSatisfy(token, msg, pos) => {j|UnexpectedSatisfy($token, $msg, $pos)|j}
      | UnexpectedSuccess(optPos) =>
        let optPos = optPos->optPosToString;
        {j|UnexpectedSuccess($optPos)|j};
      };

    let annotationToString = annotation =>
      switch (annotation) {
      | Annotation.Lookahead(optPos) =>
        let optPos = optPos->optPosToString;
        {j|Lookahead($optPos)|j};
      | Note(msg, optPos) =>
        let optPos = optPos->optPosToString;
        {j|Note($msg, $optPos)|j};
      };

    let rec parseErrorToString = parseError =>
      switch (parseError) {
      | ParseError.Simply(error) =>
        let error = error->errorToString;
        {j|Simply($error)|j};
      | OneOf(errors) =>
        let errors =
          errors->Belt.List.map(parseErrorToString)->Belt.List.toArray;
        {j|OneOf($errors)|j};
      | Because(annotation, error) =>
        let annotation = annotation->annotationToString;
        let error = error->parseErrorToString;
        {j|Because($annotation, $error)|j};
      };

    let r =
      switch (result) {
      | Ok({value, stream}) =>
        let stream = stream->streamToString;
        {j|Ok($(value), $stream)|j};
      | Error(parseError) =>
        let parseError = parseError->parseErrorToString;
        {j|Error($parseError)|j};
      };
    (c, r);
  };

  let split = Tablecloth.String.split(~on="");

  test("should append 2 parsers", () => {
    Combinators.append(char("h"), char("i"))->debugParser("hi")->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 2),
           {
             value: ("h", "i"),
             stream: {
               pos: 3,
               data: [],
             },
           }->Ok,
         )
         ->pp,
       )
  });

  test("should parse either", () => {
    let result = (parsed, rest) =>
      (
        Consumed(1, 1),
        {
          value: parsed,
          stream: {
            pos: 2,
            data: rest->split,
          },
        }->Ok,
      )
      ->pp;

    (
      Combinators.alternative(char("h"), char("i"))->debugParser("hi")->pp,
      Combinators.alternative(char("h"), char("i"))->debugParser("ih")->pp,
    )
    |> expect
    |> toEqual((result("h", "i"), result("i", "h")));
  });

  test("should partialy parse on alternative", () => {
    Combinators.alternative(
      Combinators.append(char("a"), char("b")),
      Combinators.append(char("a"), char("c")),
    )
    ->debugParser("ad")
    ->pp
    |> expect
    |> toEqual(
         (
           NoConsume,
           ParseError.OneOf([
             Error.UnexpectedToken("d", Some("b"), 2)->ParseError.Simply,
             Error.UnexpectedToken("d", Some("c"), 2)->ParseError.Simply,
           ])
           ->Error,
         )
         ->pp,
       )
  });

  test("should map parseResult", () => {
    Combinators.map(Belt.Int.fromString, decimalDigit)->debugParser("1")->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 1),
           {
             value: Some(1),
             stream: {
               pos: 2,
               data: [],
             },
           }->Ok,
         )
         ->pp,
       )
  });

  test("should bind parser", () => {
    Combinators.bind(decimalDigit, num => satisfies(c => c !== num, "myMsg"))
    ->debugParser("12")
    ->pp
    |> expect
    |> toEqual(
         (Consumed(1, 2), {
                             value: "2",
                             stream: {
                               pos: 3,
                               data: [],
                             },
                           }->Ok)
         ->pp,
       )
  });

  test("should return custom error msg", () => {
    Combinators.label(char("a"), "why not an a?")->debugParser("b")->pp
    |> expect
    |> toEqual(
         (
           NoConsume,
           ParseError.Because(
             Annotation.Note("why not an a?", 1->Some),
             Error.UnexpectedToken("b", Some("a"), 1)->ParseError.Simply,
           )
           ->Error,
         )
         ->pp,
       )
  });

  test("should keep left none parser value", () => {
    Combinators.Operators.(
      ("foo" <$ char("h"))->debugParser("hi")->pp
      |> expect
      |> toEqual(
           (
             Consumed(1, 1),
             {
               value: "foo",
               stream: {
                 pos: 2,
                 data: ["i"],
               },
             }->Ok,
           )
           ->pp,
         )
    )
  });

  test("should keep right value", () => {
    Combinators.Operators.(
      (char("h") *> char("i"))->debugParser("hi")->pp
      |> expect
      |> toEqual(
           (Consumed(1, 2), {
                               value: "i",
                               stream: {
                                 pos: 3,
                                 data: [],
                               },
                             }->Ok)
           ->pp,
         )
    )
  });

  test("should keep left value", () => {
    Combinators.Operators.(
      (char("h") <* char("i"))->debugParser("hi")->pp
      |> expect
      |> toEqual(
           (Consumed(1, 2), {
                               value: "h",
                               stream: {
                                 pos: 3,
                                 data: [],
                               },
                             }->Ok)
           ->pp,
         )
    )
  });

  test("should return pure", () => {
    Combinators.pure(42)->debugParser("foo")->pp
    |> expect
    |> toEqual(
         (NoConsume, {
                       value: 42,
                       stream: {
                         pos: 1,
                         data: "foo"->split,
                       },
                     }->Ok)
         ->pp,
       )
  });

  test("should parse many separatedBy", () => {
    decimalDigit
    ->Combinators.manySepBy(~sep=char("."))
    ->debugParser("1.2.3.4")
    ->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 7),
           {
             value: "1234"->split,
             stream: {
               pos: 8,
               data: [],
             },
           }->Ok,
         )
         ->pp,
       )
  });

  test("should parse many separatedEndBy", () => {
    decimalDigit
    ->Combinators.manySepEndBy(~sep=char(","))
    ->debugParser("1,2,3,4,")
    ->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 8),
           {
             value: "1234"->split,
             stream: {
               pos: 9,
               data: [],
             },
           }->Ok,
         )
         ->pp,
       )
  });

  test("should parse single character", () => {
    char("f")->debugParser("fun")->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 1),
           {
             value: "f",
             stream: {
               pos: 2,
               data: "un"->split,
             },
           }->Ok,
         )
         ->pp,
       )
  });

  test("should fail parse single character", () => {
    char("u")->debugParser("fun")->pp
    |> expect
    |> toEqual(
         (
           NoConsume,
           Error.UnexpectedToken("f", Some("u"), 1)
           ->ParseError.Simply
           ->Error,
         )
         ->pp,
       )
  });

  test("should parse eof", () => {
    eof->debugParser("")->pp
    |> expect
    |> toEqual(
         (NoConsume, {
                       value: (),
                       stream: {
                         pos: 1,
                         data: [],
                       },
                     }->Ok)->pp,
       )
  });

  test("should fail parse eof", () => {
    eof->debugParser("foo")->pp
    |> expect
    |> toEqual(
         (
           NoConsume,
           Error.UnexpectedToken("f", None, 1)->ParseError.Simply->Error,
         )
         ->pp,
       )
  });

  test("should satisfy predicate", () => {
    satisfies(c => c === "f", "myMsg")->debugParser("foo")->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 1),
           {
             value: "f",
             stream: {
               pos: 2,
               data: "oo"->split,
             },
           }->Ok,
         )
         ->pp,
       )
  });

  test("should fail satisfy predicate", () => {
    satisfies(c => c === "f", "myMsg")->debugParser("oof")->pp
    |> expect
    |> toEqual(
         (
           NoConsume,
           Error.UnexpectedSatisfy("o", "myMsg", 1)->ParseError.Simply->Error,
         )
         ->pp,
       )
  });

  test("should parse string", () => {
    string("fun")->debugParser("funsy")->pp
    |> expect
    |> toEqual(
         (
           Consumed(1, 3),
           {
             value: "fun",
             stream: {
               pos: 4,
               data: "sy"->split,
             },
           }->Ok,
         )
         ->pp,
       )
  });
});
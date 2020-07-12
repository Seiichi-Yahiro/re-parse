module ParseResult = {
  type t('a) = {
    value: 'a,
    stream: Stream.t,
  };
};

module ParseOutput = {
  type t('a) = (DidConsume.t, result(ParseResult.t('a), ParseError.t));
};

type t('a) = (Pos.t, Stream.t) => ParseOutput.t('a);

let debugParser = (parser: t(_), str) => parser(1, str->Stream.create);

let runParser = (parser: t(_), str) => {
  let stream = str->Stream.create;
  switch (parser(1, stream)) {
  | (_, Error(_) as error) => error
  | (_, Ok(result)) =>
    result.stream->Stream.isEmpty
      ? result.value->Ok
      : result.stream.pos
        ->Some
        ->Error.IncompleteParse
        ->ParseError.Simply
        ->Error
  };
};

module Combinators = {
  let append = (leftParser: t('a), rightParser: t('b)): t(('a, 'b)) =>
    (pos, stream) =>
      switch (leftParser(pos, stream)) {
      | (_, Error(_)) as error => error
      | (leftConsume, Ok(leftResult)) =>
        switch (rightParser(leftResult.stream.pos, leftResult.stream)) {
        | (_, Error(_)) as error => error
        | (rightConsume, Ok(rightResult)) => (
            DidConsume.append(leftConsume, rightConsume),
            {
              value: (leftResult.value, rightResult.value),
              stream: rightResult.stream,
            }
            ->Ok,
          )
        }
      };

  let alternative = (leftParser: t('a), rightParser: t('a)): t('a) =>
    (pos, stream) =>
      switch (leftParser(pos, stream)) {
      | (_, Ok(_)) as result => result
      | (_, Error(leftError)) =>
        switch (rightParser(pos, stream)) {
        | (_, Ok(_)) as result => result
        | (didConsume, Error(rightError)) => (
            didConsume,
            ParseError.append(leftError, rightError)->Error,
          )
        }
      };

  let choice = (parsers: list(t('a))): t('a) =>
    parsers->Utils.foldl1(~f=alternative);

  let map = (f: 'a => 'b, parser: t('a)): t('b) =>
    (pos, stream) =>
      switch (parser(pos, stream)) {
      | (_, Error(_)) as result => result
      | (_ as consumed, Ok(result)) => (
          consumed,
          {value: f(result.value), stream: result.stream}->Ok,
        )
      };

  let lift2 =
      (f: ('a, 'b) => 'c, leftParser: t('a), rightParser: t('b)): t('c) =>
    (pos, stream) =>
      switch (leftParser(pos, stream)) {
      | (_, Error(_)) as result => result
      | (leftConsumed, Ok(leftResult)) =>
        switch (rightParser(pos, leftResult.stream)) {
        | (_, Error(_)) as result => result
        | (rightConsumed, Ok(rightResult)) => (
            DidConsume.append(leftConsumed, rightConsumed),
            {
              value: f(leftResult.value, rightResult.value),
              stream: rightResult.stream,
            }
            ->Ok,
          )
        }
      };

  let bind = (parser: t('a), f: 'a => t('b)): t('b) =>
    (pos, stream) =>
      switch (parser(pos, stream)) {
      | (_, Error(_)) as result => result
      | (leftConsumed, Ok(leftResult)) =>
        let (rightConsumed, rightResult) =
          leftResult.value->f(pos, leftResult.stream);
        (DidConsume.append(leftConsumed, rightConsumed), rightResult);
      };

  let label = (parser: t(_), msg: string): t(_) =>
    (pos, stream) =>
      switch (parser(pos, stream)) {
      | (_, Ok(_) as result) => (NoConsume, result)
      | (_, Error(error)) => (
          NoConsume,
          ParseError.Because(
            Annotation.Note(msg, stream->Stream.pointer),
            error,
          )
          ->Error,
        )
      };

  let shouldFail = (parser: t(_)): t(unit) =>
    (pos, stream) =>
      switch (parser(pos, stream)) {
      | (_, Error(_)) => (NoConsume, {value: (), stream}->Ok)
      | (_, Ok(_)) => (
          NoConsume,
          stream
          ->Stream.pointer
          ->Error.UnexpectedSuccess
          ->ParseError.Simply
          ->Error,
        )
      };

  let shouldSucceed = (parser: t(_)): t(unit) =>
    (pos, stream) =>
      switch (parser(pos, stream)) {
      | (_, Ok(_)) => (NoConsume, {value: (), stream}->Ok)
      | (_, Error(error)) => (
          NoConsume,
          ParseError.Because(
            stream->Stream.pointer->Annotation.Lookahead,
            error,
          )
          ->Error,
        )
      };

  module Operators = {
    let (<@>) = append;
    let (<|>) = alternative;
    let (<$>) = map;
    let (<$) = (left, right) => Tablecloth.Fun.constant(left) <$> right;
    let (<*>) = (left, right) => lift2(Tablecloth.Fun.identity, left, right);
    let (>>=) = bind;
    let ( *> ) = (left, right) => left >>= Tablecloth.Fun.constant(right);
    let ( <* ) = (left, right) =>
      lift2(Tablecloth.Fun.constant, left, right);
    let (<?>) = label;
  };

  let pure = (value: 'a): t('a) =>
    (_pos, stream) => (NoConsume, {value, stream}->Ok);

  let defer = (parserF: unit => t(_)): t(_) =>
    (pos, stream) => parserF((), pos, stream);

  open Operators;

  let rec some = parser => Tablecloth.List.cons <$> parser <*> many(parser)
  and many = parser => defer(() => some(parser)) <|> pure([]);

  let someSepBy = (parser, ~sep) =>
    Tablecloth.List.cons <$> parser <*> many(sep *> parser);
  let someSepEndBy = (parser, ~sep) => {
    let p = parser <* sep;
    Tablecloth.List.cons <$> p <*> many(p);
  };

  let manySepBy = (parser, ~sep) => parser->someSepBy(~sep) <|> pure([]);
  let manySepEndBy = (parser, ~sep) =>
    parser->someSepEndBy(~sep) <|> pure([]);

  let repeatGreedyTill = (parser: t('a), ~till: t('b)): t(list('a)) =>
    (pos, stream) => {
      let rec aux = (~value=[], stream): ParseOutput.t(list('a)) =>
        switch (parser(pos, stream)) {
        | (_, Ok(result)) =>
          aux(~value=[result.value, ...value], result.stream)
        | (errConsume, Error(_)) as error =>
          switch (till(pos, stream)) {
          | (tillConsume, Ok(result)) => (
              DidConsume.append(errConsume, tillConsume),
              {value: value->Belt.List.reverse, stream: result.stream}->Ok,
            )
          | (_, Error(_)) => error
          }
        };

      aux(stream);
    };
};

let char = (c: Token.t): t(Token.t) =>
  (_pos, stream) =>
    switch (stream->Stream.pop) {
    | Some((x, pos, rest)) =>
      c === x
        ? (Consumed(pos, pos), {value: x, stream: rest}->Ok)
        : (
          NoConsume,
          Error.UnexpectedToken(x, c->Some, pos)->ParseError.Simply->Error,
        )
    | None => (NoConsume, c->Error.UnexpectedEOF->ParseError.Simply->Error)
    };

let eof: t(unit) =
  (_pos, stream) =>
    switch (stream->Stream.pop) {
    | Some((x, pos, _rest)) => (
        NoConsume,
        Error.UnexpectedToken(x, None, pos)->ParseError.Simply->Error,
      )
    | None => (NoConsume, {value: (), stream}->Ok)
    };

let satisfies = (predicate, msg): t(Token.t) =>
  (_pos, stream) =>
    switch (stream->Stream.pop) {
    | Some((x, pos, rest)) =>
      predicate(x)
        ? (Consumed(pos, pos), {value: x, stream: rest}->Ok)
        : (
          NoConsume,
          Error.UnexpectedSatisfy(x, msg, pos)->ParseError.Simply->Error,
        )
    | None => (NoConsume, msg->Error.UnexpectedEOF->ParseError.Simply->Error)
    };

let anyChar = satisfies(Tablecloth.Fun.constant(true), "any token");
let anyCharOf = charList => charList->Belt.List.map(char)->Combinators.choice;

let string = str => {
  Combinators.Operators.(
    str
    <$ str
       ->Tablecloth.String.split(~on="")
       ->Belt.List.map(char)
       ->Utils.foldl1(~f=( *> ))
  );
};

let spaces = char(" ")->Combinators.many;

let decimalDigit =
  satisfies("0123456789"->Js.String2.includes, "decimal digit (0-9)");

let lowerLatin =
  satisfies(
    "abcdefghijklmnopqrstuvwxyz"->Js.String2.includes,
    "lower case latin letter (a-z)",
  );

let upperLatin =
  satisfies(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"->Js.String2.includes,
    "upper case latin letter (A-Z)",
  );
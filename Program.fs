// Flop - Sometimes you fail, and that's OK
//
// This is a small investigation into error handling and recovery in FParsec
// based on the ideas discussed by @ebkalderon in [Error recovery with parser
// combinators (using nom)](https://www.eyalkalderon.com/nom-error-recovery/)

open FParsec

/// Sructured error information to be emitted by our parser.
type Diagnostic = Diagnostic of Position * string

/// The output of our parse.
type SyntaxNode =
    | Value of int64
    | Product of SyntaxNode * SyntaxNode
    | Sum of SyntaxNode * SyntaxNode
    | Error of Diagnostic

/// Binary Node Constructor. Converts the head::tail list from a binary parser
/// into a `SyntaxNode` by recursively applying `cons`. This has the property of
/// not wrapping the inner nodes if no `cdr` is at this level for a simpler
/// syntax tree.
let rec private binNode cons (car, cdr) =
    match cdr with
    | [] -> car
    | [single] -> cons(car, single)
    | head::tail -> binNode cons (cons(car, head), tail)

let rec getDiagnostics = function
    | Value _ -> []
    | Product (left, right)
    | Sum (left, right) ->
        getDiagnostics left @ getDiagnostics right
    | Error diag -> [diag]

// ~~~~~~~~~~~~~ Error Handling Constructs ~~~~~~~~~~~~~~~~~~
//
// These parsers are responsible for handling and syncrhonising after errors are
// encountered.

/// Expect a given parser to match in the current location. If the parser fails
/// the given `err` is emitted as a diagnostic at the current location.
let expect (p: Parser<'a, _>) err =
    let raiseErr (stream: CharStream<_>) =
        let diag = Diagnostic(stream.Position, err)
        Reply(Result.Error diag)
    attempt p |>> Result.Ok <|> raiseErr

/// A variant of `expet` that resolves parser failures with the `Error` syntax
/// node rather than returning an `option`al value.
let expectSyn (p: Parser<SyntaxNode, _>) err =
    expect p err |>> Result.defaultWith (fun diag -> Error diag)

/// Error syncrhonisation. used to parse any characters and emit them as
/// diagnostics. This is `<|>`ed into the standard combinator chain right at the
/// root to try and ensure the parser _always_ has a way to succeed.
let error (stream: CharStream<_>) =
    match stream.ReadCharOrNewline() with
    | EOS -> Reply(ReplyStatus.Error, expected "valid expression character")
    | ch ->
        let err = sprintf "Unexpected character %c" ch
        let diag = Diagnostic(stream.Position, err)
        Reply(Error diag)

// ~~~~~~~~~~~~~ The Parser ~~~~~~~~~~~~~~~~~~

let expr, exprRef = createParserForwardedToRef()

let value =
    (pint64  |>> Value ) <|> between (pchar '(') (expect (pchar ')') "missing closing ')'") (expectSyn expr "expected expression after opening (")

let product =
    let op = pchar '*' <|> pchar '/'
    (value .>>. (many (op >>. expectSyn value "expected expression after operator"))) |>> (binNode Product)

let sum =
    let op = pchar '+' <|> pchar '-'
    (product .>>. (many (op >>. expectSyn product "expected expression after operator"))) |>> (binNode Sum)

exprRef := sum

let parser = many (expr <|> error) .>> eof

/// Runs the parser on the input string and throws an exception if the parser
/// fails. We expect the parser should _always_ succeed. For malformed source
/// text an `SyntaxNode.Error` should be returned and a `Diagnostic` emitted.
let private parseExpr input =
    match runParserOnString parser () "test" input with
    | ParserResult.Failure(_) as f -> failwithf "Parser failed! %A" f
    | ParserResult.Success(r, _, _) -> (r, List.map getDiagnostics r)

let private test input =
    parseExpr input
    |> printfn "'%s' ~> %A" input

[<EntryPoint>]
let main argv =

    test "123"
    test "1*2"
    test "1+3*4"
    test "(1+2)*3"
    test "1+"
    test "(1"
    test "()"
    test "("
    test "(*1)+2)"

    0 // return an integer exit code

#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-elmish/Fable.Elmish.dll"
#r "../node_modules/fable-elmish-react/Fable.Elmish.React.dll"
#r "../node_modules/fable-react/Fable.React.dll"

open Fable.Core
open Fable.React
open Fable.Import.React
open Fable.Helpers.React.Props
open Fable.Helpers.React

open Elmish
open Elmish.React

type Input = 
    | Const of int
    | Plus
    | Minus 
    | Times
    | Div
    | Clear
    | Equals

type Model = InputStack of Input list

type Message = PushInput of Input

let (|Operation|_|) = function
    | Plus -> Some Plus
    | Minus -> Some Minus
    | Times -> Some Times
    | Div -> Some Div
    | _ -> None

///
/// Calculator logic
///

let concatNumber x y =
    int (sprintf "%d%d" x y)

let solve (InputStack [Const x; Operation op; Const y]) =
    match op with
    | Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
    | _ -> failwith "Will not happen"

let update (PushInput input) (InputStack xs) =
    if input = Clear then InputStack []
    else
    match xs with
    | [] ->
        match input with
        | Operation op -> InputStack []
        | Equals -> InputStack []
        | _ -> InputStack [input]
    | [Const x] ->
        match input with
        | Const y -> InputStack [Const (concatNumber x y)]
        | Operation op -> InputStack [Const x; op]
        | _ -> InputStack xs
    | [Const x; Operation op] ->
        match input with
        | Const y -> InputStack [Const x; op; Const y]
        | Operation differentOp -> InputStack [Const x; differentOp]
        | _ -> InputStack xs
    | [Const x; Operation op; Const y] ->
        match input with
        | Const y' -> InputStack [Const x; op; Const (concatNumber y y')]
        | Equals -> InputStack [Const (solve (InputStack xs))]
        | Operation op ->
            let result = solve (InputStack xs)
            InputStack [Const result; op]
        | _ -> InputStack xs
    | _ -> InputStack xs

///
/// view
///

let opString = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "x"
    | Div -> ":"
    | _ -> ""

let inputString = function
    | Operation op -> opString op
    | Const n -> string n
    | _ -> ""

let modelString (InputStack xs) = 
    xs 
    |> List.map inputString
    |> String.concat " "

// styles
let digitStyle = 
    Style [
        Height 40
        Padding 15
        TextAlign "center"
        Margin 5
        VerticalAlign "middle"
        BackgroundColor "lightgreen"
        unbox ("width", "55px")
        unbox ("font-size","24px")
        unbox ("cursor","pointer")
        unbox ("line-height","40px")
        unbox ("box-shadow", "0 0 3px black")
    ]

let opButtonStyle = 
    Style [
        Height 40
        Padding 15
        TextAlign "center"
        Margin 5
        VerticalAlign "middle"
        BackgroundColor "lightblue"
        unbox ("width", "55px")
        unbox ("font-size","24px")
        unbox ("line-height","40px")
        unbox ("cursor","pointer")
        unbox ("box-shadow", "0 0 3px black")
    ]

let calcStyle = 
    Style [
      unbox ("width", "407px")
      unbox ("border", "2px black solid")
      unbox ("border-radius", "15px")
      unbox ("padding", "10px")
    ]

// components
let text (content: string): ReactElement =
    unbox content

let digitButton digit dispatch =
    div
        [
            digitStyle
            OnClick (fun _ -> dispatch (PushInput (Const digit)))
        ]
        [ text (string digit) ]

let operationButton input dispatch =
    let content =
        match input with
        | Operation op -> opString op
        | Equals -> "="
        | Clear -> "CE"
        | _ -> ""
    div
        [
            opButtonStyle
            OnClick (fun _ -> dispatch (PushInput input))
        ]
        [ text (string content) ]

let tableRow xs =
    tr []
        [
            for x in xs ->
                td []
                    [x]
        ]

// view
let view model dispatch =
    let digit n = digitButton n dispatch
    let opBtn op = operationButton op dispatch

    div
        [ calcStyle ]
        [
            h2
                [ Style [ unbox ("padding-left", "20px"); unbox ("height", "30px")] ]
                [ text (modelString model) ]

            br [] []

            table []
                [
                    tableRow [digit 1; digit 2; digit 3; opBtn Plus]
                    tableRow [digit 4; digit 5; digit 6; opBtn Minus]
                    tableRow [digit 7; digit 8; digit 9; opBtn Times]
                    tableRow [opBtn Clear; digit 0; opBtn Equals; opBtn Div]
                ]                
        ]

let init () = InputStack []

Program.mkSimple init update view
|> Program.withReact "elmish-calc"
|> Program.run

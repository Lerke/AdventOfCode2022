namespace Day05

module Stack =
    type 'a stack = StackContents of 'a list

    let peek (StackContents stack) =
        match stack with
        | x :: xs -> Some(x)
        | _ -> None

    let push x (StackContents stack) = StackContents(x @ stack)

    let tail stack =
        match stack with
        | x :: xs -> Some(xs)
        | _ -> None

    let pop (StackContents stack) =
        match stack with
        | x :: xs -> ([ x ], StackContents xs)
        | _ -> ([], StackContents [])

    let empty = StackContents []

    let popMany number (StackContents stack) =
        (List.take ((min number (List.length stack))) stack, StackContents(List.skip (min number (List.length stack)) stack))

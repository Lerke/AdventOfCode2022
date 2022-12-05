namespace Day05

module Stack =
  type 'a stack =
    | Empty
    | Node of 'a * 'a stack
    
  let peek = function
    | Node(x, stack) -> Some(x)
    | Empty -> None

  let push x stack = Node(x, stack)
  
  let tail = function
    | Node (x, stack) -> stack
    | Empty -> Empty

  let pop =
    function
    | Node (x, stack) -> Node(x, tail stack)
    | Empty -> Empty
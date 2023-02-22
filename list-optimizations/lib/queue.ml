
type 'a t = {front : 'a list; rear : 'a list}

let empty = {front = []; rear = []}

let singleton x = {front = [x]; rear = []}

let snoc q x = {front = q.front; rear = x :: q.rear}

let uncons q =
    match q.front with
      f :: fs -> Some (f, {front = fs; rear = q.rear})
    | []      ->
        match List.rev q.rear with
          []      -> None
        | x :: xs -> Some (x, {front = xs; rear = []})

let map f q = {front = List.map f q.front; rear = List.map f q.rear}

let append q1 q2 =
    {front = q1.front @ List.rev_append q1.rear q2.front; rear = q2.rear}

let fold_right f q z =
    List.fold_right f q.front (List.fold_left (Fun.flip f) z q.rear)

let fold_left f z q =
    List.fold_right (Fun.flip f) q.rear (List.fold_left f z q.front)



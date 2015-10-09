namespace OwnCollections

    // it should work as documatated here: https://msdn.microsoft.com/en-us/library/ee353738.aspx 
    module OwnList =
        open System.Collections.Generic
        open System

        let append list1 list2 = list1 @ list2

        let average list = 
            (list |> List.sum) / (list |> List.length)

        let averageBy projection list =
            (list |> List.sumBy projection) / (list |> List.length)

        let rec choose chooser list =
            match list with 
            | [] -> []
            | head :: tail ->
                match (chooser head) with
                | Some(x) -> x :: (choose chooser tail)
                | _ -> (choose chooser tail)

        let rec collect mapping list =
            match list with 
            | [] -> []
            | head :: tail -> (mapping head) @ (collect mapping tail)

        let rec concat list =
            match list with
            | [] -> []
            | head :: tail -> head @ (concat tail)

        let empty = []

        let rec exists predicate list =
            match list with
            | [] -> false
            | head :: tail -> 
                match (predicate head) with
                | true -> true
                | _ -> exists predicate tail

        let rec exists2 predicate list1 list2 =
            match (list1, list2) with
            | ([],_) -> false
            | (_,[]) -> false
            | (head1::tail1, head2::tail2) ->
                match (predicate head1 head2) with
                | true -> true
                | _ -> exists2 predicate tail1 tail2

        let rec filter predicate list =
            match list with 
            | [] -> []
            | head :: tail -> 
                match (predicate head) with
                | true -> head :: (filter predicate tail)
                | _ -> filter predicate tail

        let rec find predicate list = 
            match list with
            | [] -> raise (KeyNotFoundException( ))
            | head :: tail -> 
                match (predicate head) with
                | true -> head
                | _ -> find predicate tail

        let rec findIndex predicate list =
            match list with
            | [] -> raise (ArgumentException( "Cannot find element"))
            | x :: xs ->
                if predicate x then 
                    0
                else
                    (findIndex predicate xs ) + 1

        let rec fold folder state list =
            match list with 
            | [] -> state
            | head :: tail -> fold folder (folder state head) tail

        let rec fold2 folder state list1 list2 =
            match (list1,list2) with
            | (_,[]) -> state
            | ([],_) -> state
            | (head1::tail1,head2::tail2) -> fold2 folder (folder state head1 head2) tail1 tail2
        
        let foldBack folder state list =
            list |> List.rev |> fold folder state

        let foldBack2 folder state list1 list2 = 
            fold2 folder state (list1 |> List.rev) (list2 |> List.rev) 

        let rec forall predicate list =
            match list with
            | [] -> true
            | head :: tail ->
                match (predicate head) with
                | true -> forall predicate tail
                | _ -> false

        let rec forall2 predicate list1 list2 = 
            match (list1, list2) with
            | ([],[]) -> true
            | (head1::tail1,head2::tail2) -> 
                match (predicate head1 head2) with
                | true -> forall2 predicate tail1 tail2
                | false -> false
            | _ -> raise (ArgumentException("Lists must be the same legth"))

        let head list =
            match list with
            | [] -> raise (ArgumentException("list is empty"))
            | head::_ -> head

        let init length initializer =
            [0..length-1] |> List.map initializer

        let isEmpty list =
            match list with
            | [] -> true
            | _ -> false

        let rec iter action list =
            match list with
            | [] -> ()
            | head::tail -> 
                action head
                iter action tail

        let rec iter2 action list1 list2 =
            match (list1,list2) with
            | ([],[]) -> ()
            | (head1::tail1,head2::tail2) -> 
                action head1 head2
                iter2 action tail1 tail2
            | _ -> raise (ArgumentException("Lists must be the same legth"))

        let rec length list =
            match list with
            | [] -> 0
            | _::tail -> 1 + (length tail)

        let rec map mapping list =
            match list with
            | [] -> []
            | head :: tail -> (mapping head) :: (map mapping tail)

        let rec map2 mapping list1 list2 =
            match (list1,list2) with
            | ([],_) -> []
            | (_,[]) -> []
            | (head1::tail1,head2::tail2) -> (mapping head1 head2) :: (map2 mapping tail1 tail2)

        let rec map3 mapping list1 list2 list3 =
            match (list1,list2,list3) with
            | ([],_,_) -> []
            | (_,[],_) -> []
            | (_,_,[]) -> []
            | (head1::tail1,head2::tail2,head3::tail3) ->
                (mapping head1 head2 head3) :: (map3 mapping tail1 tail2 tail3)

        let mapi mapping list =
            let rec mapIndex mapping list index =
                match list with
                | [] -> []
                | head :: tail -> (mapping index head) :: (mapIndex mapping tail (index+1))
            mapIndex mapping list 0

        let mapi2 mapping list1 list2 =
            let rec mapIndex mapping list1 list2 index =
                match (list1,list2) with
                | (_,[]) -> []
                | ([],_) -> []
                | (head1::tail1,head2::tail2) -> (mapping index head1 head2) :: (mapIndex mapping tail1 tail2 (index+1))
            mapIndex mapping list1 list2 0

        let rec max list =
            match list with
            | [] -> raise (ArgumentException("list is empty"))
            | head::tail -> 
                match tail with
                | [] -> head
                | _ -> Operators.max head (max tail)

        let maxBy projection list =
            let rec maxByi projection lastMax list = 
                match list with
                | [] -> lastMax
                | head::tail -> 
                    if Operators.max (projection head) (projection lastMax) = (projection head) then
                        maxByi projection head tail
                    else
                        maxByi projection lastMax tail

            match list with 
            |[] -> raise (ArgumentException("list is empty"))
            | head::tail -> maxByi projection head tail
                        
        let rec min list =
            match list with
            | [] -> raise (ArgumentException("list is empty"))
            | head::tail -> 
                match tail with
                | [] -> head
                | _ -> Operators.min head (min tail)

        let minBy projection list =
            let rec minByi projection lastMin list = 
                match list with
                | [] -> lastMin
                | head::tail -> 
                    if Operators.min (projection head) (projection lastMin) = (projection head) then
                        minByi projection head tail
                    else
                        minByi projection lastMin tail

            match list with 
            |[] -> raise (ArgumentException("list is empty"))
            | head::tail -> minByi projection head tail

        let rec nth list n =
            match list with
            | [] -> raise (ArgumentException("empty list of out of range"))
            | head::tail -> if n=0 then head else nth tail n-1

        let ofArray array = 
            Array.toList array

        let ofSeq seq =
            Seq.toList seq

        let rec partition predicate list =
            match list with
            | [] -> ([],[])
            | head::tail -> 
                let (t,f) = partition predicate tail
                match (predicate head) with
                | true -> (head::t,f)
                | false -> (t,head::f)

        let permute indexMap list = 
            failwith "no idea for implementation"


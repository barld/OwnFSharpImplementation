namespace OwnCollections

    // it should work as documatated here: https://msdn.microsoft.com/en-us/library/ee353738.aspx 
    module OwnList =
        open System.Collections.Generic

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

        let rec find predicate (list : 'a list)= 
            match list with
            | [] -> raise (KeyNotFoundException( ))
            | head :: tail -> 
                match (predicate head) with
                | true -> head
                | _ -> find predicate tail

        let rec findIndex predicate list =
            let mutable counter = -1
            let predicateCountWrapper (item : 'a) =
                counter <- counter + 1
                predicate item

            //assign to a value for the compiler
            let temp = find predicateCountWrapper list

            counter

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
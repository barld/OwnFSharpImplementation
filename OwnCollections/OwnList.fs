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
                    let headProjection = (projection head)
                    if Operators.max headProjection (projection lastMax) = headProjection then
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
                    let headProjection =  (projection head)
                    if Operators.min headProjection (projection lastMin) = headProjection then
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
            let listLength = length list
            let rec inMap index list =
                match list with
                | [] -> []
                | head::tail -> 
                    match (indexMap index) with
                    | nIndex when nIndex < 0 || nIndex >= listLength -> raise (ArgumentOutOfRangeException())
                    | nIndex -> (nIndex, head) :: (inMap (index+1) tail)
             
            let mappedList = inMap 0 list   
            //check if all indexes are unique
            let rec check list =
                match list with
                | [] -> true
                | (index,item)::tail ->
                    match (forall (fun (index2,_) -> index2<>index)tail) with
                    | true -> check tail
                    | false -> false

            if (check mappedList) then
                mappedList
                |> List.sortBy (fun (index,item) -> index)
                |> map (fun (_,item) -> item)
            else
                raise (ArgumentException("dubbele index"))

        let rec pick chooser list =
            match list with
            | [] -> raise (KeyNotFoundException("key not found"))
            | head::tail -> 
                match (chooser head) with
                | None -> pick chooser tail
                | Some(value) -> value

        let reduce reduction list =

            let rec inReduce acc list =
                match list with
                | [] -> acc
                | head::tail -> inReduce (reduction acc head) tail

            match list with
            | [] -> raise (ArgumentException("empty list"))
            | head::tail -> inReduce head tail

        let reduceBack reduction list =
            list |> List.rev |> reduce reduction

        let rec replicate count initial =
            match count with
            | n when n < 0 -> raise (ArgumentException("count must be non negative"))
            | 0 -> []
            | _ -> initial :: replicate (count-1) initial

        let rec rev list =
            match list with
            | [] -> []
            | head::tail -> (rev tail) @ [head]

        let rec scan folder state list =
            match list with
            | [] -> [state]
            | head::tail -> state :: (scan folder (folder state head) tail)

        let scanBack folder list state =
            let rec inScanBack list state = 
                match list with
                | [] -> [state]
                | head::tail -> (inScanBack tail (folder head state)) @ [state]
            inScanBack (list |> rev) state

        let rec sort list =
            // simple quicksort algorithm
            match list with
            | [] -> []
            | head::tail -> 
                let before = filter (fun x -> (Operators.compare x head) = -1) tail
                let after = filter (fun x -> Operators.compare x head > -1) tail
                concat [(sort before); [head];(sort after)]

        let sortBy projection list =
            let rec inSortBy list = 
                match list with
                | [] -> []
                | (projected,item)::tail ->
                    let before = filter (fun (a,b) -> (Operators.compare a projected) = -1) tail
                    let after = filter (fun (a,b) -> (Operators.compare a projected) > -1) tail
                    concat [(inSortBy before);[(projected,item)];(inSortBy after)]
            let projectedList = map (fun x -> (projection x, x)) list
            let sortedList = inSortBy projectedList
            map (fun (a,b)->b) sortedList

        let rec sortWith comparer list =
            match list with
            | [] -> []
            | head::tail -> 
                let before = filter (fun x -> (comparer x head) = -1) tail
                let after = filter (fun x -> comparer x head > -1) tail
                concat [(sort before); [head];(sort after)]

        let inline add a b = a+b

        let rec sum (list : 'a list) : 'a =    
            failwith "not generic after compiling" 
            let zero:'a = LanguagePrimitives.GenericZero 
            match list with
            | [] -> zero
            | head::tail -> (head + (sum tail)) :  'a

        let rec sumBy projection list =
            failwith "not generic"
            match list with
            | [] -> LanguagePrimitives.GenericZero
            | head::tail -> (projection head) + (sumBy projection tail)

        let tail list =
            match list with
            | [] -> raise (ArgumentException("The input list was empty"))
            | _::tail -> tail

        let toArray list =
            Array.ofList list

        let toSeq list =
            Seq.ofList list

        let rec tryFind predicate list =
            match list with
            | [] -> None
            | head::tail ->
                match (predicate head) with
                | true -> Some head
                | false -> tryFind predicate tail

        let rec tryFindIndex predicate list =
            match list with
            | [] -> None
            | head::tail ->
                match (predicate head) with
                | true -> Some 0
                | false -> 
                    match (tryFindIndex predicate tail) with
                    | None -> None
                    | Some(n) -> Some(n+1)
            
        let rec tryPick chooser list =
            match list with
            | [] -> None
            | head::tail ->
                match (chooser head) with
                | None -> tryPick chooser tail
                | Some(item) -> Some(item)

        let rec unzip list = 
            match list with
            | [] -> ([],[])
            | (a,b)::tail -> 
                let c,d = unzip tail
                (a::c,b::d)

        let rec unzip3 list =
            match list with
            | [] -> ([],[],[])
            | (a,b,c)::tail ->
                let d,e,f = unzip3 tail
                (a::d,b::e,c::f)

        let rec zip list1 list2 =
            match (list1,list2) with
            | ([],[]) -> []
            | ([],_) -> raise (ArgumentException("list is not of same length"))
            | (_,[]) -> raise (ArgumentException("list is not of same length"))
            | (head1::tail1,head2::tail2) -> (head1,head2)::zip tail1 tail2

        let rec zip3 list1 list2 list3 =
            match (list1,list2,list3) with
            | ([],[],[]) -> []
            | ([],_,_) -> raise (ArgumentException("list is not of same length"))
            | (_,[],_) -> raise (ArgumentException("list is not of same length"))
            | (_,_,[]) -> raise (ArgumentException("list is not of same length"))
            | (head1::tail1,head2::tail2,head3::tail3) -> (head1,head2,head3)::zip3 tail1 tail2 tail3

#load "Common.Types.fsx"
#load "OrderTaking.Types.fsx"
#load "Shipping.Types.fsx"
open OrderTaking

let printList1 aList =
  match aList with
    | [] ->
      printfn "list is empty"
    | [x] ->
      printfn "list has one element: %A" x
    | [x;y] ->
      printfn "list has two elements: %A and %A" x y
    | _ ->
      printfn "list has more than two elements"

let rec printListRecurssive aList =
  match aList with
    | [] ->
      printfn ""
    | first::rest ->
      printfn "%A, " first; printListRecurssive rest

printList1 [1]
printList1 [1; 2]
printList1 [1; 2; 3]
printListRecurssive [1; 2; 3]


type SaveCustomer = Orders.CustomerInfo -> unit
type PayInvoice = Orders.UnpaidInvoice -> Payments.Payment -> Orders.PaidInvoice
type ConvertPaymentCurrency = Payments.Payment -> Payments.Currency -> Payments.Payment


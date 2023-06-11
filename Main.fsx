#load "Common.Types.fsx"
#load "OrderTaking.Types.fsx"
#load "Shipping.Types.fsx"
#load "Contacts.fsx"
open OrderTaking.Orders
open Contacts

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

// Value object equality
let widgetCode1 = WidgetCode "W1234"
let widgetCode2 = WidgetCode "W1234"
printfn "are widgetCode1 %A and widgetCode2 equal %A %b" widgetCode1 widgetCode2 (widgetCode1 = widgetCode2)

// Entity object equality
let contactId = ContactId 1

let contact1 = {
  ContactId = contactId
  PhoneNumber = PhoneNumber "1234567890"
  EmailAddress = EmailAddress "aa@bb.com"
}

let contact2 = {
  ContactId = contactId
  PhoneNumber = PhoneNumber "0987654321"
  EmailAddress = EmailAddress "bb@cc.com"
}

printfn "is contact1 %A equal to %A: %b" contact1 contact2 (contact1 = contact2)

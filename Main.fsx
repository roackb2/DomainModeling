#load "Common.Types.fsx"
#load "OrderTaking.Types.fsx"
#load "Shipping.Types.fsx"
open OrderTaking.Domain.Payments
open OrderTaking.Domain.Orders
open OrderTaking.Domain.Contacts

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

let orderLine1 = {
  Id = OrderLineId 1
  OrderId = OrderId 1
  ProductCode = Widget (WidgetCode "W1234")
  OrderQuantity = Unit (UnitQuantity 2)
  Price = {
    Currency = USD
    Amount = PaymentAmount 3
  }
}

let orderLine2 = {
  Id = OrderLineId 1
  OrderId = OrderId 1
  ProductCode = Widget (WidgetCode "W1234")
  OrderQuantity = Unit (UnitQuantity 5)
  Price = {
    Currency = USD
    Amount = PaymentAmount 5
  }
}

printfn "comparing using Key: is orderLine1 %A equal to %A: %b" orderLine1 orderLine2 (orderLine1.Key = orderLine2.Key)

// Immutability
let initialContact = {
  ContactId = ContactId 1
  PhoneNumber = PhoneNumber "1234567890"
  EmailAddress = EmailAddress "john@wick.com"
}

let updatedPerson = { initialContact with PhoneNumber = PhoneNumber "0987654321" }

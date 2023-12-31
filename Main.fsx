#load "Common/Types.fsx"
#load "OrderTaking/BaseTypes.fsx"
#load "OrderTaking/PlaceOrderWorkflow.fsx"
#load "Shipping.Types.fsx"

open Common
open OrderTaking.BaseTypes.Contacts
open OrderTaking.BaseTypes.Payments
open OrderTaking.BaseTypes.Shopping
open OrderTaking.BaseTypes.Orders

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
  EmailAddress = EmailAddress.create "aa@bb.com"
}

let contact2 = {
  ContactId = contactId
  PhoneNumber = PhoneNumber "0987654321"
  EmailAddress = EmailAddress.create "bb@cc.com"
}

printfn "is contact1 %A equal to %A: %b" contact1 contact2 (contact1 = contact2)

let orderLine1 = {
  OrderLineId = OrderLineId.create "1"
  OrderId = OrderId.create "1"
  ProductCode = Widget (WidgetCode "W1234")
  Quantity = Unit (UnitQuantity.CreateUnitQuantity 2)
  Price = {
    Currency = USD
    Amount = PaymentAmount 3m
  }
}

let orderLine2 = {
  OrderLineId = OrderLineId.create "1"
  OrderId = OrderId.create "1"
  ProductCode = Widget (WidgetCode "W1234")
  Quantity = Kilogram (KilogramQuantity.CreateKilogramQuantity 5.3m)
  Price = {
    Currency = USD
    Amount = PaymentAmount 5m
  }
}

printfn "comparing using Key: is orderLine1 %A equal to %A: %b" orderLine1 orderLine2 (orderLine1.Key = orderLine2.Key)

// Immutability
let initialContact = {
  ContactId = ContactId 1
  PhoneNumber = PhoneNumber "1234567890"
  EmailAddress = EmailAddress.create "john@wick.com"
}

let updatedPerson = { initialContact with PhoneNumber = PhoneNumber "0987654321" }

let amount1 = PaymentAmount 2.0m
let amount2 = PaymentAmount 3.0m
let sum = amount1 + amount2

let item = {
  ItemId = "item_1"
  Name = "Shampoo"
}

let myCart = EmptyCart
let activeCart =  addItem myCart item
let payment = {
  Amount = PaymentAmount 2000.0m
  Currency = USD
  Method = Cash
}
let paidCart = makePayment activeCart payment
printfn "paidCart: %A" paidCart

// Currying
let add x y = x + y
let addGenerator x = fun y -> x + y
let add3 = addGenerator 3
printfn "add3 4: %d" (add3 4) // ouputs 7

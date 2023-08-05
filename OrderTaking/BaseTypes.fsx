namespace OrderTaking.BaseTypes

#load "../Common/Types.fsx"
open Common.Types
open System
open System.Text.RegularExpressions

module Contacts =
  type PhoneNumber = PhoneNumber of string
  type EmailAddress = private EmailAddress of string
  module EmailAddress =
    let create str =
      let regex = Regex(@"^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$", RegexOptions.Compiled)
      if String.IsNullOrEmpty(str) then
        failwith "EmailAddress cannot be empty"
      else if not (regex.IsMatch(str)) then
        failwith "EmailAddress is not valid"
      else
        EmailAddress str
    let value (EmailAddress str)  = str
  type ContactId = ContactId of int
  [<CustomEquality; NoComparison>]
  type Contact = {
    ContactId: ContactId
    PhoneNumber: PhoneNumber
    EmailAddress: EmailAddress
  }
  with
  override this.Equals(obj) =
    match obj with
    | :? Contact as c -> this.ContactId = c.ContactId
    | _ -> false
  override this.GetHashCode() =
    hash this.ContactId

module Payments =
  type USD = USD
  type EUD = EUD
  type CheckNumber = CheckNumber of int
  type CardNumber = CardNumber of string
  type CardType = Visa | MasterCard
  type CreditCardInfo = {
    CardNumber: CardNumber
    CardType: CardType
  }
  type PaymentMethod =
    | Cash
    | Check of CheckNumber
    | CreditCard of CreditCardInfo
  type PaymentAmount = PaymentAmount of decimal with
    static member Zero = PaymentAmount 0m
    static member (+) (PaymentAmount a, PaymentAmount b) = PaymentAmount (a + b)
    static member (-) (PaymentAmount a, PaymentAmount b) = PaymentAmount (a - b)
    static member (*) (PaymentAmount a, PaymentAmount b) = PaymentAmount (a * b)
    static member (/) (PaymentAmount a, PaymentAmount b) = PaymentAmount (a / b)
  module PaymentAmount =
    let value (PaymentAmount amt) = amt
  type Currency = USD | EUR
  type Payment = {
    Amount: PaymentAmount
    Currency: Currency
    Method: PaymentMethod
  }
  type PaymentError =
    | CardTypeNotRecognized
    | PaymentRejected
    | PaymentProviderOffline
  type Price = {
    Currency: Currency
    Amount: PaymentAmount
  }
  module Price =
    let multiply price qty =
      {
        Currency = price.Currency
        Amount = PaymentAmount (PaymentAmount.value price.Amount * qty)
      }
  type BillingAmount = PaymentAmount
  module BillingAmount =
    let sumPrices (prices: NonEmptyList<Price>) =
      let total = prices.sumBy (fun price -> PaymentAmount.value price.Amount)
      {
        Amount = PaymentAmount total
        Currency = prices.First.Currency
      }
    let value (PaymentAmount amt) = amt

module Shopping =
  type Item = {
    ItemId: string
    Name: string
    // ...
  }
  type ActiveCartData = {
    UnpaidItems: Item list
  }
  type PaidCardData = {
    PaidItems: Item list
    Payment: Payments.Payment
  }
  type ShoppingCart =
    | EmptyCart
    | ActiveCart of ActiveCartData
    | PaidCart of PaidCardData

  let addItem cart item =
    match cart with
    | EmptyCart _ -> ActiveCart { UnpaidItems = [item] }
    | ActiveCart { UnpaidItems = items } -> ActiveCart { UnpaidItems = item :: items }
    | PaidCart _ -> failwith "Cannot add item to paid cart"
  let makePayment cart payment =
    match cart with
    | EmptyCart _ -> failwith "Cannot pay for empty cart"
    | ActiveCart { UnpaidItems = items } -> PaidCart { PaidItems = items; Payment = payment }
    | PaidCart _ -> failwith "Cannot pay for already paid cart"

module Orders =
  type WidgetCode = WidgetCode of string
    // constraint: starts with "W" then 4 digits
  type GizmoCode = GizmoCode of string
    // constraint: starts with "G" then 3 digits
  type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode
  module ProductCode =
    let create str =
      if String.IsNullOrEmpty(str) then
        failwith "ProductCode cannot be empty"
      else if str.StartsWith("W") then
        Widget (WidgetCode str)
      else if str.StartsWith("G") then
        Gizmo (GizmoCode str)
      else
        failwith "ProductCode must start with W or G"
    let value = function
      | Widget (WidgetCode str) -> str
      | Gizmo (GizmoCode str) -> str

  [<Measure>]
  type kg
  // Order Quantity related
  type KilogramQuantity = private KilogramQuantity of decimal<kg>
  module KilogramQuantity =
    let create qty =
      if qty < 0m<kg> then
        Error "Quantity cannot be negative"
      else if qty > 1000m<kg> then
        Error "Quantity cannot be greater than 1000"
      else
        Ok (KilogramQuantity qty)
    let value (KilogramQuantity qty) = qty
    let CreateKilogramQuantity qty =
      match create (qty * 1.0m<kg>) : Result<KilogramQuantity,string>  with
        | Ok qty -> qty
        | Error msg -> failwith msg
  type UnitQuantity = private UnitQuantity of int
  module UnitQuantity =
    let create qty =
      if qty < 1 then
        Error "Quantity cannot be negative"
      else if qty > 1000 then
        Error "Quantity cannot be greater than 1000"
      else
        Ok (UnitQuantity qty)
    let value (UnitQuantity qty) = qty
    let CreateUnitQuantity qty =
      match create qty : Result<UnitQuantity,string>  with
        | Ok qty -> qty
        | Error msg -> failwith msg
  type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity
  // Order related
  type ProductId = ProductId of int
  type OrderId = private OrderId of string
  module OrderId =
    let create str =
      if String.IsNullOrEmpty(str) then
        failwith "OrderId cannot be empty"
      else if str.Length > 50 then
        failwith "OrderId cannot be longer than 50 characters"
      else
        OrderId str
    let value (OrderId str) = str

  type OrderLineId = private OrderLineId of string
  module OrderLineId =
    let create str =
      if String.IsNullOrEmpty(str) then
        failwith "OrderLineId cannot be empty"
      else if str.Length > 50 then
        failwith "OrderLineId cannot be longer than 50 characters"
      else
        OrderLineId str
    let value (OrderLineId str) = str
  type CustomerId = CustomerId of int
  type UnvalidatedCustomerInfo = {
    FirstName: string
    LastName: string
    EmailAddress: Contacts.EmailAddress
  }
  type CustomerInfo = {
    PersonalName: PersonalName
    EmailAddress: Contacts.EmailAddress
  }
  type UnvalidatedAddress = {
    AddressLine1: string
    AddressLine2: string
    City: string
    State: string
    Country: string
    PostalCode: string
  }

  type UnvalidatedOrderLine = {
    OrderLineId: OrderLineId // id for entity
    OrderId: OrderId
    ProductCode: ProductCode
    Quantity: decimal
    Price: Payments.Price
  }

  type PricedOrderLine = {
    OrderLineId: OrderLineId // id for entity
    OrderId: OrderId
    ProductCode: ProductCode
    Quantity: OrderQuantity
    Price: Payments.Price
  }
  with
  member this.Key =
    (this.OrderId, this.ProductCode)
  end

module Uncategorized =
  type EnvelopeContents = EnvelopeContents of string
  type QuoteForm = Undefined
  type OrderForm = Undefined
  type CategorizedMail =
    | Quote of QuoteForm
    | Order of OrderForm
  type UnpaidInvoice = Undefined
  type PaidInvoice = Undefined
  type ProductCatalog = Undefined
  // composite types
  type CalculatePricesInput = {
    OrderForm: OrderForm
    ProductCatalog: ProductCatalog
  }


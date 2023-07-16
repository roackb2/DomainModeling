namespace OrderTaking.BaseType

#load "../Common/Types.fsx"
open Common.Types

module Contacts =
  type PhoneNumber = PhoneNumber of string
  type EmailAddress = EmailAddress of string
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
  type BillingAmount = Undefined

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
  [<Measure>]
  type kg
  // Order Quantity related
  type KilogramQuantity = KilogramQuantity of decimal<kg>
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
  type OrderId = OrderId of int
  type OrderLineId = OrderLineId of int
  type CustomerId = CustomerId of int
  type CustomerInfo = Undefined
  type BaseAddress = {
    AddressLine1: string
    AddressLine2: string
    City: string
    State: string
    Country: string
    PostalCode: string
    // ...
  }
  type ShippingAddress = ShippingAddress of BaseAddress
  type BillingAddress = BillingAddress of BaseAddress
  type OrderLine = {
    Id: OrderLineId // id for entity
    OrderId: OrderId
    ProductCode: ProductCode
    OrderQuantity: OrderQuantity
    Price: Payments.Price
  }
  with
  member this.Key =
    (this.OrderId, this.ProductCode)
  end
  type BaseOrder = {
    Id: OrderId // id for entity
    CustomerId: CustomerId // customer reference
    ShppingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: NonEmptyList<OrderLine>
    AmountToBill: Payments.PaymentAmount
  }
  type HtemlString = HtemlString of string
  type OrderAcknowledgement = {
    EmailAddress: Contacts.EmailAddress
    Letter: HtemlString
  }
  type SentResult = Sent | NotSent
  type PricedOrder = PricedOrder of BaseOrder
  type Order =
    | UnvalidatedOrder
    | UnplacedOrder
    | PlacedOrder
    | PricedOrder

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


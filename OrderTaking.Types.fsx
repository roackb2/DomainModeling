namespace OrderTaking.Domain

#load "Common.Types.fsx"
open Common

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
  type PaymentAmount = PaymentAmount of decimal
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

module Orders =
  // Product Code related
  type WidgetCode = WidgetCode of string
    // constraint: starts with "W" then 4 digits
  type GizmoCode = GizmoCode of string
    // constraint: starts with "G" then 3 digits
  type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

  // Order Quantity related
  type KilogramQuantity = KilogramQuantity of decimal
  type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity

  // Order related
  type ProductId = ProductId of int
  type OrderId = OrderId of int
  type OrderLineId = OrderLineId of int
  type CustomerId = CustomerId of int
  type CustomerInfo = Undefined
  type ShippingAddress = Undefined
  type BillingAddress = Undefined
  type Price = {
    Currency: Payments.Currency
    Amount: Payments.PaymentAmount
  }
  type BillingAmount = Undefined
    [<NoEquality;NoComparison>]
  type OrderLine = {
    Id: OrderLineId // id for entity
    OrderId: OrderId
    ProductCode: ProductCode
    OrderQuantity: OrderQuantity
    Price: Price
  }
  with
  member this.Key =
    (this.OrderId, this.ProductCode)
  end
  type Order = {
    Id: OrderId // id for entity
    CustomerId: CustomerId // customer reference
    ShppingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: OrderLine list
    AmountToBill: BillingAmount
  }
  type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: CustomerInfo
    ShippingAddress: ShippingAddress
    // ...
  }

  type EnvelopeContents = EnvelopeContents of string
  type QuoteForm = Undefined
  type OrderForm = Undefined
  type CategorizedMail =
    | Quote of QuoteForm
    | Order of OrderForm


  type UnpaidInvoice = Undefined
  type PaidInvoice = Undefined
  type ValidatedOrder = Undefined
  type UnplacedOrder = Undefined
  type PlacedOrder = Undefined
  type PricedOrder = Undefined
  type ProductCatalog = Undefined
  type AcknowledgementSent = AcknowledgementSent of bool
  type OrderPlaced = OrderPlaced of bool
  type BillableOrderPlaced = BillableOrderPlaced of bool
  // composite types

  type CalculatePricesInput = {
    OrderForm: OrderForm
    ProductCatalog: ProductCatalog
  }
  // Events
  type PlaceOrderEvent = {
    AcknowledgementSent: AcknowledgementSent
    OrderPlaced: OrderPlaced
    BiilableOrderPlaced: BillableOrderPlaced
  }
  // Errors
  type PlaceOrderError =
  | ValidationError of ValidationError list
  // Processes
  type ValidateOrder = UnvalidatedOrder -> ValidationResponse<ValidatedOrder>
  type PlaceOrder = UnvalidatedOrder -> Result<PlaceOrderEvent, PlaceOrderError>
  type CalculatePrices = CalculatePricesInput -> PricedOrder
  type SaveCustomer = CustomerInfo -> unit
  type PayInvoice = UnpaidInvoice -> Payments.Payment -> PaidInvoice
  type ConvertPaymentCurrency = Payments.Payment -> Payments.Currency -> Payments.Payment


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

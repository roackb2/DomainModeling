module rec OrderTaking

#load "Common.Types.fsx"
open Common

module Payments =
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

module Orders =
  // basic types
  type CustmerId = CustomerId of int
  type ProductId = ProductId of int
  type WidgetCode = WidgetCode of string
  type GizmoCode = GizmoCode of string
  type OrderId = OrderId of int
  type UnitQuantity = UnitQuantity of int
  type KilogramQuantity = KilogramQuantity of decimal
  type EnvelopeContents = EnvelopeContents of string
  type QuoteForm = Undefined
  type OrderForm = Undefined
  type CategorizedMail =
    | Quote of QuoteForm
    | Order of OrderForm
  type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode
  type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity
  type CustomerInfo = Undefined
  type ShippingAddress = Undefined
  type BillingAddress = Undefined
  type BillingAmount = Undefined
  type UnpaidInvoice = Undefined
  type PaidInvoice = Undefined
  type UnvalidatedOrder = Undefined
  type ValidatedOrder = Undefined
  type UnplacedOrder = Undefined
  type PlacedOrder = Undefined
  type PricedOrder = Undefined
  type ProductCatalog = Undefined
  type AcknowledgementSent = AcknowledgementSent of bool
  type OrderPlaced = OrderPlaced of bool
  type BillableOrderPlaced = BillableOrderPlaced of bool
  // composite types
  [<NoEquality;NoComparison>]
  type OrderLine = {
    OrderId: OrderId
    ProductId: ProductId
    Qty: int
  }
  with
  member this.Key =
    (this.OrderId, this.ProductId)
  end
  type Order = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShppingAddress: ShippingAddress
    BillingAddress: BillingAddress
    Lines: OrderLine list
    AmountToBill: BillingAmount
  }
  type CalculatePricesInput = {
    OrderForm: OrderForm
    ProductCatalog: ProductCatalog
  }
  // Processes
  type ValidateOrder = UnvalidatedOrder -> ValidationResponse<ValidatedOrder>
  type PlaceOrder = UnplacedOrder -> PlacedOrder
  type CalculatePrices = CalculatePricesInput -> PricedOrder
  type SaveCustomer = CustomerInfo -> unit
  type PayInvoice = UnpaidInvoice -> Payments.Payment -> PaidInvoice
  type ConvertPaymentCurrency = Payments.Payment -> Payments.Currency -> Payments.Payment
  // Events
  type PlaceOrderEvent = {
    AcknowledgementSent: AcknowledgementSent
    OrderPlaced: OrderPlaced
    BiilableOrderPlaced: BillableOrderPlaced
  }

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

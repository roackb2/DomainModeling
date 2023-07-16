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

  [<Measure>]
  type kg
  // Order Quantity related
  type KilogramQuantity = KilogramQuantity of decimal<kg>
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

  type ValidatedOrderLine = ValidatedOrderLine of OrderLine
  type BaseOrder = {
    Id: OrderId // id for entity
    CustomerId: CustomerId // customer reference
    ShppingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: NonEmptyList<OrderLine>
    AmountToBill: Payments.PaymentAmount
  }
  type UnvalidatedOrder = UnvalidatedOrder of BaseOrder
  type UnplacedOrder = UnplacedOrder of BaseOrder
  type ValidatedOrder = ValidatedOrder of BaseOrder
  type PlacedOrder = PlacedOrder of BaseOrder
  type PricedOrder = PricedOrder of BaseOrder
  type Order =
    | UnvalidatedOrder
    | UnplacedOrder
    | PlacedOrder
    | PricedOrder

  type EnvelopeContents = EnvelopeContents of string
  type QuoteForm = Undefined
  type OrderForm = Undefined
  type CategorizedMail =
    | Quote of QuoteForm
    | Order of OrderForm


  type UnpaidInvoice = Undefined
  type PaidInvoice = Undefined
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
  let changeOrdlinePrice order orderLineId newPrice =
    let orderLine = order.OrderLines.find (fun ol -> ol.Id = orderLineId)
    let newOrderLine = { orderLine with Price = newPrice }
    let newOrderLines = order.OrderLines.map (fun ol -> if ol.Id = orderLineId then newOrderLine else ol)
    let prices = newOrderLines.map (fun ol -> match ol.Price.Amount with Payments.PaymentAmount a -> a)
    let newAmountToBill = prices.toList |> List.sum |> Payments.PaymentAmount
    let newOrder = {
      order with
        OrderLines = newOrderLines
        AmountToBill = newAmountToBill
      }
    newOrder
  module Workflow =
  // the command
    type PlaceOrderCmd = Command<UnvalidatedOrder>
    type ChangeOrderCmd = Command<ValidatedOrder>
    type CancelOrderCmd = Command<ValidatedOrder>

    type OrderTakingCommmand =
      | Place of PlaceOrderCmd
      | Change of ChangeOrderCmd
      | Cancel of CancelOrderCmd


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

namespace OrderTaking.Workflows

#load "../Common/Types.fsx"
#load "BaseTypes.fsx"
#load "Api.fsx"
open Common.Types
open OrderTaking.BaseType.Payments
open OrderTaking.BaseType.Orders
open OrderTaking.BaseType.Uncategorized
open OrderTaking.Api.InputData
open OrderTaking.Api.OutputData
open OrderTaking.Api.Errors

module Workflows =
  type CheckProductCodeExists = ProductCode -> bool
  // AsyncResult indicates that this service has both async and result side effects
  type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>
  type ValidateOrder =
    CheckProductCodeExists -> // dependency
      CheckAddressExists -> // dependency
      UnvalidatedOrder -> // input
      AsyncResult<ValidatedOrder, ValidationError> // output
  // Pricing validation
  type GetProductPrice = ProductCode -> Price
  type PriceOrder =
    GetProductPrice -> // dependency
      ValidatedOrder -> // input
      Result<PricedOrder, PricingError>  // result indicating that there might be pricing error cause pricing is an error-prone process
  type CreateOrderAcknowledgementLetter = PricedOrder -> HtemlString
  // Async indicating that sending acknowlecgement is doing a network IO, so there might be error, but we don't care about the actual error
  type SendOrderAcknowledgement = CreateOrderAcknowledgementLetter -> Async<SentResult>
  type AcknowledgeOrder =
    CreateOrderAcknowledgementLetter -> // dependency
      SendOrderAcknowledgement -> // dependency
      PricedOrder -> // input
      Async<Events.OrderAcknowledgementSent option> // output

module Uncategorized =
  // Processes
  type PlaceOrder = UnvalidatedOrder -> Result<Events.PlaceOrderEvent, PlaceOrderError>
  type CalculatePrices = CalculatePricesInput -> PricedOrder
  type SaveCustomer = CustomerInfo -> unit
  type PayInvoice = UnpaidInvoice -> Payment -> PaidInvoice
  type ConvertPaymentCurrency = Payment -> Currency -> Payment
  let changeOrdlinePrice order orderLineId newPrice =
    let orderLine = order.OrderLines.find (fun ol -> ol.Id = orderLineId)
    let newOrderLine = { orderLine with Price = newPrice }
    let newOrderLines = order.OrderLines.map (fun ol -> if ol.Id = orderLineId then newOrderLine else ol)
    let prices = newOrderLines.map (fun ol -> match ol.Price.Amount with PaymentAmount a -> a)
    let newAmountToBill = prices.toList |> List.sum |> PaymentAmount
    let newOrder = {
      order with
        OrderLines = newOrderLines
        AmountToBill = newAmountToBill
      }
    newOrder

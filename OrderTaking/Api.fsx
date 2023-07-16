namespace OrderTaking.Api

#load "../Common/Types.fsx"
#load "BaseTypes.fsx"

open Common.Types
open OrderTaking.BaseType.Payments
open OrderTaking.BaseType.Contacts
open OrderTaking.BaseType.Orders

module InputData =
  type UnvalidatedAddress = UnvalidatedAddress of BaseAddress
  type UnvalidatedOrder = UnvalidatedOrder of BaseOrder
  type UnplacedOrder = UnplacedOrder of BaseOrder

module OutputData =
  type CheckedAddress = CheckedAddress of BaseAddress
  type ValidatedOrderLine = ValidatedOrderLine of OrderLine
  type ValidatedOrder = ValidatedOrder of BaseOrder
  type PlacedOrder = PlacedOrder of BaseOrder
  type AcknowledgementSent = AcknowledgementSent of bool
  type OrderPlaced = PricedOrder
  type BillableOrderPlaced = {
    OrderId: OrderId
    BillingAddress: BillingAddress
    AmountToBill: BillingAmount
  }
  module Events =
    type OrderAcknowledgementSent = {
      OrderId: OrderId
      EmailAddress: EmailAddress
    }
    type PlaceOrderEvent =
      | OrderPlaced of OrderPlaced
      | BillableOrderPlaced of BillableOrderPlaced
      | OrderAcknowledgementSent of OrderAcknowledgementSent
    type CreateEvents = PricedOrder -> PlaceOrderEvent list

module Errors =
  type AddressValidationError = AddressValidationError of string
  type PricingError = PricingError of string
  type PlaceOrderError =
    | ValidationError of ValidationError list

module Commands =
  type PlaceOrderCmd = Command<InputData.UnvalidatedOrder>
  type ChangeOrderCmd = Command<OutputData.ValidatedOrder>
  type CancelOrderCmd = Command<OutputData.ValidatedOrder>
  type OrderTakingCommmand =
    | Place of PlaceOrderCmd
    | Change of ChangeOrderCmd
    | Cancel of CancelOrderCmd

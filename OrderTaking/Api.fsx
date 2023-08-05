namespace OrderTaking.Api

#load "../Common/Types.fsx"
#load "BaseTypes.fsx"

open Common.Types
open OrderTaking.BaseTypes.Payments
open OrderTaking.BaseTypes.Contacts
open OrderTaking.BaseTypes.Orders

module InputData =
  type UnvalidatedOrder = {
    OrderId: OrderId // id for entity
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    BillingAddress: UnvalidatedAddress
    OrderLines: NonEmptyList<UnvalidatedOrderLine>
    AmountToBill: PaymentAmount
  }
  type UnplacedOrder = UnvalidatedOrder
  type Order =
    | UnvalidatedOrder
    | UnplacedOrder
    | PlacedOrder
    | PricedOrder

module OutputData =
  type ValidatedOrder = {
    OrderId: OrderId // id for entity
    CustomerInfo: CustomerInfo
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: NonEmptyList<ValidatedOrderLine>
    AmountToBill: PaymentAmount
  }
  type PricedOrder = {
    OrderId: OrderId // id for entity
    CustomerInfo: CustomerInfo
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: NonEmptyList<PricedOrderLine>
    AmountToBill: PaymentAmount
  }
  type PlacedOrder = ValidatedOrder
  type AcknowledgementSent = AcknowledgementSent of bool
  module PlaceOrderEvents =
    type OrderPlaced = PricedOrder
    type BillableOrderPlaced = {
      OrderId: OrderId
      BillingAddress: BillingAddress
      AmountToBill: BillingAmount
    }
    type OrderAcknowledgementSent = {
      OrderId: OrderId
      EmailAddress: EmailAddress
    }
    type PlaceOrderEvent =
      | OrderPlaced of OrderPlaced
      | BillableOrderPlaced of BillableOrderPlaced
      | AcknowledgementSent of OrderAcknowledgementSent
    type CreateEvents =
      PricedOrder
        -> OrderAcknowledgementSent option
        -> PlaceOrderEvent list

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

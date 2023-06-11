module rec OrderTaking

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
  type ProductCode = ProductCode of string
  type OrderQuantity =
    | UnitQuantity of int
    | KilogramQuantity of decimal
  type OrderId = OrderId of string
  type OrderLine = OrderLine of string
  type Order = {
    OrderId: OrderId
    Lines: OrderLine list
  }
  type Customer = Customer of string
  type UnpaidInvoice = UnpaidInvoice of string
  type PaidInvoice = PaidInvoice of string


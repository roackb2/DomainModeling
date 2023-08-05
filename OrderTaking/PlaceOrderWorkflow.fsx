namespace OrderTaking.PlaceOrderWorkflow

#load "../Common/Types.fsx"
#load "BaseTypes.fsx"

open Common.Types
open OrderTaking.BaseTypes.Payments
open OrderTaking.BaseTypes.Contacts
open OrderTaking.BaseTypes.Orders

module Types =
  module Internal =
    type AcknowledgementSent = AcknowledgementSent of bool

    type OrderAcknowledgementSent = {
      OrderId: OrderId
      EmailAddress: EmailAddress
    }

    type UnvalidatedOrder = {
      OrderId: OrderId // id for entity
      CustomerInfo: UnvalidatedCustomerInfo
      ShippingAddress: UnvalidatedAddress
      BillingAddress: UnvalidatedAddress
      OrderLines: NonEmptyList<UnvalidatedOrderLine>
      AmountToBill: PaymentAmount
    }

  module Errors =
    type AddressValidationError = AddressValidationError of string
    type PricingError = PricingError of ValidationError
    type PlaceOrderError =
      | ValidationError of ValidationError list

  module Validate =
    open Internal
    open Errors

    type CheckProductCodeExists = ProductCode -> bool
    type CheckedAddress = {
      AddressLine1: String50
      AddressLine2: Option<String50>
      City: String50
      State: String50
      Country: String50
      PostalCode: String50
    }
    type ShippingAddress = ShippingAddress of CheckedAddress
    type BillingAddress = BillingAddress of CheckedAddress
    type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>
    type ValidatedOrderLine = {
      OrderLineId: OrderLineId // id for entity
      OrderId: OrderId
      ProductCode: ProductCode
      Quantity: OrderQuantity
      Price: Price
    }
    with
    member this.Key =
      (this.OrderId, this.ProductCode)
    end

    type PricedOrder = {
      OrderId: OrderId // id for entity
      CustomerInfo: CustomerInfo
      ShippingAddress: ShippingAddress
      BillingAddress: BillingAddress
      OrderLines: NonEmptyList<PricedOrderLine>
      AmountToBill: PaymentAmount
    }

    type ValidatedOrder = {
      OrderId: OrderId // id for entity
      CustomerInfo: CustomerInfo
      ShippingAddress: ShippingAddress
      BillingAddress: BillingAddress
      OrderLines: NonEmptyList<ValidatedOrderLine>
      AmountToBill: PaymentAmount
    }

    type ValidateOrder =
      CheckProductCodeExists -> // dependency
        CheckAddressExists -> // dependency
        UnvalidatedOrder -> // input
        ValidatedOrder // output

  module Pricing =
    open Validate

    type GetProductPrice =
      ProductCode -> Price

    type PriceOrder =
      GetProductPrice -> // dependency
        Validate.ValidatedOrder -> // input
        PricedOrder // result indicating that there might be pricing error cause pricing is an error-prone process

  module Acknowledgement =
    open Internal
    open Validate

    type HtemlString = HtemlString of string

    type OrderAcknowledgement = {
      EmailAddress: EmailAddress
      Letter: HtemlString
    }

    type CreateOrderAcknowledgementLetter = PricedOrder -> HtemlString

    type SendResult = Sent | NotSent

    type SendOrderAcknowledgement = OrderAcknowledgement -> Async<SendResult>

    type AcknowledgeOrder =
      CreateOrderAcknowledgementLetter -> // dependency
        SendOrderAcknowledgement -> // dependency
        PricedOrder -> // input
        OrderAcknowledgementSent option // output

  module Events =
    open Internal
    open Validate

    type OrderPlaced = PricedOrder

    type BillableOrderPlaced = {
      OrderId: OrderId
      BillingAddress: BillingAddress
      AmountToBill: BillingAmount
    }

    type PlaceOrderEvent =
      | OrderPlaced of OrderPlaced
      | BillableOrderPlaced of BillableOrderPlaced
      | AcknowledgementSent of OrderAcknowledgementSent

    type CreateEvents =
      PricedOrder
        -> OrderAcknowledgementSent option
        -> PlaceOrderEvent list

  type PlaceOrder = Internal.UnvalidatedOrder -> Result<Events.PlaceOrderEvent list, Errors.PlaceOrderError>

module Implementation =
  module Validate =
    open Types.Validate
    let toCustomerInfo (customer: UnvalidatedCustomerInfo): CustomerInfo =
      let firstName = customer.FirstName |> String50.create
      let lastName = customer.LastName |> String50.create
      let email = customer.EmailAddress |> EmailAddress.value |> EmailAddress.create

      let personalName = {
        FirstName = firstName
        MiddleInitial = None
        LastName = lastName
      }
      {
        PersonalName = personalName
        EmailAddress = email
      }
    let toCheckedAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress: CheckedAddress =
      let checkedAddress = checkAddressExists unvalidatedAddress |> Async.RunSynchronously
      let address =
        match checkedAddress with
          | Ok (addr) -> addr
          | Error error -> failwith "address validation error"
      let addressLine2 =
        match address.AddressLine2 with
          | Some addressLine2 -> Some addressLine2
          | None -> None
      {
        AddressLine1 = address.AddressLine1 |> String50.value |> String50.create
        AddressLine2 = addressLine2
        City = address.City |> String50.value |> String50.create
        State = address.State |> String50.value |> String50.create
        Country = address.Country |> String50.value |> String50.create
        PostalCode = address.PostalCode |> String50.value |> String50.create
      }
    let toOrderQuantity productCode quantity =
      match productCode with
        | Widget _ ->
          quantity
          |> int
          |> UnitQuantity.CreateUnitQuantity
          |> Unit
        | Gizmo _ ->
          quantity
          |> KilogramQuantity.CreateKilogramQuantity
          |> Kilogram
    let predicateToPassthru errMsg f x =
      if f x then
        x
      else
        failwith errMsg
    let toProductCode (checkProductCodeExists: CheckProductCodeExists) productCode =
      let errMsg = sprintf "Invalid product code: %s" productCode
      let checkProduct = predicateToPassthru errMsg checkProductCodeExists
      productCode
        |> ProductCode.create
        |> checkProduct

    let toValidatedOrderLine checkProductCodeExists (unvalidatedOrderLine: UnvalidatedOrderLine) =
      let orderLineId = unvalidatedOrderLine.OrderLineId |> OrderLineId.value |> OrderLineId.create
      let productCode = unvalidatedOrderLine.ProductCode |> ProductCode.value |> toProductCode checkProductCodeExists
      let quantity = unvalidatedOrderLine.Quantity |> toOrderQuantity productCode
      let validatedOrderLine: ValidatedOrderLine = {
        OrderLineId = orderLineId
        OrderId = unvalidatedOrderLine.OrderId
        ProductCode = productCode
        Quantity = quantity
        Price = unvalidatedOrderLine.Price
      }
      validatedOrderLine

    let validateOrder: ValidateOrder =
      fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
        let orderId =
          OrderId.value unvalidatedOrder.OrderId
          |> OrderId.create
        let customerInfo =
          unvalidatedOrder.CustomerInfo
          |> toCustomerInfo
        let shippingAddress =
          unvalidatedOrder.ShippingAddress
          |> toCheckedAddress checkAddressExists
          |> ShippingAddress
        let billingAddress =
          unvalidatedOrder.BillingAddress
          |> toCheckedAddress checkAddressExists
          |> BillingAddress
        let orderLines =
          unvalidatedOrder.OrderLines.map(toValidatedOrderLine checkProductCodeExists)
        let amountToBill =
          unvalidatedOrder.AmountToBill
        let validatedOrder: ValidatedOrder = {
          OrderId = orderId
          CustomerInfo = customerInfo
          ShippingAddress = shippingAddress
          BillingAddress = billingAddress
          OrderLines = orderLines
          AmountToBill = amountToBill
        }
        validatedOrder

  module Pricing =
    open Types.Validate
    open Types.Pricing
    let getProductPrice (productCode: ProductCode): Price =
      failwith "not implemented"
    let toPricedOrderLine getProductPrice (line: ValidatedOrderLine): PricedOrderLine =
      let qty: OrderQuantity = line.Quantity
      let price: Price = getProductPrice line.ProductCode
      let linePrice =
        match qty with
          | Unit q -> decimal (UnitQuantity.value q) |> Price.multiply price
          | Kilogram (q: KilogramQuantity) -> decimal (KilogramQuantity.value q) |> Price.multiply price
      {
        OrderLineId = line.OrderLineId
        OrderId = line.OrderId
        ProductCode = line.ProductCode
        Quantity = line.Quantity
        Price = linePrice
      }
    let priceOrder: PriceOrder =
      fun getProductPrice validatedOrder ->
        let lines =
          validatedOrder.OrderLines.map (toPricedOrderLine getProductPrice)
        let amountToBill =
          lines.map (fun line -> line.Price)
          |> BillingAmount.sumPrices
        let pricedOrder: PricedOrder = {
          OrderId = validatedOrder.OrderId
          CustomerInfo = validatedOrder.CustomerInfo
          ShippingAddress = validatedOrder.ShippingAddress
          BillingAddress = validatedOrder.BillingAddress
          OrderLines = lines
          AmountToBill = amountToBill.Amount
        }
        pricedOrder

  module Acknowledgement =
    open Types.Internal
    open Types.Validate
    open Types.Acknowledgement

    let acknowledgeOrder: AcknowledgeOrder =
      fun createAcknowledgementLetter sendAcknowledgement pricedOrder ->
        let letter = createAcknowledgementLetter pricedOrder
        let acknowledgement = {
          EmailAddress = pricedOrder.CustomerInfo.EmailAddress
          Letter = letter
        }
        let res = Async.RunSynchronously (sendAcknowledgement acknowledgement)
        match res with
          | Sent ->
            let acknowledgementSent: OrderAcknowledgementSent = {
              OrderId = pricedOrder.OrderId
              EmailAddress = pricedOrder.CustomerInfo.EmailAddress
            }
            Some acknowledgementSent
          | NotSent ->
            None

  module Events =
    open Types
    open Types.Validate
    open Types.Events
    let createBillingEvents (placedOrder: PricedOrder): BillableOrderPlaced option =
      let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
      if billingAmount > 0m then
        let order: BillableOrderPlaced = {
          OrderId = placedOrder.OrderId
          BillingAddress = placedOrder.BillingAddress
          AmountToBill = placedOrder.AmountToBill
        }
        Some order
      else
        None
    let createEvents: CreateEvents =
      let listOfOption opt =
        match opt with
          | Some x -> [x]
          | None -> []
      fun pricedOrder acknowledgementSentOpt ->
        let events1 =
          pricedOrder
          |> OrderPlaced
          |> List.singleton
        let events2 =
          acknowledgementSentOpt
          |> Option.map AcknowledgementSent
          |> listOfOption
        let events3 =
          pricedOrder
          |> createBillingEvents
          |> Option.map BillableOrderPlaced
          |> listOfOption
        [
          yield! events1
          yield! events2
          yield! events3
        ]
    let placeOrder
      checkProductCodeExists
      checkAddressExists
      getProductPrice
      createOrderAcknowledgementLetter
      sendOrderAcknowledgement
      : PlaceOrder =
        fun unvalidatedOrder ->
          let validatedOrder =
            unvalidatedOrder
            |> Validate.validateOrder checkProductCodeExists checkAddressExists
          let pricedOrder =
            validatedOrder
            |> Pricing.priceOrder getProductPrice
          let acknowledgementOption =
            pricedOrder
            |> Acknowledgement.acknowledgeOrder createOrderAcknowledgementLetter sendOrderAcknowledgement
          let events =
            createEvents pricedOrder acknowledgementOption
          Ok events

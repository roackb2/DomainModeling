namespace OrderTaking.Workflows

#load "../Common/Types.fsx"
#load "BaseTypes.fsx"
#load "Api.fsx"
open Common.Types
open OrderTaking.BaseTypes.Payments
open OrderTaking.BaseTypes.Contacts
open OrderTaking.BaseTypes.Orders
open OrderTaking.BaseTypes.Uncategorized
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
  type GetProductPrice = ProductCode -> Price
  type PriceOrder =
    GetProductPrice -> // dependency
      ValidatedOrder -> // input
      Result<PricedOrder, PricingError>  // result indicating that there might be pricing error cause pricing is an error-prone process
  type CreateOrderAcknowledgementLetter = PricedOrder -> HtemlString
  // Async indicating that sending acknowlecgement is doing a network IO, so there might be error, but we don't care about the actual error
  type SendOrderAcknowledgement = OrderAcknowledgement -> Async<SentResult>
  type AcknowledgeOrder =
    CreateOrderAcknowledgementLetter -> // dependency
      SendOrderAcknowledgement -> // dependency
      PricedOrder -> // input
      Option<Events.OrderAcknowledgementSent> // output
  type PlaceOrder = UnvalidatedOrder -> Result<Events.PlaceOrderEvent, PlaceOrderError>
  module Validate =
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
    let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress =
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

    let valdiateOrder: ValidateOrder =
      fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
        let orderId =
          OrderId.value unvalidatedOrder.OrderId
          |> OrderId.create
        let customerInfo =
          unvalidatedOrder.CustomerInfo
          |> toCustomerInfo
        let shippingAddress =
          unvalidatedOrder.ShippingAddress
          |> toAddress checkAddressExists
          |> ShippingAddress
        let billingAddress =
          unvalidatedOrder.BillingAddress
          |> toAddress checkAddressExists
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
        AsyncResult.resolve (Ok validatedOrder)
  module Pricing =
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
        Ok pricedOrder
  module Acknowledgement =
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
            let acknowledgementSent: Events.OrderAcknowledgementSent = {
              OrderId = pricedOrder.OrderId
              EmailAddress = pricedOrder.CustomerInfo.EmailAddress
            }
            Some acknowledgementSent
          | NotSent ->
            None
  // let placeOrder unvalidatedOrder =
  //   unvalidatedOrder
  //   |> validateOrder
  //   |> priceOrder
  //   |> acknowledgeOrder
  //   |> createEvents

module Uncategorized =
  // Processes
  type CalculatePrices = CalculatePricesInput -> PricedOrder
  type SaveCustomer = CustomerInfo -> unit
  type PayInvoice = UnpaidInvoice -> Payment -> PaidInvoice
  type ConvertPaymentCurrency = Payment -> Currency -> Payment
  let changeOrdlinePrice order orderLineId newPrice =
    let orderLine = order.OrderLines.find (fun ol -> ol.OrderLineId = orderLineId)
    let newOrderLine = { orderLine with Price = newPrice }
    let newOrderLines = order.OrderLines.map (fun ol -> if ol.OrderLineId = orderLineId then newOrderLine else ol)
    let prices = newOrderLines.map (fun ol -> match ol.Price.Amount with PaymentAmount a -> a)
    let newAmountToBill = prices.toList |> List.sum |> PaymentAmount
    let newOrder = {
      order with
        OrderLines = newOrderLines
        AmountToBill = newAmountToBill
      }
    newOrder

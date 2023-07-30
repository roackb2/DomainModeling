namespace Common.Types

open System

type Undefined = exn
type Option<'a> =
  | Some of 'a
  | None
type Result<'Success, 'Failure> =
  | Ok of 'Success
  | Error of 'Failure
type AsyncResult<'Success, 'Failure> = Async<Result<'Success, 'Failure>>
module AsyncResult =
  let resolve res = async { return res }
type String50 = private String50 of string
module String50 =
  let create str =
    if String.IsNullOrEmpty(str) then
      failwith "String50 cannot be empty"
    else if str.Length > 50 then
      failwith "String50 cannot be longer than 50 characters"
    else
      String50 str
  let createOption str =
    if String.IsNullOrEmpty(str) then
      None
    else
      Some (String50 str)
  let value (String50 str) = str
type PersonalName = {
  FirstName: String50
  MiddleInitial: Option<String50>
  LastName: String50
}
type ValidationError = {
  FieldName: string
  ErrorDescription: string
}
type ValidationResponse<'a> = Async<Result<'a, ValidationError list>>

type NonEmptyList<'a> = {
  First: 'a
  Rest: 'a list
} with
  member this.find predicate =
    match this with
      | { First = first; Rest = rest } -> if predicate(first) then first else this.find predicate
  member this.map mapper =
    match this with
      | { First = first; Rest = rest } -> { First = mapper(first); Rest = List.map mapper rest }
  member this.sumBy mapper =
    match this with
      | { First = first; Rest = rest } -> mapper(first) * 1.0m + List.sumBy mapper rest
  member this.toList =
    match this with
      | { First = first; Rest = rest } -> first :: rest
  end

type Command<'a> = {
  Data: 'a
  Timestamp: System.DateTime
  UserId: string
}

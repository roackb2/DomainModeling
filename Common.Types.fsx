module Common

type Undefined = exn
type Option<'a> =
  | Some of 'a
  | None
type Result<'Success, 'Failure> =
  | Ok of 'Success
  | Error of 'Failure
type AsyncResult<'Success, 'Failure> = Async<Result<'Success, 'Failure>>
type PersonalName = {
  FirstName: string
  MiddleInitial: Option<string>
  LastName: string
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
      | { First = first; Rest = rest } -> mapper(first) + List.sumBy mapper rest
  member this.toList =
    match this with
      | { First = first; Rest = rest } -> first :: rest
  end

type Command<'a> = {
  Data: 'a
  Timestamp: System.DateTime
  UserId: string
}

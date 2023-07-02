module Common

type Undefined = exn
type Option<'a> =
  | Some of 'a
  | None
type Result<'Success, 'Failure> =
  | Ok of 'Success
  | Error of 'Failure
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
type NonEmptyList<'a>  = {
  First: 'a
  Rest: 'a list
}

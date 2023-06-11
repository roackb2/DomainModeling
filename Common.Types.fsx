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

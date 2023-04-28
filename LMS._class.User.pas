unit LMS._class.User;

interface

uses
  System.JSON,
  Generics.Collections,

  LMS._interface.LMS;

type
  TUser = class(TInterfacedObject, IUser)
  strict private
    fCourse: ICourse;
    fEmail: string;
    fFirstName: string;
    fFullName: string;
    fid: integer;
    flastcourseaccess: TDateTime;
    fLastName: string;
    fLMS: ILMS;
    fOtherEnrolledCourses: TList<ICourse>;
    fRoles: string;
    fUserName: string;
    function GetCourse: ICourse;
    function GetEmail: string;
    function getFilterContent: string;
    function GetFirstName: string;
    function GetFullName: string;
    function GetId: integer;
    function GetLastAccessAsString: string;
    function GetLastAccessFromAsString: string;
    function GetLastName: string;
    function getLMS: ILMS;
    function GetOtherEnrolledCourses: TList<ICourse>;
    function GetRoles: string;
    function GetUserName: string;
    procedure SetCourse(const value: ICourse);
    procedure SetRoles(const value: string);
  public
    constructor Create(const LMS: ILMS; const aJSONValue: TJSONValue);
    property Course: ICourse read GetCourse write SetCourse;
    property Email: string read GetEmail;
    property FilterContent: string read getFilterContent;
    property First_Name: string read GetFirstName;
    property Full_Name: string read GetFullName;
    property Id: integer read GetId;
    property Last_access: string read GetLastAccessAsString;
    property Last_access_from: string read GetLastAccessFromAsString;
    property Last_Name: string read GetLastName;
    // procedure AssignByJson(const aJsonValue: TJSONValue);

    // Properties for view components, do not resource
    property LMS: ILMS read getLMS;
    property OtherEnrolledCourses: TList<ICourse> read GetOtherEnrolledCourses;
    property Roles: string read GetRoles write SetRoles;
    property UserName: string read GetUserName;
  end;

implementation

uses
  DateUtils, sysutils,
  LMS.Helper.Utils,
  LMS.Helper.Log;

constructor TUser.Create(const LMS: ILMS; const aJSONValue: TJSONValue);
var
  timestamp: Int64;
  enrolledcourses: TJSONArray;
  aOtherCourse: ICourse;
begin
  fOtherEnrolledCourses := TList<ICourse>.Create;

  fLMS := LMS;
  fid := aJSONValue.GetValue<cardinal>('id');
  fUserName := aJSONValue.GetValue<string>('username');
  fFirstName := aJSONValue.GetValue<string>('firstname');
  fLastName := aJSONValue.GetValue<string>('lastname');
  fFullName := aJSONValue.GetValue<string>('fullname');
  fEmail := aJSONValue.GetValue<string>('email');

  // Some rest query does not return some fields so we have to check
  if aJSONValue.TryGetValue<Int64>('lastcourseaccess', timestamp) then
    flastcourseaccess := unixtodatetime(timestamp);

  if aJSONValue.TryGetValue<TJSONArray>('enrolledcourses', enrolledcourses) then
    for var acourse in enrolledcourses do
    begin
      aOtherCourse := fLMS.getCourseById(acourse.GetValue<integer>('id'));
      OtherEnrolledCourses.add(aOtherCourse);
    end;
end;

function TUser.GetCourse: ICourse;
begin
  result := fCourse;
end;

function TUser.GetEmail: string;
begin
  result := fEmail;
end;

function TUser.getFilterContent: string;
begin
  result := fEmail + ' ' + fFullName;
end;

function TUser.GetFirstName: string;
begin
  result := fFirstName;
end;

function TUser.GetFullName: string;
begin
  result := fFullName;
end;

function TUser.GetId: integer;
begin
  result := fid;
end;

function TUser.GetLastAccessAsString: string;
begin
  result := FormatDateTimeNever(flastcourseaccess);
end;

function TUser.GetLastAccessFromAsString: string;
const
  DaysPerWeek = 7;
  DaysPerMonth = 30;
var
  DifInDays: integer;
begin
  if datetimetounix(flastcourseaccess) = 0 then
    result := 'never'
  else
  begin
    DifInDays := Round(Now - flastcourseaccess);

    if DifInDays < DaysPerWeek then
    begin
      if DifInDays = 1 then
        result := 'a day ago'
      else if DifInDays > 1 then
        result := IntToStr(DifInDays) + ' days ago'
      else
        result := 'today';
    end
    else if DifInDays < DaysPerMonth then
    begin
      if DifInDays div DaysPerWeek = 1 then
        result := 'a week ago'
      else
        result := Format('%d weeks ago', [DifInDays div DaysPerWeek]);
    end
    else
    begin
      if DifInDays div DaysPerMonth = 1 then
        result := 'a month ago'
      else
        result := Format('%d months ago', [DifInDays div DaysPerMonth]);
    end;
  end;

end;

function TUser.GetLastName: string;
begin
  result := fLastName;
end;

function TUser.getLMS: ILMS;
begin
  result := fLMS;
end;

function TUser.GetOtherEnrolledCourses: TList<ICourse>;
begin
  result := fOtherEnrolledCourses;
end;

function TUser.GetRoles: string;
begin
  result := fRoles;
end;

function TUser.GetUserName: string;
begin
  result := fUserName;
end;

procedure TUser.SetCourse(const value: ICourse);
begin
  fCourse := value;
end;

procedure TUser.SetRoles(const value: string);
begin
  fRoles := value;
end;

end.

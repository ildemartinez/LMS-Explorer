unit LMS._class.User;

interface

uses
  System.JSON,
  Generics.Collections,

  LMS._interface.LMS;

type
  TUser = class(TInterfacedObject, IUser)
  private
    fLMS: ILMS;
    fid: integer;
    fUserName: string;
    flastcourseaccess: TDateTime;
    fCourse: ICourse;
    fEmail: string;
    fFullName: string;
    fFirstName: string;
    fLastName: string;
    fRoles: string;
    fOtherEnrolledCourses: TList<ICourse>;
    function getFilterContent: string;
    function GetLastAccessAsString: string;
    function GetRoles: string;
    function GetEmail: string;
    function GetFirstName: string;
    function GetFullName: string;
    function GetLastName: string;
    function GetCourse: ICourse;
    function GetId: integer;
    function GetUserName: string;
    procedure SetCourse(const value: ICourse);
    procedure SetRoles(const value: string);
    function getLMS: ILMS;
    function GetOtherEnrolledCourses: TList<ICourse>;
  public
    constructor Create(const LMS: ILMS; const aJSONValue: TJSONValue);
    // procedure AssignByJson(const aJsonValue: TJSONValue);

    // Properties for view components, do not resource
    property LMS: ILMS read getLMS;
    property Id: integer read GetId;
    property UserName: string read GetUserName;
    property Full_Name: string read GetFullName;
    property First_Name: string read GetFirstName;
    property Last_Name: string read GetLastName;
    property Email: string read GetEmail;
    property Last_access: string read GetLastAccessAsString;
    property Roles: string read GetRoles write SetRoles;
    property Course: ICourse read GetCourse write SetCourse;

    property FilterContent: string read getFilterContent;
    property OtherEnrolledCourses: TList<ICourse> read GetOtherEnrolledCourses;
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

procedure TUser.SetRoles(const value: string);
begin
  fRoles := value;
end;

function TUser.GetOtherEnrolledCourses: TList<ICourse>;
begin
  result := fOtherEnrolledCourses;
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

procedure TUser.SetCourse(const value: ICourse);
begin
  fCourse := value;
end;

function TUser.GetId: integer;
begin
  result := fid;
end;

function TUser.GetLastAccessAsString: string;
begin
  result := FormatDateTimeNever(flastcourseaccess);
end;

function TUser.GetLastName: string;
begin
  result := fLastName;
end;

function TUser.getLMS: ILMS;
begin
  result := fLMS;
end;

function TUser.GetRoles: string;
begin
  result := fRoles;
end;

function TUser.GetUserName: string;
begin
  result := fUserName;
end;

end.

unit LMS._class.User;

interface

uses
  System.JSON,
  LMS._interface.LMS;

type
  TUser = class(TInterfacedObject, IUser)
  private
    fid: integer;
    fUserName: string;
    flastcourseaccess: TDateTime;
    fCourse: ICourse;
    fEmail: string;
    fFullName: string;
    fFirstName: string;
    fLastName: string;
    fRoles: string;
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
  public

    procedure AssignByJson(const aJsonValue: TJSONValue);

    // Properties for view components, do not resource
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
  end;

implementation

uses
  DateUtils,
  LMS.Helper.Utils;

procedure TUser.AssignByJson(const aJsonValue: TJSONValue);
var
  timestamp: Int64;
begin
  fid := aJsonValue.GetValue<cardinal>('id');
  fUserName := aJsonValue.GetValue<string>('username');
  fFirstName := aJsonValue.GetValue<string>('firstname');
  fLastName := aJsonValue.GetValue<string>('lastname');
  fFullName := aJsonValue.GetValue<string>('fullname');
  fEmail := aJsonValue.GetValue<string>('email');

  // Some rest query does not return some fields so we have to check
  if aJsonValue.TryGetValue<Int64>('lastcourseaccess', timestamp) then
    flastcourseaccess := unixtodatetime(timestamp);
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

function TUser.GetRoles: string;
begin
  result := fRoles;
end;

function TUser.GetUserName: string;
begin
  result := fUserName;
end;

end.

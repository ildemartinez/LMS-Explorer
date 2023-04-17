unit LMS._class.UsersGroup;

interface

uses
  System.Classes,
  Generics.Collections,

  LMS._interface.LMS;

type
  TUsersGroup = class(TInterfacedObject, IUsersGroup)
  private
    fid: cardinal;
    fGroupName: string;
    // Users in this group
    fUsersInGroup: TList<IUser>;

    function getId: cardinal;
    function getFilterContent: string;
    function GetUsersInGroup: TList<IUser>;
    procedure SetUsersInGroup(const Value: TList<IUser>);
    procedure SetId(const Value: cardinal);
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
  public
    constructor Create;

    property Id: cardinal read getId write SetId;
    property Group_Name: string read GetGroupName write SetGroupName;
    property FilterContent: string read getFilterContent;
    property UsersInGroup: TList<IUser> read GetUsersInGroup
      write SetUsersInGroup;
  end;

implementation

uses
  LMS._class.User;

constructor TUsersGroup.Create;
begin
  fUsersInGroup := TList<IUser>.Create;
end;

function TUsersGroup.getFilterContent: string;
begin
  result := fGroupName;
end;

function TUsersGroup.GetGroupName: string;
begin
  result := fGroupName;
end;

function TUsersGroup.getId: cardinal;
begin
  result := fid;
end;

function TUsersGroup.GetUsersInGroup: TList<IUser>;
begin
  result := fUsersInGroup;
end;

procedure TUsersGroup.SetGroupName(const Value: string);
begin
  fGroupName := Value;
end;

procedure TUsersGroup.SetId(const Value: cardinal);
begin
  fid := Value;
end;

procedure TUsersGroup.SetUsersInGroup(const Value: TList<IUser>);
begin
  fUsersInGroup := Value;
end;

end.

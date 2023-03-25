unit LMSNetworkUnit;

interface

uses
  System.Classes,
  System.JSON,
  Generics.Collections,
  dialogs,
  sysutils,

  LMS._interface.LMS,
  LMS.Rest.Moodle;

type
  

  TLMSUserGroup = class(TInterfacedObject, IUsersGroup)
  private
    fid: cardinal;
    fGroupName: string;
    // Users in this group
    fUsersInGroup: TLMSUsers;

    function getId: cardinal;
    function getFilterContent: string;
    function GetUsersInGroup: TLMSUsers;
    procedure SetUsersInGroup(const Value: TLMSUsers);
    procedure SetId(const Value: cardinal);
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
  public

    constructor Create;

    property Id: cardinal read getId write SetId;
    property GroupName: string read GetGroupName write SetGroupName;
    property FilterContent: string read getFilterContent;
    property UsersInGroup: TLMSUsers read GetUsersInGroup write SetUsersInGroup;
  end;

  TLMSCourse = class(TInterfacedObject, ICourse)
  private
    fLMS: ILMS;
    fid: cardinal;
    fGroupMode: cardinal;
    fshortname: string;
    fFullName: string;
    fdisplayname: string;

    // All users enrolled in this course
    fUsers: TLMSUsers;
    // All course groups
    fUserGroups: TLMSUserGroups;

    function getFilterContent: string;
    function GetDisplayContent: string;
    function GetLMS: ILMS;

    procedure RefreshUserGroups;
    function GetStudentsCount: integer;
    function getId: cardinal;
    procedure SetId(const Value: cardinal);
    function GetGroupMode: cardinal;
    procedure SetGroupMode(const Value: cardinal);
    function GetUserGroups: TLMSUserGroups;
    function GetUsers: TLMSUsers;
    procedure SetUserGroups(const Value: TLMSUserGroups);
    procedure SetUsers(const Value: TLMSUsers);
    function GetFullName: string;
    function GetShortName: string;
    procedure SetFullName(const Value: string);
    procedure SetShortName(const Value: string);
    function GetDisplayName: string;
    procedure SetDisplayName(const Value: string);
  public
    constructor Create(const LMS: ILMS);

    procedure RefreshEnrolledUsers;
    procedure GetCourseRoles(aCourseRoles: TStringlist);
    function GetUserCountByRol(const aRole: string): cardinal;

    // Pointer to the LMS parent
    property LMS: ILMS read GetLMS;

    property DisplayName : string read GetDisplayName write SetDisplayName;
    property shortname: string read GetShortName write SetShortName;
    property FullName: string read GetFullName write SetFullName;

    property Id: cardinal read getId write SetId;
    // Returns the apropiated text to show information about courses
    property DisplayContent: string read GetDisplayContent;

    // Return the course information that can be filtered from
    property FilterContent: string read getFilterContent;

    Property StudentsCount: integer read GetStudentsCount;

    property GroupMode: cardinal read GetGroupMode write SetGroupMode;

    property Users: TLMSUsers read GetUsers write SetUsers;
    // All course groups
    property UserGroups: TLMSUserGroups read GetUserGroups write SetUserGroups;
  end;

  TLMSCategory = class(TInterfacedObject, ICategory)
  private
    fLMS: ILMS;
    fid: cardinal;
    fParentCategory: cardinal;
    fName: string;

    fcategories: TList<ICategory>;
    fcourses: TList<ICourse>;

    function GetCoursesCount: cardinal;
    function GetSubCategoriesCount: cardinal;
    function GetLMS: ILMS;
    function GetParentCategory: cardinal;
    procedure SetParentCategory(const Value: cardinal);
    function getId: cardinal;
    procedure SetId(const Value: cardinal);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetCategories: TList<ICategory>;
    function GetCourses: TList<ICourse>;
  public
    // categoryid : cardinal;
    constructor Create(const parent: ILMS);

    // Pointer to the LMS parent
    property LMS: ILMS read GetLMS;

    property Id: cardinal read getId write SetId;
    property Name: string read GetName write SetName;
    property ParentCategory: cardinal read GetParentCategory
      write SetParentCategory;
    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
    property Courses: TList<ICourse> read GetCourses;
    property CoursesCount: cardinal read GetCoursesCount;
    property Categories: TList<ICategory> read GetCategories;

  end;

implementation

uses
  DateUtils,

  LMS._class.User,
  LMS.Helper.Utils,
  LMS.Helper.Log;

{ TLMSCategory }

constructor TLMSCategory.Create(const parent: ILMS);
begin
  inherited Create;

  fLMS := parent;

  fcourses := TList<ICourse>.Create;
  fcategories := TList<ICategory>.Create;
end;

function TLMSCategory.GetCategories: TList<ICategory>;
begin
  result := fcategories;
end;

function TLMSCategory.GetCourses: TList<ICourse>;
begin
  result := fcourses;
end;

function TLMSCategory.GetCoursesCount: cardinal;
begin
  result := fcourses.count;
end;

function TLMSCategory.getId: cardinal;
begin
  result := fid;
end;

function TLMSCategory.GetLMS: ILMS;
begin
  result := fLMS;
end;

function TLMSCategory.GetName: string;
begin
  result := fName;
end;

function TLMSCategory.GetParentCategory: cardinal;
begin
  result := fParentCategory;
end;

function TLMSCategory.GetSubCategoriesCount: cardinal;
begin
  result := fcategories.count;
end;

procedure TLMSCategory.SetId(const Value: cardinal);
begin
  fid := Value;
end;

procedure TLMSCategory.SetName(const Value: string);
begin
  fName := Value;
end;

procedure TLMSCategory.SetParentCategory(const Value: cardinal);
begin
  fParentCategory := Value;
end;

{ TLMSCourse }

constructor TLMSCourse.Create(const LMS: ILMS);
begin
  inherited Create;

  fUsers := TLMSUsers.Create;
  fUserGroups := TLMSUserGroups.Create;

  fLMS := LMS;

end;

procedure TLMSCourse.GetCourseRoles(aCourseRoles: TStringlist);
var
  aUser: IUser;
begin
  for aUser in fUsers do
    if aCourseRoles.IndexOf(aUser.Roles) < 0 then
      aCourseRoles.add(aUser.Roles);
end;

function TLMSCourse.GetDisplayContent: string;
begin
  result := shortname + ' - ' + displayname;
end;

function TLMSCourse.GetDisplayName: string;
begin
  result := fdisplayname;
end;

procedure TLMSCourse.RefreshEnrolledUsers;
var
  aUser: IUser;
  aUsers: TJSonArray;
  User: TJSONValue;

  groups: TJSonArray;
  Group: TJSONValue;

  rol: TJSONValue;
  arealrol: string;
begin
  // Populate user groups first
  RefreshUserGroups;

  fUsers.Clear;

  aUsers := fLMS.aLMSConnection.GetEnrolledUsersByCourseId(self.Id);

  if aUsers <> nil then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create;
      aUser.Course := self;
      aUser.AssignByJson(User);

      // Get user roles
      for rol in User.GetValue<TJSonArray>('roles') do
      begin
        arealrol := rol.GetValue<string>('name');
        if arealrol = '' then
          aUser.Roles := aUser.Roles + rol.GetValue<string>('shortname')
        else
          aUser.Roles := aUser.Roles + arealrol;
      end;

      // Add user to users list
      fUsers.add(aUser);

      // Include the user in the corresponding group
      groups := User.GetValue<TJSonArray>('groups');

      if groups <> nil then
      begin
        // log(groups.ToString);
        for Group in groups do
        begin
          for var agroup in self.fUserGroups do
          begin
            if agroup.Id = Group.GetValue<cardinal>('id') then
              agroup.UsersInGroup.add(aUser)
          end;

        end;
      end;
      //
    end;
  end;
end;

procedure TLMSCourse.RefreshUserGroups;
var
  aUserGroups: TJSonArray;
  agroup: TJSONValue;
  aUserGroup: IUsersGroup;
begin
  fUserGroups.Clear;

  aUserGroups := fLMS.aLMSConnection.GetUserGroupsByCourseId(self.Id);

  if aUserGroups <> nil then
  begin
    // log(aUserGroups.ToString);
    for agroup in aUserGroups do
    begin
      aUserGroup := TLMSUserGroup.Create;

      aUserGroup.Id := agroup.GetValue<cardinal>('id');
      aUserGroup.GroupName := agroup.GetValue<string>('name');
      { aUser.fFirstName := User.GetValue<string>('firstname');
        aUser.fLastName := User.GetValue<string>('lastname');
        aUser.fFullName := User.GetValue<string>('fullname');
      }
      fUserGroups.add(aUserGroup)
    end;
  end;

end;

procedure TLMSCourse.SetDisplayName(const Value: string);
begin
fdisplayname := Value;
end;

procedure TLMSCourse.SetFullName(const Value: string);
begin
fFullName := Value;
end;

procedure TLMSCourse.SetGroupMode(const Value: cardinal);
begin
  fGroupMode := Value;
end;

procedure TLMSCourse.SetId(const Value: cardinal);
begin
  fid := Value;
end;

procedure TLMSCourse.SetShortName(const Value: string);
begin
  fshortname := value;
end;

procedure TLMSCourse.SetUserGroups(const Value: TLMSUserGroups);
begin
  fUserGroups := Value;
end;

procedure TLMSCourse.SetUsers(const Value: TLMSUsers);
begin
  fUsers := Value;
end;

function TLMSCourse.getFilterContent: string;
begin
  result := shortname + ' ' + FullName + ' ' + self.displayname
end;

function TLMSCourse.GetFullName: string;
begin
  result := fFullName;
end;

function TLMSCourse.GetGroupMode: cardinal;
begin
  result := fGroupMode;
end;

function TLMSCourse.getId: cardinal;
begin
  result := fid;
end;

function TLMSCourse.GetLMS: ILMS;
begin
  result := fLMS;
end;

function TLMSCourse.GetShortName: string;
begin
  result := fshortname;
end;

function TLMSCourse.GetStudentsCount: integer;
var
  aUser: IUser;
begin
  result := 0;
  for aUser in fUsers do
  begin
    if aUser.Roles = 'student' then
      inc(result);
  end;
end;

function TLMSCourse.GetUserCountByRol(const aRole: string): cardinal;
var
  aUser: IUser;
begin
  result := 0;
  for aUser in fUsers do
  begin
    if aUser.Roles = aRole then
      inc(result);
  end;
end;

function TLMSCourse.GetUserGroups: TLMSUserGroups;
begin
  result := fUserGroups;
end;

function TLMSCourse.GetUsers: TLMSUsers;
begin
  result := fUsers;
end;

{ TLMSUserGroup }

constructor TLMSUserGroup.Create;
begin
  fUsersInGroup := TLMSUsers.Create;
end;

function TLMSUserGroup.getFilterContent: string;
begin
  result := fGroupName;
end;

function TLMSUserGroup.GetGroupName: string;
begin
  result := fGroupName;
end;

function TLMSUserGroup.getId: cardinal;
begin
  result := fid;
end;

function TLMSUserGroup.GetUsersInGroup: TLMSUsers;
begin
  result := fUsersInGroup;
end;

procedure TLMSUserGroup.SetGroupName(const Value: string);
begin
  fGroupName := Value;
end;

procedure TLMSUserGroup.SetId(const Value: cardinal);
begin
  fid := Value;
end;

procedure TLMSUserGroup.SetUsersInGroup(const Value: TLMSUsers);
begin
  fUsersInGroup := Value;
end;


end.

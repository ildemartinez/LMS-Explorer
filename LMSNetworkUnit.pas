unit LMSNetworkUnit;

interface

uses
  System.Classes,
  System.JSON,
  Generics.Collections, dialogs, sysutils,

  LMSRestmoodleunit;

type

  TLMS = class;
  TLMSCategory = class;
  TLMSCourse = class;

  TLMSUser = class
  private
    fEmail: string;
    function getFilterContent: string;
  public
    fCourse: TLMSCourse;

    fid: integer;
    fUserName, fFirstName, fLastName, fFullName, fRoles: string;
    flastcourseaccess: TDateTime;

    procedure AssignByJson(const aJsonValue: TJSONValue);

    property Email: string read fEmail;
    property FilterContent: string read getFilterContent;

  end;

  TLMSUsers = TList<TLMSUser>;

  TLMSUserGroup = class
  private
    fid: cardinal;
    function getId: cardinal;
    function getFilterContent: string;
  public
    fName: string;

    // Users in this group
    fUsersInGroup: TLMSUsers;

    constructor Create;

    property Id: cardinal read getId;
    property FilterContent: string read getFilterContent;
  end;

  TLMSUserGroups = TList<TLMSUserGroup>;

  TLMSCourse = class
  private
    fLMS: TLMS;

    function getFilterContent: string;
    function GetDisplayContent: string;
    function GetLMS: TLMS;

    procedure RefreshUserGroups;
    function GetStudentsCount: integer;
  public
    Id: cardinal;
    shortname: string;
    fullname: string;
    displayname: string;
    groupmode: cardinal;

    // All users enrolled in this course
    fUsers: TLMSUsers;
    // All course groups
    fUserGroups: TLMSUserGroups;

    constructor Create(const LMS: TLMS);

    procedure RefreshEnrolledUsers;
    procedure GetCourseRoles(aCourseRoles: TStringlist);
    function GetUserCountByRol(const aRole: string): cardinal;

    // Pointer to the LMS parent
    property LMS: TLMS read GetLMS;

    // Returns the apropiated text to show information about courses
    property DisplayContent: string read GetDisplayContent;

    // Return the course information that can be filtered from
    property FilterContent: string read getFilterContent;

    Property StudentsCount: integer read GetStudentsCount;

  end;

  TLMSCategory = class
  private
    fLMS: TLMS;

    function GetCoursesCount: cardinal;
    function GetSubCategoriesCount: cardinal;
    function GetLMS: TLMS;
  public
    fcategories: TList<TLMSCategory>;
    fcourses: TList<TLMSCourse>;
    Id: cardinal;
    // categoryid : cardinal;
    name: string;
    fparent: cardinal;

    constructor Create(const parent: TLMS);

    // Pointer to the LMS parent
    property LMS: TLMS read GetLMS;

    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
    property CoursesCount: cardinal read GetCoursesCount;

  end;

  TLMS = class(TComponent)
  private
    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
    function GetHost: string;

    procedure MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
      const aFunctionName: string);

  public
    Id: string;
    autoconnect: boolean;

    // All LMS categories
    categories: TList<TLMSCategory>;
    // All LMS courses
    courses: TList<TLMSCourse>;

    aLMSConnection: TLMSRestMoodle;

    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function connected: boolean;

    function FirstLevelCategoriesCount: cardinal;
    function GetCategoryById(Id: cardinal): TLMSCategory;
    function GetUsersByFirstName(var aLMSUsers: TLMSUsers;
      const aFilter: string): integer;

    procedure GetCategories;
    procedure GetCourses;

    property User: string write SetUser;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property Host: string read GetHost write SetHost;
  end;

  TLMSNetwork = class
  private
    fLMSList: TList<TLMS>;
    function GetLMS(index: integer): TLMS;
  public
    constructor Create;
    destructor Destroy; override;
    procedure add(aLMS: TLMS);
    function count: cardinal;

    property item[index: integer]: TLMS read GetLMS;
  end;

function GetGlobalNetwork: TLMSNetwork;

implementation

uses
  LMSLogUnit, DateUtils;

var
  _GlobalLMSNetWork: TLMSNetwork;

function GetGlobalNetwork: TLMSNetwork;
begin
  if _GlobalLMSNetWork = nil then
    _GlobalLMSNetWork := TLMSNetwork.Create;

  result := _GlobalLMSNetWork
end;

{ TLMSNetwork }

procedure TLMSNetwork.add(aLMS: TLMS);
begin
  fLMSList.add(aLMS);

end;

function TLMSNetwork.count: cardinal;
begin
  result := fLMSList.count;
end;

constructor TLMSNetwork.Create;
begin
  fLMSList := TList<TLMS>.Create;
end;

destructor TLMSNetwork.Destroy;
begin
  fLMSList.free;

  inherited;
end;

function TLMSNetwork.GetLMS(index: integer): TLMS;
begin
  result := fLMSList.items[index];
end;

{ TLMS }

function TLMS.FirstLevelCategoriesCount: cardinal;
begin
  result := 0;

  for var Category in categories do
    if Category.fparent = 0 then
      inc(result);
end;

procedure TLMS.Connect;
begin
  aLMSConnection.Connect;
end;

function TLMS.connected: boolean;
begin
  result := aLMSConnection.connected;
end;

constructor TLMS.Create(Owner: TComponent);
begin
  aLMSConnection := TLMSRestMoodle.Create(self);
  aLMSConnection.OnFunctionNotAdded := MyOnFunctionNotAdded;

  categories := TList<TLMSCategory>.Create;
  courses := TList<TLMSCourse>.Create;
end;

procedure TLMS.GetCategories;
var
  aCategory: TLMSCategory;
  aCategories: TJSonArray;
  Category: TJSONValue;
begin
  log('Retrieving LMS Categories');
  aCategories := aLMSConnection.GetCategories;

  if aCategories <> nil then
  begin
    // log(aCategories.ToString);

    for Category in aCategories do
    begin
      // Add to a global category list
      aCategory := TLMSCategory.Create(self);
      aCategory.Id := Category.GetValue<cardinal>('id');
      aCategory.name := Category.GetValue<string>('name');
      aCategory.fparent := Category.GetValue<cardinal>('parent');
      categories.add(aCategory);
    end;

    // Add it to the parent category list
    // Each category has its own subcategory list
    for aCategory in categories do
    begin
      if aCategory.fparent <> 0 then
        GetCategoryById(aCategory.fparent).fcategories.add(aCategory);
    end;
  end;
end;

procedure TLMS.GetCourses;
var
  aCourse: TLMSCourse;
  aCourses: TJSonArray;
  course: TJSONValue;
  aCourseCategory: TLMSCategory;
begin
  log('Retrieving LMS Courses - may take some time');
  aCourses := aLMSConnection.GetCourses;

  if aCourses <> nil then
  begin
    // log(aCourses.ToString);
    for course in aCourses do
    begin
      aCourse := TLMSCourse.Create(self);

      if course.GetValue<string>('format') = 'site' then
      begin
        // ignore de course site
      end
      else
      begin
        aCourse.Id := course.GetValue<cardinal>('id');
        aCourse.shortname := course.GetValue<string>('shortname');
        aCourse.fullname := course.GetValue<string>('fullname');
        aCourse.displayname := course.GetValue<string>('displayname');
        aCourse.groupmode := course.GetValue<cardinal>('groupmode');

        // Have to check because the category function service could not be enable
        aCourseCategory := GetCategoryById
          (course.GetValue<cardinal>('categoryid'));
        if aCourseCategory <> nil then
          aCourseCategory.fcourses.add(aCourse);
        //

      end;
    end;
  end;

end;

function TLMS.GetUsersByFirstName(var aLMSUsers: TLMSUsers;
  const aFilter: string): integer;
var
  aUsers: TJSonArray;
  User: TJSONValue;
  aUser: TLMSUser;
begin

  aUsers := aLMSConnection.GetUsersByFirstName(aFilter);

  if aUsers <> nil then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TLMSUser.Create;
      aUser.AssignByJson(User);
//      aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  result := aLMSUsers.count;
end;

procedure TLMS.MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
  const aFunctionName: string);
begin
  log('error not service function not defined ' + aFunctionName +
    ' please added it in the LMS service functions');
end;

function TLMS.GetHost: string;
begin
  result := aLMSConnection.Host;
end;

procedure TLMS.SetHost(const Value: string);
begin
  aLMSConnection.Host := Value;
end;

procedure TLMS.SetPassword(const Value: string);
begin
  aLMSConnection.Password := Value;
end;

procedure TLMS.SetService(const Value: string);
begin
  aLMSConnection.Service := Value;
end;

procedure TLMS.SetUser(const Value: string);
begin
  aLMSConnection.User := Value;
end;

function TLMS.GetCategoryById(Id: cardinal): TLMSCategory;
var
  cat: TLMSCategory;
begin
  result := nil;

  for cat in categories do
  begin
    if (cat.Id = Id) then
    begin
      result := cat;
      break;
    end;
  end;

end;

{ TLMSCategory }

constructor TLMSCategory.Create(const parent: TLMS);
begin
  inherited Create;

  fLMS := parent;

  fcourses := TList<TLMSCourse>.Create;
  fcategories := TList<TLMSCategory>.Create;
end;

function TLMSCategory.GetCoursesCount: cardinal;
begin
  result := fcourses.count;
end;

function TLMSCategory.GetLMS: TLMS;
begin
  result := fLMS;
end;

function TLMSCategory.GetSubCategoriesCount: cardinal;
begin
  result := fcategories.count;
end;

{ TLMSCourse }

constructor TLMSCourse.Create(const LMS: TLMS);
begin
  inherited Create;

  fUsers := TLMSUsers.Create;
  fUserGroups := TLMSUserGroups.Create;

  fLMS := LMS;

end;

procedure TLMSCourse.GetCourseRoles(aCourseRoles: TStringlist);
var
  aUser: TLMSUser;
begin
  for aUser in fUsers do
    if aCourseRoles.IndexOf(aUser.fRoles) < 0 then
      aCourseRoles.add(aUser.fRoles);
end;

function TLMSCourse.GetDisplayContent: string;
begin
  result := shortname + ' - ' + displayname;
end;

procedure TLMSCourse.RefreshEnrolledUsers;
var
  aUser: TLMSUser;
  aUsers: TJSonArray;
  User: TJSONValue;

  groups: TJSonArray;
  group: TJSONValue;

  rol: TJSONValue;
  arealrol : string;
begin
  // Populate user groups first
  RefreshUserGroups;

  fUsers.Clear;

  aUsers := fLMS.aLMSConnection.GetEnrolledUsersByCourseId(self.Id);

  if aUsers <> nil then
  begin
    //log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TLMSUser.Create;
      aUser.fCourse := self;
      aUser.AssignByJson(User);

      // Get user roles
      for rol in User.GetValue<TJSonArray>('roles') do
      begin
        arealrol := rol.GetValue<string>('name');
        if arealrol = '' then
          aUser.fRoles := aUser.fRoles + rol.GetValue<string>('shortname')
        else
          aUser.fRoles := aUser.fRoles + arealrol;
      end;

      // Add user to users list
      fUsers.add(aUser);

      // Include the user in the corresponding group
      groups := User.GetValue<TJSonArray>('groups');

      if groups <> nil then
      begin
        // log(groups.ToString);
        for group in groups do
        begin
          for var agroup in self.fUserGroups do
          begin
            if agroup.fid = group.GetValue<cardinal>('id') then
              agroup.fUsersInGroup.add(aUser)
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
  aUserGroup: TLMSUserGroup;
begin
  fUserGroups.Clear;

  aUserGroups := fLMS.aLMSConnection.GetUserGroupsByCourseId(self.Id);

  if aUserGroups <> nil then
  begin
    // log(aUserGroups.ToString);
    for agroup in aUserGroups do
    begin
      aUserGroup := TLMSUserGroup.Create;

      aUserGroup.fid := agroup.GetValue<cardinal>('id');
      aUserGroup.fName := agroup.GetValue<string>('name');
      { aUser.fFirstName := User.GetValue<string>('firstname');
        aUser.fLastName := User.GetValue<string>('lastname');
        aUser.fFullName := User.GetValue<string>('fullname');
      }
      fUserGroups.add(aUserGroup)
    end;
  end;

end;

function TLMSCourse.getFilterContent: string;
begin
  result := shortname + ' ' + fullname + ' ' + self.displayname
end;

function TLMSCourse.GetLMS: TLMS;
begin
  result := fLMS;
end;

function TLMSCourse.GetStudentsCount: integer;
var
  aUser: TLMSUser;
begin
  result := 0;
  for aUser in fUsers do
  begin
    if aUser.fRoles = 'student' then
      inc(result);

  end;

end;

function TLMSCourse.GetUserCountByRol(const aRole: string): cardinal;
var
  aUser: TLMSUser;
begin
  result := 0;
  for aUser in fUsers do
  begin
    if aUser.fRoles = aRole then
      inc(result);
  end;
end;

{ TLMSUserGroup }

constructor TLMSUserGroup.Create;
begin
  fUsersInGroup := TLMSUsers.Create;
end;

function TLMSUserGroup.getFilterContent: string;
begin
  result := fName;
end;

function TLMSUserGroup.getId: cardinal;
begin
  result := fid;
end;

{ TLMSUser }

procedure TLMSUser.AssignByJson(const aJsonValue: TJSONValue);
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

function TLMSUser.getFilterContent: string;
begin
  result := fEmail + ' ' + fFullName;
end;

end.

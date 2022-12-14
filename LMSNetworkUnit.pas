unit LMSNetworkUnit;

interface

uses
  System.Classes,
  Generics.Collections, dialogs, sysutils, LMSRestmoodleunit;

type

  TLMS = class;
  TLMSCategory = class;
  TLMSCourse = class;

  TLMSUser = class
  private
    fEmail : string;
  public
    fCourse : TLMSCourse;

    fid: integer;
    fUserName, fFirstName, fLastName, fFullName: string;

    property Email : string read fEmail;

  end;

  TLMSUsers = TList<TLMSUser>;

  TLMSUserGroup = class
  private
    fid: cardinal;
    function getId: cardinal;
  public
    fName: string;

    // Users in this group
    fUsersInGroup: TLMSUsers;

    constructor Create;

    property Id: cardinal read getId;
  end;

  TLMSUserGroups = TList<TLMSUserGroup>;

  TLMSCourse = class
  private
    fLMS: TLMS;

    function GetFilterContent: string;
    function GetDisplayContent: string;
    function GetLMS: TLMS;

    procedure RefreshUserGroups;
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

    // Pointer to the LMS parent
    property LMS: TLMS read GetLMS;

    // Returns the apropiated text to show information about courses
    property DisplayContent: string read GetDisplayContent;

    // Return the course information that can be filtered from
    property FilterContent: string read GetFilterContent;
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
  System.JSON,
  LMSLogUnit;

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
var
  k: integer;
begin
  result := 0;

  for k := 0 to categories.count - 1 do
  begin
    if categories.items[k].fparent = 0 then
      inc(result)
  end;
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

  categories := TList<TLMSCategory>.Create;
  courses := TList<TLMSCourse>.Create;
end;

procedure TLMS.GetCategories;
var
  aCategory: TLMSCategory;
  aCategories: TJSonArray;
  category: TJSONValue;
begin
  log('Retrieving LMS Categories');
  aCategories := aLMSConnection.GetCategories;

  if aCategories <> nil then
  begin
    // log(aCategories.ToString);

    for category in aCategories do
    begin
      // Add to a global category list
      aCategory := TLMSCategory.Create(self);
      aCategory.Id := category.GetValue<cardinal>('id');
      aCategory.name := category.GetValue<string>('name');
      aCategory.fparent := category.GetValue<cardinal>('parent');
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
begin
  log('Retrieving LMS Courses - may take some time');
  aCourses := aLMSConnection.GetCourses;

  if aCourses <> nil then
  begin
    log(aCourses.ToString);
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
        GetCategoryById(course.GetValue<cardinal>('categoryid'))
          .fcourses.add(aCourse);

      end;
    end;
  end;

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
      aUser := TLMSUser.Create;
      aUser.fCourse := self;

      aUser.fid := User.GetValue<cardinal>('id');
      aUser.fUserName := User.GetValue<string>('username');
      aUser.fFirstName := User.GetValue<string>('firstname');
      aUser.fLastName := User.GetValue<string>('lastname');
      aUser.fFullName := User.GetValue<string>('fullname');
      aUser.fEmail := User.GetValue<string>('email');

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

function TLMSCourse.GetFilterContent: string;
begin
  result := shortname + ' ' + fullname + ' ' + self.displayname
end;

function TLMSCourse.GetLMS: TLMS;
begin
  result := fLMS;
end;

{ TLMSUserGroup }

constructor TLMSUserGroup.Create;
begin
  fUsersInGroup := TLMSUsers.Create;
end;

function TLMSUserGroup.getId: cardinal;
begin
  result := fId;
end;

end.

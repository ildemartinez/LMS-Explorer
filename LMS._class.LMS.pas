unit LMS._class.LMS;

interface

uses
  System.Classes,
  Generics.Collections,

  LMS._interface.LMS,
 // lmsnetworkunit,
  LMS.Rest.Moodle;

type

  TLMS = class(TComponent, ILMS)
  private
    fId: string;
    fautoconnect: boolean;

    aLMSConnection: TLMSRestMoodle;

    // All LMS categories
    fCategories: TListCategory;

    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
    function GetHost: string;

    procedure MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
      const aFunctionName: string);

    function GetId: string;
    procedure SetId(const aId: string);
    function GetAutoConnect: boolean;
    procedure SetAutoConnect(const Value: boolean);
    procedure SetCategories(const Value: TListCategory);
    function GetCategories: TListCategory;
  public

    // All LMS courses
    courses: TList<ICourse>;

    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function connected: boolean;

    function FirstLevelCategoriesCount: cardinal;
    function GetCategoryById(Id: cardinal): ICategory;
    function GetUsersByAlmostAllFields(var aLMSUsers: TLMSUsers;
      const aFilter: string): integer;

    procedure GetCategoriesFromConnection;
    procedure GetCourses;

    function GetLMSConnection: TLMSRestMoodle;

    property Id: string read GetId;
    property AutoConnect: boolean read GetAutoConnect write SetAutoConnect;
    property User: string write SetUser;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property Host: string read GetHost write SetHost;

    property Categories: TListCategory read GetCategories write SetCategories;

  end;

implementation

uses
  System.JSON,

  LMS._class.User,
  LMSNetworkunit,
  LMS.Helper.Log;

function TLMS.FirstLevelCategoriesCount: cardinal;
begin
  result := 0;

  for var Category in Categories do
    if Category.ParentCategory = 0 then
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
  inherited;

  aLMSConnection := TLMSRestMoodle.Create(self);
  aLMSConnection.OnFunctionNotAdded := MyOnFunctionNotAdded;

  Categories := TList<ICategory>.Create;
  courses := TList<ICourse>.Create;
end;

function TLMS.GetAutoConnect: boolean;
begin
  result := fautoconnect;
end;

procedure TLMS.GetCategoriesFromConnection;
var
  aCategory: ICategory;
  aCategories: TJSonArray;
  Category: TJSONValue;
begin
  // log('Retrieving LMS Categories');
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
      aCategory.ParentCategory := Category.GetValue<cardinal>('parent');
      Categories.add(aCategory);
    end;

    // Add it to the parent category list
    // Each category has its own subcategory list
    for aCategory in Categories do
    begin
      if aCategory.ParentCategory <> 0 then
        GetCategoryById(aCategory.ParentCategory).Categories.add(aCategory);
    end;
  end;
end;

function TLMS.GetCategories: TListCategory;
begin
  result := fCategories;
end;

procedure TLMS.GetCourses;
var
  aCourse: TLMSCourse;
  aCourses: TJSonArray;
  Course: TJSONValue;
  aCourseCategory: ICategory;
begin
  // log('Retrieving LMS Courses - may take some time');
  aCourses := aLMSConnection.GetCourses;

  if aCourses <> nil then
  begin
    // log(aCourses.ToString);
    for Course in aCourses do
    begin
      aCourse := TLMSCourse.Create(self);

      if Course.GetValue<string>('format') = 'site' then
      begin; // ignore de course site
      end
      else
      begin
        aCourse.Id := Course.GetValue<cardinal>('id');
        aCourse.shortname := Course.GetValue<string>('shortname');
        aCourse.FullName := Course.GetValue<string>('fullname');
        aCourse.displayname := Course.GetValue<string>('displayname');
        aCourse.GroupMode := Course.GetValue<cardinal>('groupmode');

        // Have to check because the category function service could not be enable
        aCourseCategory := GetCategoryById
          (Course.GetValue<cardinal>('categoryid'));
        if aCourseCategory <> nil then
          aCourseCategory.courses.add(aCourse);
        //

      end;
    end;
  end;

end;

function TLMS.GetUsersByAlmostAllFields(var aLMSUsers: TLMSUsers;
  const aFilter: string): integer;
var
  aUsers: TJSonArray;
  User: TJSONValue;
  aUser: IUser;
begin

  aUsers := aLMSConnection.GetUsersByFirstName(aFilter);

  if (aUsers <> nil) and (aUsers.count > 0) then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create;
      aUser.AssignByJson(User);
      // aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  aUsers := aLMSConnection.GetUsersByLastName(aFilter);

  if (aUsers <> nil) and (aUsers.count > 0) then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create;
      aUser.AssignByJson(User);
      // aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  aUsers := aLMSConnection.GetUsersByEmail(aFilter);

  if (aUsers <> nil) and (aUsers.count > 0) then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create;
      aUser.AssignByJson(User);
      // aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  result := aLMSUsers.count;
end;

procedure TLMS.MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
  const aFunctionName: string);
begin
  Log('error not service function not defined ' + aFunctionName +
    ' please added it in the LMS service functions');
end;

function TLMS.GetHost: string;
begin
  result := aLMSConnection.Host;
end;

function TLMS.GetId: string;
begin
  result := fId;
end;

function TLMS.GetLMSConnection: TLMSRestMoodle;
begin
  result := aLMSConnection;
end;

procedure TLMS.SetAutoConnect(const Value: boolean);
begin
  fautoconnect := Value;
end;

procedure TLMS.SetCategories(const Value: TListCategory);
begin
  fCategories := Value;
end;

procedure TLMS.SetHost(const Value: string);
begin
  aLMSConnection.Host := Value;
end;

procedure TLMS.SetId(const aId: string);
begin
  fId := aId;
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

function TLMS.GetCategoryById(Id: cardinal): ICategory;
var
  cat: ICategory;
begin
  result := nil;

  for cat in Categories do
  begin
    if (cat.Id = Id) then
    begin
      result := cat;
      break;
    end;
  end;

end;

end.

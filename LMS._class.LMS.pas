unit LMS._class.LMS;

interface

uses
  System.Classes,
  Generics.Collections,

  LMS._interface.LMS,
  LMS.Rest.Moodle;

type

  TLMS = class(TComponent, ILMS)
  strict private
    fautoconnect: boolean;
    // All LMS categories
    fCategories: TList<ICategory>;
    fConnection: TLMSRestMoodle;
    fFlatCourses: TList<ICourse>;
    fId: string;
    // procedure DownloadContent(const Content: IContent);
    procedure DownloadAllCourseContent(const Course: ICourse);
    function GetAutoConnect: boolean;
    function GetCategories: TList<ICategory>;
    function GetFlatCourses: TList<ICourse>;
    function GetHost: string;
    function GetId: string;
    procedure MyOnFunctionNotAdded(Sender: TLMSRestMoodle; const aFunctionName: string);
    procedure SetAutoConnect(const Value: boolean);
    procedure SetCategories(const Value: TList<ICategory>);
    procedure SetFlatCourses(const Value: TList<ICourse>);
    procedure SetHost(const Value: string);
    procedure SetId(const aId: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
  public
    constructor Create(Owner: TComponent); override;
    procedure Connect;
    function connected: boolean;
    function FirstLevelCategoriesCount: cardinal;
    procedure GetCategoriesFromConnection;
    function GetCategoryById(const Id: cardinal): ICategory;
    function GetCourseById(const Id: cardinal): ICourse;
    procedure GetCourses;
    function GetLMSConnection: TLMSRestMoodle;
    function GetUsersByAlmostAllFields(var aLMSUsers: TList<IUser>; const aFilter: string): integer;
    property AutoConnect: boolean read GetAutoConnect write SetAutoConnect;
    property Categories: TList<ICategory> read GetCategories write SetCategories;
    property FlatCourses: TList<ICourse> read GetFlatCourses write SetFlatCourses;
    property Host: string read GetHost write SetHost;
    property Id: string read GetId;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property User: string write SetUser;
  end;

implementation

uses
  System.JSON,
  System.DateUtils,
  System.SysUtils,
  System.IOUtils,
  forms,

  LMS._class.User,
  LMS._class.Course,
  LMS._class.Category,
  LMS.Helper.Log;

constructor TLMS.Create(Owner: TComponent);
begin
  inherited;

  // Create the Rest Moodle Connection
  fConnection := TLMSRestMoodle.Create(self);
  fConnection.OnFunctionNotAdded := MyOnFunctionNotAdded;

  // LMS Categories
  Categories := TList<ICategory>.Create;

  // All courses in flat hiearchy
  FlatCourses := TList<ICourse>.Create;
end;

procedure TLMS.Connect;
begin
  fConnection.Connect;
end;

function TLMS.connected: boolean;
begin
  result := fConnection.connected;
end;

procedure TLMS.DownloadAllCourseContent(const Course: ICourse);
var
  path: string;

  aSection: ISection;
  aModule: IModule;
  aContent: IContent;

begin
  path := ExtractFileDir(Application.ExeName);

  path := path + '\content\' + Course.LMS.Id + '\' + Course.Category.Name + '\'
    + Course.shortname;

  path := StringReplace(path, '/', '-', [rfReplaceAll]);

  TDirectory.CreateDirectory(path);

  SetCurrentDir(path);

  for aSection in Course.Sections do
  begin
    for aModule in aSection.Modules do
    begin
      for aContent in aModule.Contents do
      begin
        Log(aContent.FileURL);

        Course.LMS.GetLMSConnection.DownloadContent(aContent.FileURL,
          //path +
          '.\' + aContent.FileName);
      end;
    end;
  end;
  log('Download done!!!');
end;

function TLMS.FirstLevelCategoriesCount: cardinal;
begin
  result := 0;

  for var Category in Categories do
    if Category.ParentCategory = 0 then
      inc(result);
end;

function TLMS.GetAutoConnect: boolean;
begin
  result := fautoconnect;
end;

function TLMS.GetCategories: TList<ICategory>;
begin
  result := fCategories;
end;

procedure TLMS.GetCategoriesFromConnection;
var
  aCategory: ICategory;
  aCategories: TJSonArray;
  Category: TJSONValue;
begin
  // log('Retrieving LMS Categories');
  aCategories := fConnection.GetCategories;

  if aCategories <> nil then
  begin
    // log(aCategories.ToString);

    for Category in aCategories do
    begin
      // Add to a global category list
      aCategory := TCategory.Create(self, Category);
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

function TLMS.GetCategoryById(const Id: cardinal): ICategory;
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

function TLMS.GetCourseById(const Id: cardinal): ICourse;
var
  cat: ICategory;
  cour: ICourse;
begin
  result := nil;

  for cat in Categories do
  begin
    for cour in cat.Courses do

      if (cour.Id = Id) then
      begin
        result := cour;
        break;
      end;
  end;

end;

procedure TLMS.GetCourses;
var
  aCourse: TLMSCourse;
  aCourses: TJSonArray;
  Course: TJSONValue;
  aCourseCategory: ICategory;
begin
  // log('Retrieving LMS Courses - may take some time');
  aCourses := fConnection.GetCourses;

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
        aCourse.DisplayName := Course.GetValue<string>('displayname');
        aCourse.GroupMode := Course.GetValue<cardinal>('groupmode');

        // if aJSONValue.TryGetValue<Int64>('lastcourseaccess', timestamp) then
        // flastcourseaccess := unixtodatetime(timestamp);

        aCourse.Start_Date :=
          unixtodatetime(Course.GetValue<Int64>('startdate'));
        aCourse.End_Date := unixtodatetime(Course.GetValue<Int64>('enddate'));
        aCourse.Time_Created :=
          unixtodatetime(Course.GetValue<Int64>('timecreated'));
        aCourse.Time_Modified :=
          unixtodatetime(Course.GetValue<Int64>('timemodified'));

        // Have to check because the category function service could not be enable
        aCourseCategory := GetCategoryById
          (Course.GetValue<cardinal>('categoryid'));
        if aCourseCategory <> nil then
          aCourseCategory.Courses.add(aCourse);

        // Add course in flat hierarchy
        FlatCourses.add(aCourse);
        //

      end;
    end;
  end;

end;

function TLMS.GetFlatCourses: TList<ICourse>;
begin
  result := fFlatCourses;
end;

function TLMS.GetHost: string;
begin
  result := fConnection.Host;
end;

function TLMS.GetId: string;
begin
  result := fId;
end;

function TLMS.GetLMSConnection: TLMSRestMoodle;
begin
  result := fConnection;
end;

function TLMS.GetUsersByAlmostAllFields(var aLMSUsers: TList<IUser>; const aFilter: string): integer;
var
  aUsers: TJSonArray;
  User: TJSONValue;
  aUser: IUser;
begin
  aUsers := fConnection.GetUsersByFirstName(aFilter);

  if (aUsers <> nil) and (aUsers.count > 0) then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create(self, User);
      // aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  aUsers := fConnection.GetUsersByLastName(aFilter);

  if (aUsers <> nil) and (aUsers.count > 0) then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create(self, User);
      // aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  aUsers := fConnection.GetUsersByEmail(aFilter);

  if (aUsers <> nil) and (aUsers.count > 0) then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create(self, User);
      // aUser.fCourse.fLMS := self;  // set the LMS of the user
      aLMSUsers.add(aUser);
    end;
  end;

  result := aLMSUsers.count;
end;

procedure TLMS.MyOnFunctionNotAdded(Sender: TLMSRestMoodle; const aFunctionName: string);
begin
  Log('error not service function not defined ' + aFunctionName +
    ' please added it in the LMS service functions');
end;

procedure TLMS.SetAutoConnect(const Value: boolean);
begin
  fautoconnect := Value;
end;

procedure TLMS.SetCategories(const Value: TList<ICategory>);
begin
  fCategories := Value;
end;

procedure TLMS.SetFlatCourses(const Value: TList<ICourse>);
begin
  fFlatCourses := Value;
end;

procedure TLMS.SetHost(const Value: string);
begin
  fConnection.Host := Value;
end;

procedure TLMS.SetId(const aId: string);
begin
  fId := aId;
end;

procedure TLMS.SetPassword(const Value: string);
begin
  fConnection.Password := Value;
end;

procedure TLMS.SetService(const Value: string);
begin
  fConnection.Service := Value;
end;

procedure TLMS.SetUser(const Value: string);
begin
  fConnection.User := Value;
end;

end.

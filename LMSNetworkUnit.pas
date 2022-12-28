unit LMSNetworkUnit;

interface

uses
  System.Classes,
  Generics.Collections, dialogs, sysutils, LMSRestmoodleunit;

type

  TLMS = class;
  TLMSCategory = class;

  TLMSCourse = class
  private
    function GetFilterContent: string;
    function GetDisplayContent: string;
  public
    fLMS : tlms;

    id: cardinal;
    shortname: string;
    fullname: string;
    displayname: string;

    constructor Create(const LMS : TLMS);

    // Returns the apropiated text to show information about courses
    property DisplayContent: string read GetDisplayContent;

    property FilterContent: string read GetFilterContent;
  end;

  TLMSCategory = class
  private
    function GetCoursesCount: integer;
    function GetSubCategoriesCount: cardinal;
  public
    fLMS: TLMS;
    fcategories: TList<TLMSCategory>;
    fcourses: TList<TLMSCourse>;
    id: cardinal;
    // categoryid : cardinal;
    name: string;
    fparent: cardinal;

    constructor Create(const parent : TLMS);

    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
    property CoursesCount: integer read GetCoursesCount;

  end;

  TLMS = class(TComponent)
  private
    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
    function GetHost: string;
  public
    id: string;
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
    function GetCategoryById(id: cardinal): TLMSCategory;

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
      aCategory.id := category.GetValue<cardinal>('id');
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
        aCourse.id := course.GetValue<cardinal>('id');
        aCourse.shortname := course.GetValue<string>('shortname');
        aCourse.fullname := course.GetValue<string>('fullname');
        aCourse.displayname := course.GetValue<string>('displayname');
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

function TLMS.GetCategoryById(id: cardinal): TLMSCategory;
var
  cat: TLMSCategory;
begin
  result := nil;

  for cat in categories do
  begin
    if (cat.id = id) then
    begin
      result := cat;
      break;
    end;
  end;

end;

{ TLMSCategory }

constructor TLMSCategory.Create(const parent : TLMS);
begin
  inherited create;

  fLMS := parent;

  fcourses := TList<TLMSCourse>.Create;
  fcategories := TList<TLMSCategory>.Create;
end;

function TLMSCategory.GetCoursesCount: integer;
begin
  result := fcourses.count;
end;

function TLMSCategory.GetSubCategoriesCount: cardinal;
begin
  result := fcategories.count;
end;

{ TLMSCourse }

constructor TLMSCourse.Create(const LMS : TLMS);
begin
  inherited create;

  fLMS := lms;


end;

function TLMSCourse.GetDisplayContent: string;
begin
  result := shortname + ' - ' + displayname;
end;

function TLMSCourse.GetFilterContent: string;
begin
  result := shortname + ' ' + fullname + ' ' + self.displayname
end;

end.

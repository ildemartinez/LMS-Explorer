unit LMSNetworkUnit;

interface

uses
  System.Classes,
  Generics.Collections, dialogs, sysutils, LMSRestmoodleunit;

type

  TLMSCourse = class
    id: cardinal;
    name: string;
  end;

  TLMSCategory = class
  private
    fcourses: TList<TLMSCourse>;

    function GetCoursesCount: integer;
    function GetSubCategoriesCount: cardinal;
  public
    fcategories: TList<TLMSCategory>;
    id: cardinal;
    name: string;
    fparent: cardinal;

    constructor Create;

    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
    property CoursesCount: integer read GetCoursesCount;

  end;

  TLMS = class(TComponent)
  private
    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
  public
    id: string;

    categories: TList<TLMSCategory>;
    courses: TList<TLMSCourse>;

    aLMSConnection: TLMSRestMoodle;

    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function connected: boolean;

    function CategoriesLevel(level: cardinal): cardinal;

    function GetCategoryById(id: cardinal): TLMSCategory;

    procedure GetCategories;
    procedure GetCourses;

    property User: string write SetUser;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property Host: string write SetHost;
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

function TLMS.CategoriesLevel(level: cardinal): cardinal;
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
  aCategory : TLMSCategory;
  aCategories: TJSonArray;
  category: TJSONValue;
begin

  aCategories := aLMSConnection.GetCategories;

  if aCategories <> nil then
  begin
    log(aCategories.ToString);

    for category in aCategories do
    begin
      // Add to a global category list
      aCategory := TLMSCategory.Create;
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

  aCourses := aLMSConnection.GetCourses;

  if aCourses <> nil then
  begin
    log(aCourses.ToString);
    for course in aCourses do
    begin
      aCourse := TLMSCourse.Create;

      if course.GetValue<string>('format') = 'site' then
      begin
        // ignore de course site
      end
      else
      begin
        aCourse.id := course.GetValue<cardinal>('id');
        aCourse.name := course.GetValue<string>('shortname');
        GetCategoryById(course.GetValue<cardinal>('categoryid'))
          .fcourses.add(aCourse);
      end;
    end;
  end;

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

constructor TLMSCategory.Create;
begin
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

end.

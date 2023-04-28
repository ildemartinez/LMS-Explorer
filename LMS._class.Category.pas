unit LMS._class.Category;

interface

uses
  Generics.Collections,
  System.JSON,

  LMS._interface.LMS;

type
  TCategory = class(TInterfacedObject, ICategory)
  private
    fcategories: TList<ICategory>;
    fcourses: TList<ICourse>;
    fid: cardinal;
    fLMS: ILMS;
    fName: string;
    fParentCategory: cardinal;
    function GetCategories: TList<ICategory>;
    function GetCourses: TList<ICourse>;
    function GetCoursesCount: cardinal;
    function getId: cardinal;
    function GetLMS: ILMS;
    function GetName: string;
    function GetParentCategory: cardinal;
    function GetSubCategoriesCount: cardinal;
    procedure SetParentCategory(const Value: cardinal);
  public
    // categoryid : cardinal;
    constructor Create(const parent: ILMS; const Values: TJSONValue);
    property Categories: TList<ICategory> read GetCategories;
    property Courses: TList<ICourse> read GetCourses;
    property CoursesCount: cardinal read GetCoursesCount;
    property Id: cardinal read getId;
    // Pointer to the LMS parent
    property LMS: ILMS read GetLMS;
    property Name: string read GetName;
    property ParentCategory: cardinal read GetParentCategory write SetParentCategory;
    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
  end;

implementation

const
  SParent = 'parent';
  SName = 'name';
  SId = 'id';

constructor TCategory.Create(const parent: ILMS; const Values: TJSONValue);
begin
  inherited Create;

  fLMS := parent;

  fcourses := TList<ICourse>.Create;
  fcategories := TList<ICategory>.Create;

  fid := Values.GetValue<cardinal>(SId);
  fName := Values.GetValue<string>(SName);
  ParentCategory := Values.GetValue<cardinal>(SParent);
end;

function TCategory.GetCategories: TList<ICategory>;
begin
  result := fcategories;
end;

function TCategory.GetCourses: TList<ICourse>;
begin
  result := fcourses;
end;

function TCategory.GetCoursesCount: cardinal;
begin
  result := fcourses.count;
end;

function TCategory.getId: cardinal;
begin
  result := fid;
end;

function TCategory.GetLMS: ILMS;
begin
  result := fLMS;
end;

function TCategory.GetName: string;
begin
  result := fName;
end;

function TCategory.GetParentCategory: cardinal;
begin
  result := fParentCategory;
end;

function TCategory.GetSubCategoriesCount: cardinal;
begin
  result := fcategories.count;
end;

procedure TCategory.SetParentCategory(const Value: cardinal);
begin
  fParentCategory := Value;
end;

end.

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
    property GroupName: string read GetGroupName write SetGroupName;
    property FilterContent: string read getFilterContent;
    property UsersInGroup: TList<IUser> read GetUsersInGroup write SetUsersInGroup;
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
    function GetName: string;
    function GetCategories: TList<ICategory>;
    function GetCourses: TList<ICourse>;
  public
    // categoryid : cardinal;
    constructor Create(const parent: ILMS; const Values : TJSONValue);

    // Pointer to the LMS parent
    property LMS: ILMS read GetLMS;

    property Id: cardinal read getId;
    property Name: string read GetName;
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

constructor TLMSCategory.Create(const parent: ILMS; const Values : TJSONValue);
begin
  inherited Create;

  fLMS := parent;

  fcourses := TList<ICourse>.Create;
  fcategories := TList<ICategory>.Create;

  fId := values.GetValue<cardinal>('id');
  fName := values.GetValue<string>('name');
  ParentCategory := values.GetValue<cardinal>('parent');
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

procedure TLMSCategory.SetParentCategory(const Value: cardinal);
begin
  fParentCategory := Value;
end;

{ TLMSUserGroup }

constructor TLMSUserGroup.Create;
begin
  fUsersInGroup := TList<IUser>.Create;
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

function TLMSUserGroup.GetUsersInGroup: TList<IUser>;
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

procedure TLMSUserGroup.SetUsersInGroup(const Value: TList<IUser>);
begin
  fUsersInGroup := Value;
end;


end.

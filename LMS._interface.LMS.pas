unit LMS._interface.LMS;

interface

uses
  Generics.Collections,

  LMS.Rest.Moodle;

type
  ILMS = interface;
  ILMSUser = interface;
  ILMSCourse = interface;

  TLMSUsers = TList<ILMSUser>;

  ILMSUserGroup = interface
    ['{586BCC1C-EA18-4D15-A788-3BF00EFD328F}']
    function getId: cardinal;
    function GetUsersInGroup: TLMSUsers;
    procedure SetUsersInGroup(const Value: TLMSUsers);
    procedure SetId(const Value: cardinal);
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GetFilterContent: string;

    property Id: cardinal read getId write SetId;
    property GroupName: string read GetGroupName write SetGroupName;
    property UsersInGroup: TLMSUsers read GetUsersInGroup write SetUsersInGroup;
    property FilterContent: string read GetFilterContent;
  end;

  TLMSUserGroups = TList<ILMSUserGroup>;

  ILMSUser = interface
    ['{F9286941-71B4-4B95-9E3E-201708B28483}']
    function GetRoles: string;
    function GetFilterContent: string;
    function GetEmail: string;
    function GetFirstName: string;
    function GetFullName: string;
    function GetLastName: string;
    function GetCourse: ILMSCourse;
    function getId: integer;
    function GetUserName: string;

    property Id: integer read getId;
    property UserName: string read GetUserName;
    property Full_Name: string read GetFullName;
    property First_Name: string read GetFirstName;
    property Last_Name: string read GetLastName;
    property Email: string read GetEmail;
    property Course: ILMSCourse read GetCourse;

    property Roles: string read GetRoles;
    property FilterContent: string read GetFilterContent;
  end;

  ILMSCourse = interface
    ['{5B363A62-53F5-4156-8920-D6338355446D}']
    function GetLMS: ILMS;
    function getId: cardinal;
    function GetGroupMode: cardinal;
    procedure SetGroupMode(const Value: cardinal);
    procedure RefreshUserGroups;
    procedure RefreshEnrolledUsers;
    function GetUserGroups: TLMSUserGroups;
    function GetUsers: TLMSUsers;
    procedure SetUserGroups(const Value: TLMSUserGroups);
    procedure SetUsers(const Value: TLMSUsers);
    function GetFullName: string;
    function GetShortName: string;
    procedure SetFullName(const Value: string);
    procedure SetShortName(const Value: string);
    function GetDisplayName: string;
    function getFilterContent: string;
    procedure SetDisplayName(const Value: string);

    property LMS: ILMS read GetLMS;
    property shortname: string read GetShortName write SetShortName;
    property FullName: string read GetFullName write SetFullName;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property Id: cardinal read getId;
    property GroupMode: cardinal read GetGroupMode write SetGroupMode;
    property Users: TLMSUsers read GetUsers write SetUsers;
        property FilterContent: string read getFilterContent;
    // All course groups
    property UserGroups: TLMSUserGroups read GetUserGroups write SetUserGroups;
  end;

  ILMSCategory = interface
    ['{5D65887F-0D0A-4B43-8FB3-4F35B0911C7E}']
    function GetParentCategory: cardinal;
    procedure SetParentCategory(const Value: cardinal);
    function getId: cardinal;
    procedure SetId(const Value: cardinal);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetCategories: TList<ILMSCategory>;
    function GetCourses: TList<ILMSCourse>;
    function GetCoursesCount: cardinal;
    function GetSubCategoriesCount: cardinal;
    function GetLMS: ILMS;

    property LMS: ILMS read GetLMS;
    property Id: cardinal read getId write SetId;
    property ParentCategory: cardinal read GetParentCategory
      write SetParentCategory;
    property Name: string read GetName write SetName;
    property Categories: TList<ILMSCategory> read GetCategories;
    property Courses: TList<ILMSCourse> read GetCourses;
    property CoursesCount: cardinal read GetCoursesCount;
    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
  end;

  TListCategory = TList<ILMSCategory>;

  ILMS = interface
    ['{F67CAF42-24D9-4906-BE7A-09E9A669D9F1}']
    function GetLMSConnection: TLMSRestMoodle;

    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
    function GetHost: string;

    procedure SetCategories(const Value: TListCategory);
    function GetCategories: TListCategory;

    function getId: string;
    procedure SetId(const aId: string);

    function GetAutoConnect: boolean;
    procedure SetAutoConnect(const Value: boolean);

    procedure MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
      const aFunctionName: string);

    // All LMS categories
    // categories: TList<TLMSCategory>;
    // All LMS courses
    // courses: TList<TLMSCourse>;

    procedure Connect;
    function connected: boolean;

    function GetCategoryById(Id: cardinal): ILMSCategory;
    function FirstLevelCategoriesCount: cardinal;
    // function GetCategoryById(Id: cardinal): TLMSCategory;
    // function GetUsersByAlmostAllFields(var aLMSUsers: TLMSUsers;      const aFilter: string): integer;

    // procedure GetCategories;
    procedure GetCourses;

    property Id: string read getId write SetId;
    property AutoConnect: boolean read GetAutoConnect write SetAutoConnect;
    property User: string write SetUser;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property Host: string read GetHost write SetHost;
    property Categories: TListCategory read GetCategories write SetCategories;

    property aLMSConnection: TLMSRestMoodle read GetLMSConnection;
  end;

implementation

end.

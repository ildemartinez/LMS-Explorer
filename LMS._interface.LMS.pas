unit LMS._interface.LMS;

interface

uses
  System.Classes,
  Generics.Collections,
  System.JSON,

  LMS.Rest.Moodle;

type
  ILMS = interface;
  IUser = interface;
  ICourse = interface;
  ICategory = interface;
  IModule = interface;
  ISection = interface;

  IGradeItem = interface
    ['{44E6EA75-B61B-448F-A7C5-73DF21A28C7F}']
    function getItemname: string;
    property ItemName: string read getItemname;
  end;

  IUsersGroup = interface
    ['{586BCC1C-EA18-4D15-A788-3BF00EFD328F}']
    function GetFilterContent: string;
    function GetGroupName: string;
    function getId: cardinal;
    function GetUsersInGroup: TList<IUser>;
    procedure SetGroupName(const Value: string);
    procedure SetId(const Value: cardinal);
    procedure SetUsersInGroup(const Value: TList<IUser>);
    property FilterContent: string read GetFilterContent;
    property Group_Name: string read GetGroupName write SetGroupName;
    property Id: cardinal read getId write SetId;
    property UsersInGroup: TList<IUser> read GetUsersInGroup write SetUsersInGroup;
  end;

  IUser = interface
    ['{F9286941-71B4-4B95-9E3E-201708B28483}']
    function GetCourse: ICourse;
    function GetEmail: string;
    function GetFilterContent: string;
    function GetFirstName: string;
    function GetFullName: string;
    function getId: integer;
    function GetLastName: string;
    function GetLMS: ILMS;
    function GetOtherEnrolledCourses: TList<ICourse>;
    function GetRoles: string;
    function GetUserName: string;
    procedure SetCourse(const Value: ICourse);
    procedure SetRoles(const Value: string);
    property Course: ICourse read GetCourse write SetCourse;
    property Email: string read GetEmail;
    property FilterContent: string read GetFilterContent;
    property First_Name: string read GetFirstName;
    property Full_Name: string read GetFullName;
    property Id: integer read getId;
    property Last_Name: string read GetLastName;
    property LMS: ILMS read GetLMS;
    property OtherEnrolledCourses: TList<ICourse> read GetOtherEnrolledCourses;
    property Roles: string read GetRoles write SetRoles;
    property UserName: string read GetUserName;
  end;

  TModType = (mnforum, mnlabel, mnresource, mnunknow);

  IContent = interface
    ['{F74F2674-738F-4464-880F-0C2F71E092A1}']
    function GetFileName: string;
    function GetFileURL: string;
    function GetMiMeType: string;
    function GetModule: IModule;
    procedure SetFilename(const Value: string);
    procedure SetFileURL(const Value: string);
    procedure SetMimeType(const Value: string);
    property FileName: string read GetFileName write SetFilename;
    property FileURL: string read GetFileURL write SetFileURL;
    property MimeType: string read GetMiMeType write SetMimeType;
    property Module: IModule read GetModule;
  end;

  IModule = interface
    ['{9378D0F9-91C4-41D2-82C5-9D3596A14964}']
    function GetContents: TList<IContent>;
    function GetModType: TModType;
    function GetName: string;
    function GetSection: ISection;
    procedure SetModName(const Value: string);
    procedure SetName(const Value: string);
    property Contents: TList<IContent> read GetContents;
    property ModName: string write SetModName;
    property ModType: TModType read GetModType;
    property Name: string read GetName write SetName;
    property Section: ISection read GetSection;
  end;

  ISection = interface
    ['{CD546519-E83C-4938-9903-F8BCEDB3FBA8}']
    function GetModules: TList<IModule>;
    function GetName: string;
    procedure SetName(const Value: string);
    property Modules: TList<IModule> read GetModules;
    property Name: string read GetName write SetName;
  end;

  ICourse = interface
    ['{5B363A62-53F5-4156-8920-D6338355446D}']
    function GetCategory: ICategory;
    procedure GetCourseContent;
    procedure GetCourseRoles(aCourseRoles: TStringlist);
    function GetDisplayContent: string;
    function GetDisplayName: string;
    function GetEndDate: TDateTime;
    function GetFilterContent: string;
    function GetFullName: string;
    procedure GetGradeBook;
    function GetGradeItems: TList<IGradeItem>;
    function GetGroupMode: cardinal;
    function getId: cardinal;
    function GetLMS: ILMS;
    function GetSections: TList<ISection>;
    function GetShortName: string;
    function GetStartDate: TDateTime;
    function GetTimeCreated: TDateTime;
    function GetTimeModified: TDateTime;
    function GetUserCountByRol(const aRole: string): cardinal;
    function GetUserGroups: TList<IUsersGroup>;
    function GetUsers: TList<IUser>;
    procedure RefreshEnrolledUsers;
    procedure RefreshUserGroups;
    procedure SetDisplayName(const Value: string);
    procedure SetEndDate(const Value: TDateTime);
    procedure SetFullName(const Value: string);
    procedure SetGroupMode(const Value: cardinal);
    procedure SetShortName(const Value: string);
    procedure SetStartDate(const Value: TDateTime);
    procedure SetTimeCreated(const Value: TDateTime);
    procedure SetTimeModified(const Value: TDateTime);
    procedure SetUserGroups(const Value: TList<IUsersGroup>);
    procedure SetUsers(const Value: TList<IUser>);
    property Category: ICategory read GetCategory;
    property DisplayContent: string read GetDisplayContent;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property EndDate: TDateTime read GetEndDate write SetEndDate;
    property FilterContent: string read GetFilterContent;
    property FullName: string read GetFullName write SetFullName;
    property GradeItems: TList<IGradeItem> read GetGradeItems;
    property GroupMode: cardinal read GetGroupMode write SetGroupMode;
    property Id: cardinal read getId;
    property LMS: ILMS read GetLMS;
    property Sections: TList<ISection> read GetSections;
    property shortname: string read GetShortName write SetShortName;
    property StartDate: TDateTime read GetStartDate write SetStartDate;
    property TimeCreated: TDateTime read GetTimeCreated write SetTimeCreated;
    property TimeModified: TDateTime read GetTimeModified write SetTimeModified;
    // All course groups
    property UserGroups: TList<IUsersGroup> read GetUserGroups
      write SetUserGroups;
    property Users: TList<IUser> read GetUsers write SetUsers;
  end;

  ICategory = interface
    ['{5D65887F-0D0A-4B43-8FB3-4F35B0911C7E}']
    function GetCategories: TList<ICategory>;
    function GetCourses: TList<ICourse>;
    function GetCoursesCount: cardinal;
    function getId: cardinal;
    function GetLMS: ILMS;
    function GetName: string;
    function GetParentCategory: cardinal;
    function GetSubCategoriesCount: cardinal;
    procedure SetParentCategory(const Value: cardinal);
    property Categories: TList<ICategory> read GetCategories;
    property Courses: TList<ICourse> read GetCourses;
    property CoursesCount: cardinal read GetCoursesCount;
    property Id: cardinal read getId;
    property LMS: ILMS read GetLMS;
    property Name: string read GetName;
    property ParentCategory: cardinal read GetParentCategory
      write SetParentCategory;
    property SubCategoriesCount: cardinal read GetSubCategoriesCount;
  end;

  ILMS = interface
    ['{F67CAF42-24D9-4906-BE7A-09E9A669D9F1}']
    // All LMS categories
    // categories: TList<TLMSCategory>;
    // All LMS courses
    // courses: TList<TLMSCourse>;

    procedure Connect;
    function connected: boolean;
    // procedure DownloadContent(const Content : IContent);
    procedure DownloadAllCourseContent(const Course: ICourse);
    function FirstLevelCategoriesCount: cardinal;
    function GetAutoConnect: boolean;
    function GetCategories: TList<ICategory>;
    procedure GetCategoriesFromConnection;
    function GetCategoryById(const Id: cardinal): ICategory;
    function GetCourseById(const Id: cardinal): ICourse;
    // function GetCategoryById(Id: cardinal): TLMSCategory;
    // function GetUsersByAlmostAllFields(var aLMSUsers: TLMSUsers;      const aFilter: string): integer;

    // procedure GetCategories;
    procedure GetCourses;
    function GetFlatCourses: TList<ICourse>;
    function GetHost: string;
    function getId: string;
    function GetLMSConnection: TLMSRestMoodle;
    function GetUsersByAlmostAllFields(var aLMSUsers: TList<IUser>;
      const aFilter: string): integer;
    procedure MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
      const aFunctionName: string);
    procedure SetAutoConnect(const Value: boolean);
    procedure SetCategories(const Value: TList<ICategory>);
    procedure SetFlatCourses(const Value: TList<ICourse>);
    procedure SetHost(const Value: string);
    procedure SetId(const aId: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
    property aLMSConnection: TLMSRestMoodle read GetLMSConnection;
    property AutoConnect: boolean read GetAutoConnect write SetAutoConnect;
    property Categories: TList<ICategory> read GetCategories
      write SetCategories;
    property FlatCourses: TList<ICourse> read GetFlatCourses
      write SetFlatCourses;
    property Host: string read GetHost write SetHost;
    property Id: string read getId write SetId;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property User: string write SetUser;
  end;

implementation

end.

unit LMS._interface.LMS;

interface

uses
  LMS.Rest.Moodle;

type
  ILMS = interface
    ['{F67CAF42-24D9-4906-BE7A-09E9A669D9F1}']
    function GetLMSConnection: TLMSRestMoodle;

    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetService(const Value: string);
    procedure SetUser(const Value: string);
    function GetHost: string;

    procedure MyOnFunctionNotAdded(Sender: TLMSRestMoodle;
      const aFunctionName: string);

    // All LMS categories
    // categories: TList<TLMSCategory>;
    // All LMS courses
    // courses: TList<TLMSCourse>;

    procedure Connect;
    function connected: boolean;

    function FirstLevelCategoriesCount: cardinal;
    // function GetCategoryById(Id: cardinal): TLMSCategory;
    // function GetUsersByAlmostAllFields(var aLMSUsers: TLMSUsers;      const aFilter: string): integer;

    procedure GetCategories;
    procedure GetCourses;

    property User: string write SetUser;
    property Password: string write SetPassword;
    property Service: string write SetService;
    property Host: string read GetHost write SetHost;

    property aLMSConnection: TLMSRestMoodle read GetLMSConnection;
  end;

implementation

end.

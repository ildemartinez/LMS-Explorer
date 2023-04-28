unit LMS._class.Course;

interface

uses
  System.Classes,
  Generics.Collections,

  LMS._interface.LMS;

type
  TLMSCourse = class(TInterfacedObject, ICourse)
  strict private
    fdisplayname: string;
    fEndDate: TDateTime;
    fFullName: string;
    fGradeItems: TList<IGradeItem>;
    fGroupMode: cardinal;
    fid: cardinal;
    fLMS: ILMS;
    fSections: TList<ISection>;
    fshortname: string;
    fStartDate: TDateTime;
    fTimeCreated: TDateTime;
    fTimeModified: TDateTime;
    // All course groups
    fUserGroups: TList<IUsersGroup>;
    // All users enrolled in this course
    fUsers: TList<IUser>;
    function GetCategory: ICategory;
    function GetDisplayContent: string;
    function GetDisplayName: string;
    function GetEndDate: TDateTime;
    function getFilterContent: string;
    function GetFullName: string;
    function GetGradeItems: TList<IGradeItem>;
    function GetGroupMode: cardinal;
    function getId: cardinal;
    function GetLMS: ILMS;
    function GetSections: TList<ISection>;
    function GetShortName: string;
    function GetStartDate: TDateTime;
    function GetTimeCreated: TDateTime;
    function GetTimeModified: TDateTime;
    function GetUserGroups: TList<IUsersGroup>;
    function GetUsers: TList<IUser>;
    procedure RefreshUserGroups;
    procedure SetDisplayName(const Value: string);
    procedure SetEndDate(const Value: TDateTime);
    procedure SetFullName(const Value: string);
    procedure SetGroupMode(const Value: cardinal);
    procedure SetId(const Value: cardinal);
    procedure SetShortName(const Value: string);
    procedure SetStartDate(const Value: TDateTime);
    procedure SetTimeCreated(const Value: TDateTime);
    procedure SetTimeModified(const Value: TDateTime);
    procedure SetUserGroups(const Value: TList<IUsersGroup>);
    procedure SetUsers(const Value: TList<IUser>);
  public
    constructor Create(const LMS: ILMS);
    destructor Destroy; override;
    procedure GetCourseContent;
    procedure GetCourseRoles(aCourseRoles: TStringlist);
    procedure GetGradeBook;
    function GetUserCountByRol(const aRole: string): cardinal;
    procedure RefreshEnrolledUsers;
    // Returns the apropiated text to show information about courses
    property DisplayContent: string read GetDisplayContent;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property End_Date: TDateTime read GetEndDate write SetEndDate;
    // Return the course information that can be filtered from
    property FilterContent: string read getFilterContent;
    property FullName: string read GetFullName write SetFullName;
    property GroupMode: cardinal read GetGroupMode write SetGroupMode;
    property Id: cardinal read getId write SetId;
    // Pointer to the LMS parent
    property LMS: ILMS read GetLMS;
    property shortname: string read GetShortName write SetShortName;
    property Start_Date: TDateTime read GetStartDate write SetStartDate;
    property Time_Created: TDateTime read GetTimeCreated write SetTimeCreated;
    property Time_Modified: TDateTime read GetTimeModified
      write SetTimeModified;
    // All course groups
    property UserGroups: TList<IUsersGroup> read GetUserGroups
      write SetUserGroups;
    property Users: TList<IUser> read GetUsers write SetUsers;
  end;

implementation

uses
  System.JSON,
  System.SysUtils,

  LMS.Helper.Log,
  LMS._class.GradeItem,
  LMS._class.User,
  LMS._class.Section,
  LMS._class.Module,
  LMS._class.Content,
  LMS._class.UsersGroup;

constructor TLMSCourse.Create(const LMS: ILMS);
begin
  inherited Create;

  fUsers := TList<IUser>.Create;
  fUserGroups := TList<IUsersGroup>.Create;
  fGradeItems := TList<IGradeItem>.Create;
  fSections := TList<ISection>.Create;

  fLMS := LMS;
end;

destructor TLMSCourse.Destroy;
begin
  fSections.free;
  fGradeItems.free;
  fUserGroups.free;
  fUsers.free;

  inherited;
end;

function TLMSCourse.GetCategory: ICategory;
var
  cat: ICategory;
  cour: ICourse;
begin
  result := nil;

  for cat in fLMS.Categories do
  begin
    for cour in cat.Courses do

      if (cour.Id = Id) then
      begin
        result := cat;
        break;
      end;
  end;
end;

procedure TLMSCourse.GetCourseContent;
var
  aSectionItems, aModuleItems, aContentItems: TJSonArray;
  aSection: ISection;
  aModule: IModule;
  aContent: IContent;
  aJsonSection, aJsonModule, aJsonContent: TJSONValue;
  aContentsInfo: TJsonObject;
  aMimeType: string;
begin
  fSections.clear;

  aSectionItems := fLMS.GetLMSConnection.GetCourseContent(fid);

  for aJsonSection in aSectionItems do
  begin
    aSection := TSection.Create(self);

    aSection.Name := aJsonSection.GetValue<string>('name');
    aModuleItems := aJsonSection.GetValue<TJSonArray>('modules');

    for aJsonModule in aModuleItems do
    begin
      aModule := tmodule.Create(aSection);
      aModule.Name := aJsonModule.GetValue<string>('name');
      aModule.ModName := aJsonModule.GetValue<string>('modname');

      if aJsonModule.TryGetValue<TJsonObject>('contentsinfo', aContentsInfo)
      then
      begin
        // here we have the all contents info
      end;

      if aJsonModule.TryGetValue<TJSonArray>('contents', aContentItems) then
        for aJsonContent in aContentItems do
        begin
          aContent := TContent.Create(aModule);

          aContent.FileName := aJsonContent.GetValue<string>('filename');
          if aJsonContent.TryGetValue<string>('mimetype', aMimeType) then
            aContent.MimeType := aMimeType;

          aContent.fileurl := aJsonContent.GetValue<string>('fileurl');

          aModule.Contents.add(aContent);
        end;

      aSection.Modules.add(aModule);
    end;

    fSections.add(aSection);

  end;

  // log(aSectionItems.ToString);
end;

procedure TLMSCourse.GetCourseRoles(aCourseRoles: TStringlist);
var
  aUser: IUser;
begin
  for aUser in fUsers do
    if aCourseRoles.IndexOf(aUser.Roles) < 0 then
      aCourseRoles.add(aUser.Roles);
end;

function TLMSCourse.GetDisplayContent: string;
begin
  result := format('%s - %s - (%d)', [shortname, DisplayName, Id]);
end;

function TLMSCourse.GetDisplayName: string;
begin
  result := fdisplayname;
end;

function TLMSCourse.GetEndDate: TDateTime;
begin
  result := fEndDate;
end;

function TLMSCourse.getFilterContent: string;
begin
  result := shortname + ' ' + FullName + ' ' + DisplayName + ' ' + Id.ToString;
end;

function TLMSCourse.GetFullName: string;
begin
  result := fFullName;
end;

procedure TLMSCourse.GetGradeBook;
var
  aUsers, aGradeItems: TJSonArray;
  aUser, aGradeItem: TJSONValue;
  aGradeItemC: TGradeItem;
begin
  fGradeItems.clear;

  aGradeItems := fLMS.GetLMSConnection.GetUsersGradeBook(self.fid);
end;

function TLMSCourse.GetGradeItems: TList<IGradeItem>;
begin
  result := fGradeItems;
end;

function TLMSCourse.GetGroupMode: cardinal;
begin
  result := fGroupMode;
end;

function TLMSCourse.getId: cardinal;
begin
  result := fid;
end;

function TLMSCourse.GetLMS: ILMS;
begin
  result := fLMS;
end;

function TLMSCourse.GetSections: TList<ISection>;
begin
  result := fSections;
end;

function TLMSCourse.GetShortName: string;
begin
  result := fshortname;
end;

function TLMSCourse.GetStartDate: TDateTime;
begin
  result := fStartDate;
end;

function TLMSCourse.GetTimeCreated: TDateTime;
begin
  result := fTimeCreated;
end;

function TLMSCourse.GetTimeModified: TDateTime;
begin
  result := fTimeModified;
end;

function TLMSCourse.GetUserCountByRol(const aRole: string): cardinal;
var
  aUser: IUser;
begin
  result := 0;
  for aUser in fUsers do
  begin
    if aUser.Roles = aRole then
      inc(result);
  end;
end;

function TLMSCourse.GetUserGroups: TList<IUsersGroup>;
begin
  result := fUserGroups;
end;

function TLMSCourse.GetUsers: TList<IUser>;
begin
  result := fUsers;
end;

procedure TLMSCourse.RefreshEnrolledUsers;
var
  aUser: IUser;
  aUsers: TJSonArray;
  User: TJSONValue;

  groups: TJSonArray;
  Group: TJSONValue;

  rol: TJSONValue;
  arealrol: string;
begin
  // Populate user groups first
  RefreshUserGroups;

  fUsers.clear;

  aUsers := fLMS.aLMSConnection.GetEnrolledUsersByCourseId(self.Id);

  if aUsers <> nil then
  begin
    // log(aUsers.ToString);
    for User in aUsers do
    begin
      aUser := TUser.Create(fLMS, User);
      aUser.Course := self;

      // Get user roles
      for rol in User.GetValue<TJSonArray>('roles') do
      begin
        arealrol := rol.GetValue<string>('name');
        if arealrol = '' then
          aUser.Roles := aUser.Roles + rol.GetValue<string>('shortname')
        else
          aUser.Roles := aUser.Roles + arealrol;
      end;

      // Add user to users list
      fUsers.add(aUser);

      // Include the user in the corresponding group
      groups := User.GetValue<TJSonArray>('groups');

      if groups <> nil then
      begin
        // log(groups.ToString);
        for Group in groups do
        begin
          for var agroup in self.fUserGroups do
          begin
            if agroup.Id = Group.GetValue<cardinal>('id') then
              agroup.UsersInGroup.add(aUser)
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
  aUserGroup: IUsersGroup;
begin
  fUserGroups.clear;

  aUserGroups := fLMS.aLMSConnection.GetUserGroupsByCourseId(self.Id);

  if aUserGroups <> nil then
  begin
    // log(aUserGroups.ToString);
    for agroup in aUserGroups do
    begin
      aUserGroup := TUsersGroup.Create;

      aUserGroup.Id := agroup.GetValue<cardinal>('id');
      aUserGroup.Group_Name := agroup.GetValue<string>('name');
      { aUser.fFirstName := User.GetValue<string>('firstname');
        aUser.fLastName := User.GetValue<string>('lastname');
        aUser.fFullName := User.GetValue<string>('fullname');
      }
      fUserGroups.add(aUserGroup)
    end;
  end;

end;

procedure TLMSCourse.SetDisplayName(const Value: string);
begin
  fdisplayname := Value;
end;

procedure TLMSCourse.SetEndDate(const Value: TDateTime);
begin
  fEndDate := Value;
end;

procedure TLMSCourse.SetFullName(const Value: string);
begin
  fFullName := Value;
end;

procedure TLMSCourse.SetGroupMode(const Value: cardinal);
begin
  fGroupMode := Value;
end;

procedure TLMSCourse.SetId(const Value: cardinal);
begin
  fid := Value;
end;

procedure TLMSCourse.SetShortName(const Value: string);
begin
  fshortname := Value;
end;

procedure TLMSCourse.SetStartDate(const Value: TDateTime);
begin
  fStartDate := Value;
end;

procedure TLMSCourse.SetTimeCreated(const Value: TDateTime);
begin
  fTimeCreated := Value;
end;

procedure TLMSCourse.SetTimeModified(const Value: TDateTime);
begin
  fTimeModified := Value;
end;

procedure TLMSCourse.SetUserGroups(const Value: TList<IUsersGroup>);
begin
  fUserGroups := Value;
end;

procedure TLMSCourse.SetUsers(const Value: TList<IUser>);
begin
  fUsers := Value;
end;

end.

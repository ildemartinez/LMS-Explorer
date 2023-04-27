unit LMS.Rest.Moodle;

interface

uses
  System.Classes,
  Rest.Client,
  System.JSON;

type

  TLMSRestMoodle = class;

  TFunctionNotAddedNotifyEvent = procedure(Sender: TLMSRestMoodle;
    const aFunctionName: string) of object;

  TLMFunctionRequest = class(TRestRequest)
  public
    constructor Create(Owner: TComponent); override;
  end;

  TLMSRestMoodle = class(TComponent)
  private
    fuser: string;
    fpassword: string;
    fservice: string;
    fhost: string;
    fToken: string;

    aRestClient: TRestClient;
    aRestRequest: TLMFunctionRequest;
    arestresponse: TRESTResponse;
    fFunctionNotAddedNotifyEvent: TFunctionNotAddedNotifyEvent;

    procedure PrepareParams(const servicefunction: string);
    function ExecuteRequest(const servicefunction: string): TJSonArray;
    function ExecuteRequest2(const servicefunction: string): TJSonValue;
  public
    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function Connected: boolean;

    function GetCategories: TJSonArray;
    function GetCourses: TJSonArray;
    function GetEnrolledUsersByCourseId(const courseID: integer): TJSonArray;
    function GetUserGroupsByCourseId(const courseID: integer): TJSonArray;

    function GetUsersGradeBook(const courseID: integer): TJSonArray;
    function GetCourseContent(const courseID: integer): TJSonArray;

    function GetUsersByFirstName(const aValue: string): TJSonArray;
    function GetUsersByLastName(const aValue: string): TJSonArray;
    function GetUsersByEmail(const aValue: string): TJSonArray;

    procedure DownloadContent(const aFilename: string; const path: string);

    property User: string write fuser;
    property Password: string write fpassword;
    property Service: string write fservice;
    property Host: string read fhost write fhost;

    property OnFunctionNotAdded: TFunctionNotAddedNotifyEvent
      read fFunctionNotAddedNotifyEvent write fFunctionNotAddedNotifyEvent;
  end;

implementation

uses
  forms,
  sysutils,
  System.Generics.Collections,
  Dialogs,
  Rest.Types,
  System.UITypes,
  urlmon,

  LMS.Helper.Consts,
  LMS.Helper.Log,
  LMS.Form.UserPassword;

{ TLMSRestMoodle }

procedure TLMSRestMoodle.Connect;
var
  jValue: TJSonValue;
  aItem: TRESTRequestParameter;
  aerrorcode: string;
begin
  var
    ask: boolean := false;

  if not Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'service';
    aItem.value := fservice;

    if not((fpassword <> '') and (fuser <> '')) then
    begin
      var
        LMSUserPasswordForm: TLMSUserPasswordForm :=
          TLMSUserPasswordForm.Create(application);

      LMSUserPasswordForm.username := fuser;
      LMSUserPasswordForm.Password := fpassword;

      if LMSUserPasswordForm.ShowModal = mrOk then
      begin
        fuser := LMSUserPasswordForm.username;
        fpassword := LMSUserPasswordForm.Password;
        // if (fpassword <> '') and (fuser <> '') then // test it in the form close modal
        ask := true
      end;

      LMSUserPasswordForm.free;
    end
    else
      ask := true;

    if ask = true then
    begin

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'username';
      aItem.value := fuser;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'password';
      aItem.value := fpassword;
      aRestClient.BaseURL := fhost + '/login/token.php';

      try
        aRestRequest.Execute;
        jValue := arestresponse.JSONValue;
        // log(jValue.ToString);

        if jValue.TryGetValue<string>('errorcode', aerrorcode) then
        begin
          if aerrorcode = 'enablewsdescription' then
            logerror(jValue.GetValue<string>('error') + ' fix at ' + fhost +
              '/admin/search.php?query=enablewebservices')
          else if aerrorcode = 'servicenotavailable' then
            logerror(jValue.GetValue<string>('error') + ' maybe ' + fservice +
              ' is not enabled or not exists')
          else
            logerror(aerrorcode);

        end
        else
          fToken := jValue.GetValue<string>('token');

      except
        On E: ERestException do
        begin
          Log('Error ' + aRestClient.BaseURL);
        end
      end;
    end;
  end;
end;

function TLMSRestMoodle.Connected: boolean;
begin
  result := fToken <> '';
end;

constructor TLMSRestMoodle.Create(Owner: TComponent);
begin
  inherited;

  aRestClient := TRestClient.Create(self);
  aRestRequest := TLMFunctionRequest.Create(self);
  arestresponse := TRESTResponse.Create(self);

  aRestRequest.Client := aRestClient;
  aRestRequest.Response := arestresponse;
end;

procedure TLMSRestMoodle.DownloadContent(const aFilename: string;
  const path: string);
begin
  Log(aFilename + ' ' + path);
  URLDownloadToFile(nil, pchar(aFilename + '&token=' + self.fToken),
    pchar(path), 0, nil);
end;

function TLMSRestMoodle.ExecuteRequest(const servicefunction: string)
  : TJSonArray;
var
  jValue: TJSonValue;
  err: string;
begin
  result := nil;

  try
    screen.Cursor := crHourGlass;
    aRestRequest.Execute;

    jValue := arestresponse.JSONValue;

    if jValue.TryGetValue<string>('exception', err) = true then
    begin
      if assigned(fFunctionNotAddedNotifyEvent) then
        fFunctionNotAddedNotifyEvent(self, servicefunction);
    end
    else
      result := jValue as TJSonArray;

  finally
    screen.Cursor := crDefault;
  end;
end;

function TLMSRestMoodle.ExecuteRequest2(const servicefunction: string)
  : TJSonValue;
var
  jValue: TJSonValue;
  err: string;
begin
  result := nil;

  try
    screen.Cursor := crHourGlass;
    aRestRequest.Execute;

    jValue := arestresponse.JSONValue;

    if jValue.TryGetValue<string>('exception', err) = true then
    begin
      if assigned(fFunctionNotAddedNotifyEvent) then
        fFunctionNotAddedNotifyEvent(self, servicefunction);
    end
    else
      result := jValue;

  finally
    screen.Cursor := crDefault;
  end;

end;

function TLMSRestMoodle.GetCategories: TJSonArray;
begin
  result := nil;

  if not Connected then
    Connect;

  if Connected then
  begin
    PrepareParams(CORE_COURSE_GET_CATEGORIES);
    result := ExecuteRequest(CORE_COURSE_GET_CATEGORIES);
  end

  // GetCourses;

  { "id":1,"name":"Miscellaneous","idnumber":null,"description":"","descriptionformat":1,"parent":0,"sortorder":10000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670251074,"depth":1,"path":"\/1","theme":"" }
  { "id":2,"name":"Category1","idnumber":"","description":"","descriptionformat":1,"parent":0,"sortorder":20000,"coursecount":1,"visible":1,"visibleold":1,"timemodified":1670279392,"depth":1,"path":"\/2","theme":"" }
  { "id":4,"name":"SubCategory1.1","idnumber":"","description":"","descriptionformat":1,"parent":2,"sortorder":30000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670279419,"depth":2,"path":"\/2\/4","theme":"" }
  { "id":3,"name":"Category2","idnumber":"","description":"","descriptionformat":1,"parent":0,"sortorder":40000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670279366,"depth":1,"path":"\/3","theme":"" }

end;

function TLMSRestMoodle.GetCourseContent(const courseID: integer): TJSonArray;
begin
  if Connected then
  begin
    PrepareParams(CORE_COURSE_GET_CONTENTS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'courseid';
      value := inttostr(courseID);
    end;

    result := ExecuteRequest(CORE_COURSE_GET_CONTENTS);
  end
  else
    result := nil;

end;

function TLMSRestMoodle.GetCourses: TJSonArray;
begin

  if Connected then
  begin
    PrepareParams(CORE_COURSE_GET_COURSES);
    result := ExecuteRequest(CORE_COURSE_GET_COURSES);
  end
  else
    result := nil;

end;

constructor TLMFunctionRequest.Create(Owner: TComponent);
begin
  inherited;

end;

function TLMSRestMoodle.GetEnrolledUsersByCourseId(const courseID: integer)
  : TJSonArray;
begin

  if Connected then
  begin
    PrepareParams(CORE_ENROL_GET_ENROLLED_USERS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'courseid';
      value := inttostr(courseID);
    end;

    result := ExecuteRequest(CORE_ENROL_GET_ENROLLED_USERS);
  end
  else
    result := nil;

end;

function TLMSRestMoodle.GetUserGroupsByCourseId(const courseID: integer)
  : TJSonArray;
begin

  if Connected then
  begin
    PrepareParams(CORE_GROUP_GET_COURSE_GROUPS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'courseid';
      value := inttostr(courseID);
    end;

    result := ExecuteRequest(CORE_GROUP_GET_COURSE_GROUPS);
  end
  else
    result := nil;

end;

function TLMSRestMoodle.GetUsersByFirstName(const aValue: string): TJSonArray;
var
  jValue: TJSonValue;

begin

  if Connected then
  begin
    PrepareParams(CORE_USER_GET_USERS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'criteria[0][key]';
      value := 'firstname';
    end;

    with aRestRequest.Params.AddItem do
    begin
      name := 'criteria[0][value]';
      value := aValue;
    end;

    jValue := ExecuteRequest2(CORE_USER_GET_USERS);
    jValue := jValue.GetValue<TJSonArray>('users');

    result := jValue as TJSonArray;
  end
  else
    result := nil;

end;

function TLMSRestMoodle.GetUsersByLastName(const aValue: string): TJSonArray;
var
  jValue: TJSonValue;

begin

  if Connected then
  begin
    PrepareParams(CORE_USER_GET_USERS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'criteria[0][key]';
      value := 'lastname';
    end;

    with aRestRequest.Params.AddItem do
    begin
      name := 'criteria[0][value]';
      value := aValue;
    end;

    jValue := ExecuteRequest2(CORE_USER_GET_USERS);
    jValue := jValue.GetValue<TJSonArray>('users');

    result := jValue as TJSonArray;
  end
  else
    result := nil;

end;

function TLMSRestMoodle.GetUsersGradeBook(const courseID: integer): TJSonArray;
var
  jValue: TJSonValue;

begin

  if Connected then
  begin
    PrepareParams(GRADEREPORT_USER_GET_GRADE_ITEMS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'courseid';
      value := courseID.ToString;
    end;

    jValue := ExecuteRequest2(GRADEREPORT_USER_GET_GRADE_ITEMS);
    jValue := jValue.GetValue<TJSonArray>('usergrades');

    result := jValue as TJSonArray;

    // Log(result.ToString);
  end
  else
    result := nil;

  //
end;

function TLMSRestMoodle.GetUsersByEmail(const aValue: string): TJSonArray;
var
  jValue: TJSonValue;

begin

  if Connected then
  begin
    PrepareParams(CORE_USER_GET_USERS);

    with aRestRequest.Params.AddItem do
    begin
      name := 'criteria[0][key]';
      value := 'email';
    end;

    with aRestRequest.Params.AddItem do
    begin
      name := 'criteria[0][value]';
      value := aValue;
    end;

    jValue := ExecuteRequest2(CORE_USER_GET_USERS);
    jValue := jValue.GetValue<TJSonArray>('users');

    result := jValue as TJSonArray;
  end
  else
    result := nil;

end;

procedure TLMSRestMoodle.PrepareParams(const servicefunction: string);
begin
  aRestRequest.Params.Clear;

  with aRestRequest.Params.AddItem do
  begin
    name := WSTOKEN;
    value := self.fToken;
  end;

  with aRestRequest.Params.AddItem do
  begin
    name := WSFUNCTION;
    value := servicefunction;
  end;

  with aRestRequest.Params.AddItem do
  begin
    name := 'moodlewsrestformat';
    value := 'json';
  end;

  aRestClient.BaseURL := fhost + '/webservice/rest/server.php';
end;

end.

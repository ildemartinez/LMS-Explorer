unit LMSRestMoodleUnit;

interface

uses
  System.Classes,
  rest.Client,
  System.JSON;

type
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
  public
    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function Connected: boolean;

    function GetCategories: TJSonArray;
    function GetCourses: TJSonArray;
    function GetEnrolledUsersByCourseId(const courseID: integer): TJSonArray;
    function GetUserGroupsByCourseId(const courseID: integer): TJSonArray;

    property User: string write fuser;
    property Password: string write fpassword;
    property Service: string write fservice;
    property Host: string read fhost write fhost;

  end;

implementation

uses
  forms, sysutils,
  System.Generics.Collections,
  Dialogs,
  rest.Types,
  System.UITypes,

  LMSUserPasswordFormUnit,
  LMSConstsUnit,
  LMSLogUnit;

{ TLMSRestMoodle }

procedure TLMSRestMoodle.Connect;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
begin
  var
    ask: boolean := false;

  if not Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'service';
    aItem.Value := fservice;

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
      aItem.Value := fuser;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'password';
      aItem.Value := fpassword;
      aRestClient.BaseURL := fhost + '/login/token.php';

      try
        aRestRequest.Execute;
        jValue := arestresponse.JSONValue;
        fToken := jValue.GetValue<string>('token');

      except
        On E: ERestException do
        begin
          log('Error ' + aRestClient.BaseURL);
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

function TLMSRestMoodle.GetCategories: TJSonArray;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
begin
  if not Connected then
    Connect;

  if Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSTOKEN;
    aItem.Value := self.fToken;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSFUNCTION;
    aItem.Value := CORE_COURSE_GET_CATEGORIES;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'moodlewsrestformat';
    aItem.Value := 'json';

    aRestClient.BaseURL := fhost + '/webservice/rest/server.php';

    try
      screen.Cursor := crHourGlass;
      aRestRequest.Execute;
    finally
      screen.Cursor := crDefault;
    end;

    jValue := arestresponse.JSONValue;

    result := jValue as TJSonArray;
  end
  else
    result := nil;

  // GetCourses;

  { "id":1,"name":"Miscellaneous","idnumber":null,"description":"","descriptionformat":1,"parent":0,"sortorder":10000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670251074,"depth":1,"path":"\/1","theme":"" }
  { "id":2,"name":"Category1","idnumber":"","description":"","descriptionformat":1,"parent":0,"sortorder":20000,"coursecount":1,"visible":1,"visibleold":1,"timemodified":1670279392,"depth":1,"path":"\/2","theme":"" }
  { "id":4,"name":"SubCategory1.1","idnumber":"","description":"","descriptionformat":1,"parent":2,"sortorder":30000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670279419,"depth":2,"path":"\/2\/4","theme":"" }
  { "id":3,"name":"Category2","idnumber":"","description":"","descriptionformat":1,"parent":0,"sortorder":40000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670279366,"depth":1,"path":"\/3","theme":"" }

end;

function TLMSRestMoodle.GetCourses: TJSonArray;

var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
begin

  if Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSTOKEN;
    aItem.Value := self.fToken;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSFUNCTION;
    aItem.Value := CORE_COURSE_GET_COURSES;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'moodlewsrestformat';
    aItem.Value := 'json';

    aRestClient.BaseURL := fhost + '/webservice/rest/server.php';

    try
      screen.Cursor := crHourGlass;
      aRestRequest.Execute;
    finally
      screen.Cursor := crDefault;
    end;

    jValue := arestresponse.JSONValue;
    result := jValue as TJSonArray;
  end
  else
    result := nil;

end;

{ TLMFunctionRequest }

constructor TLMFunctionRequest.Create(Owner: TComponent);
begin
  inherited;

end;

function TLMSRestMoodle.GetEnrolledUsersByCourseId(const courseID: integer)
  : TJSonArray;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
begin

  if Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSTOKEN;
    aItem.Value := self.fToken;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSFUNCTION;
    aItem.Value := CORE_ENROL_GET_ENROLLED_USERS;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'moodlewsrestformat';
    aItem.Value := 'json';

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'courseid';
    aItem.Value := inttostr(courseID);

    aRestClient.BaseURL := fhost + '/webservice/rest/server.php';

    try
      screen.Cursor := crHourGlass;
      aRestRequest.Execute;
    finally
      screen.Cursor := crDefault;
    end;

    jValue := arestresponse.JSONValue;
    result := jValue as TJSonArray;
  end
  else
    result := nil;

end;

function TLMSRestMoodle.GetUserGroupsByCourseId(
  const courseID: integer): TJSonArray;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
begin

  if Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSTOKEN;
    aItem.Value := self.fToken;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := WSFUNCTION;
    aItem.Value := CORE_GROUP_GET_COURSE_GROUPS;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'moodlewsrestformat';
    aItem.Value := 'json';

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'courseid';
    aItem.Value := inttostr(courseID);

    aRestClient.BaseURL := fhost + '/webservice/rest/server.php';

    try
      screen.Cursor := crHourGlass;
      aRestRequest.Execute;
    finally
      screen.Cursor := crDefault;
    end;

    jValue := arestresponse.JSONValue;
    result := jValue as TJSonArray;
  end
  else
    result := nil;

end;

end.

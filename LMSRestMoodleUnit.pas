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
    // procedure GetCourses;

    property User: string write fuser;
    property Password: string write fpassword;
    property Service: string write fservice;
    property Host: string write fhost;

  end;

implementation

uses
  System.Generics.Collections,
  Dialogs,
  rest.Types,

  LMSConstsUnit,
  LMSLogUnit;

{ TLMSRestMoodle }

procedure TLMSRestMoodle.Connect;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;

begin

  if not Connected then
  begin
    aRestRequest.Params.Clear;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'username';
    aItem.Value := fuser;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'password';
    aItem.Value := fpassword;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'service';
    aItem.Value := fservice;

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

function TLMSRestMoodle.Connected: boolean;
begin
  result := fToken <> '';
end;

constructor TLMSRestMoodle.Create(Owner: TComponent);
begin
  inherited;

  aRestClient := TRestClient.Create(self);
  aRestClient.ReadTimeout := 1000;

  aRestRequest := TLMFunctionRequest.Create(self);
  arestresponse := TRESTResponse.Create(self);
  aRestRequest.ConnectTimeout := 1000;
  aRestRequest.Timeout := 1000;
  aRestRequest.ReadTimeout := 1000;

  aRestRequest.Client := aRestClient;
  aRestRequest.Response := arestresponse;
end;

function TLMSRestMoodle.GetCategories: TJSonArray;
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
    aItem.Value := CORE_COURSE_GET_CATEGORIES;

    aItem := aRestRequest.Params.AddItem;
    aItem.name := 'moodlewsrestformat';
    aItem.Value := 'json';

    aRestClient.BaseURL := fhost + '/webservice/rest/server.php';

    aRestRequest.Execute;

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

{
  procedure TLMSRestMoodle.GetCourses;
  var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
  aCategory: TLMSCategory;
  aCategories: TJSonArray;
  k: Integer;

  begin
  if Owner is tlms then
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

  aRestClient.BaseURL := tlms(Owner).url + '/webservice/rest/server.php';

  aRestRequest.Execute;

  jValue := arestresponse.JSONValue;

  aCategories := jValue as TJSonArray;

  log(aCategories.ToString);

  exit;
  for k := 0 to aCategories.Count - 1 do
  begin
  aCategory := TLMSCategory.Create;
  aCategory.id := aCategories[k].GetValue<cardinal>('id');
  aCategory.name := aCategories[k].GetValue<string>('name');
  aCategory.fparent := aCategories[k].GetValue<cardinal>('parent');
  tlms(Owner).categories.Add(aCategory);
  end;

  end;
  end;

  end;
}
{ TLMFunctionRequest }

constructor TLMFunctionRequest.Create(Owner: TComponent);
begin
  inherited;

end;

end.

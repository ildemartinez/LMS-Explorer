unit LMSRestMoodleUnit;

interface

uses
  System.Classes, rest.Client;

type
  TLMSRestMoodle = class(TComponent)
  private
    fToken: string;

    aRestClient: TRestClient;
    aRestRequest: trestrequest;
    arestresponse: TRESTResponse;
  public
    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function Connected: boolean;

    procedure GetCategories;

  end;

implementation

uses
  lmsnetworkunit, System.JSON, dialogs,

  LMSLogUnit;

{ TLMSRestMoodle }

procedure TLMSRestMoodle.Connect;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;

begin
  if Owner is tlms then
  begin

    if not Connected then
    begin
      aRestRequest.Params.Clear;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'username';
      aItem.Value := tlms(Owner).user;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'password';
      aItem.Value := tlms(Owner).password;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'service';
      aItem.Value := tlms(Owner).service;

      aRestClient.BaseURL := tlms(Owner).url + '/login/token.php';
      aRestRequest.Execute;

      jValue := arestresponse.JSONValue;
      fToken := jValue.GetValue<string>('token');
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

  aRestRequest := trestrequest.Create(self);
  arestresponse := TRESTResponse.Create(self);

  aRestRequest.Client := aRestClient;
  aRestRequest.Response := arestresponse;
end;

procedure TLMSRestMoodle.GetCategories;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;
  aCategory: TLMSCategory;
  aCategories: TJSonArray;

begin
  if Owner is tlms then
  begin

    // if Connected then
    begin
      aRestRequest.Params.Clear;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'wstoken';
      aItem.Value := self.fToken;

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'wsfunction';
      aItem.Value := 'core_course_get_categories';

      aItem := aRestRequest.Params.AddItem;
      aItem.name := 'moodlewsrestformat';
      aItem.Value := 'json';

      aRestClient.BaseURL := tlms(Owner).url + '/webservice/rest/server.php';
      aRestRequest.Execute;

      jValue := arestresponse.JSONValue;

      aCategories := jValue as TJSonArray;

      log(aCategories.ToString);

      aCategory := TLMSCategory.Create;
      aCategory.fparent := 0;
      aCategory.id := 1;
      tlms(Owner).categories.Add(aCategory);

      aCategory := TLMSCategory.Create;
      aCategory.fparent := 0;
      aCategory.id := 2;
      tlms(Owner).categories.Add(aCategory);

            aCategory := TLMSCategory.Create;
      aCategory.fparent := 0;
      aCategory.id := 3;
      tlms(Owner).categories.Add(aCategory);

      aCategory := TLMSCategory.Create;
      aCategory.fparent := 2;
      aCategory.id := 4;
      tlms(Owner).categories.Add(aCategory);

    end;
  end;

  // fToken := jValue.GetValue<string>('token');

  { for var k := 0 to aCategories.Count-1 do
    begin
    aCategory := TLMSCategory.Create;

    tlms(Owner).categories.Add(aCategory)
    end;
    end;
    end;
  }
  { "id":1,"name":"Miscellaneous","idnumber":null,"description":"","descriptionformat":1,"parent":0,"sortorder":10000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670251074,"depth":1,"path":"\/1","theme":"" }
  { "id":2,"name":"Category1","idnumber":"","description":"","descriptionformat":1,"parent":0,"sortorder":20000,"coursecount":1,"visible":1,"visibleold":1,"timemodified":1670279392,"depth":1,"path":"\/2","theme":"" }
  { "id":4,"name":"SubCategory1.1","idnumber":"","description":"","descriptionformat":1,"parent":2,"sortorder":30000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670279419,"depth":2,"path":"\/2\/4","theme":"" }
  { "id":3,"name":"Category2","idnumber":"","description":"","descriptionformat":1,"parent":0,"sortorder":40000,"coursecount":0,"visible":1,"visibleold":1,"timemodified":1670279366,"depth":1,"path":"\/3","theme":"" }

end;

end.

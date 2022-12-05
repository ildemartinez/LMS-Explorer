unit LMSRestMoodleUnit;

interface

uses
  System.Classes, rest.Client;

type
  TLMSRestMoodle = class(TComponent)
  private
    aRestClient: TRestClient;
    aRestRequest: trestrequest;
    arestresponse: TRESTResponse;
  public
    constructor Create(Owner: TComponent); override;

    procedure Connect;

  end;

implementation

uses
  lmsnetworkunit, System.JSON, dialogs;

{ TLMSRestMoodle }

procedure TLMSRestMoodle.Connect;
var
  jValue: TJsonValue;
  aItem: TRESTRequestParameter;

begin
  if Owner is tlms then
  begin

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
      showmessage(jValue.ToString);
    end;
  end;
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

end.

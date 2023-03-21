unit LMS.Form.User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  LMSNetworkUnit
  ;

type
  TLMSUserForm = class(TForm)
  private
    fLMSUser: TLMSUser;
    procedure SetLMSUser(const Value: TLMSUser);
    { Private declarations }
  public
    { Public declarations }
    property aUser : TLMSUser read fLMSUser write SetLMSUser;
  end;

var
  LMSUserForm: TLMSUserForm;

implementation

{$R *.dfm}

{ TLMSUserForm }

procedure TLMSUserForm.SetLMSUser(const Value: TLMSUser);
begin
  fLMSUser := Value;

  caption := fLMSUser.Full_Name;
end;

end.

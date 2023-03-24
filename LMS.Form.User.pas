unit LMS.Form.User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  LMSNetworkUnit;

type
  TLMSUserForm = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fLMSUser: TLMSUser;
    procedure SetLMSUser(const Value: TLMSUser);
  public
    property aUser: TLMSUser read fLMSUser write SetLMSUser;
  end;

implementation

{$R *.dfm}

procedure TLMSUserForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSUserForm.SetLMSUser(const Value: TLMSUser);
begin
  fLMSUser := Value;

  caption := fLMSUser.Full_Name;
end;

end.

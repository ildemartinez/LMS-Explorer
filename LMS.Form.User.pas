unit LMS.Form.User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  LMS._interface.LMS,
  LMSNetworkUnit;

type
  TLMSUserForm = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fLMSUser: IUser;

    procedure SetLMSUser(const Value: IUser);
  public
    property User: IUser read fLMSUser write SetLMSUser;
  end;

implementation

{$R *.dfm}

procedure TLMSUserForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSUserForm.SetLMSUser(const Value: IUser);
begin
  fLMSUser := Value;

  caption := fLMSUser.Full_Name;
end;

end.

unit LMSUserPasswordFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus;

type
  TLMSUserPasswordForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    EdUserName: TEdit;
    EdPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetPassword: string;
    function GetUserName: string;
    procedure SetPassword(const Value: string);
    procedure SetUsername(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner : TComponent); override;

    property Username : string read GetUserName write SetUsername;
    property Password : string read GetPassword write SetPassword;
  end;


implementation

{$R *.dfm}

{ TLMSUserPasswordForm }

constructor TLMSUserPasswordForm.Create(Owner: TComponent);
begin
  inherited;

  Position := poMainFormCenter;

  EdPassword.PasswordChar := '*';
end;

procedure TLMSUserPasswordForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  //CanClose := false;
end;

function TLMSUserPasswordForm.GetPassword: string;
begin
  result := EdPassword.Text;
end;

function TLMSUserPasswordForm.GetUserName: string;
begin
  result := EdUserName.Text;
end;

procedure TLMSUserPasswordForm.SetPassword(const Value: string);
begin
  EdPassword.Text := Value;
end;

procedure TLMSUserPasswordForm.SetUsername(const Value: string);
begin
  EdUserName.Text := value;
end;

end.

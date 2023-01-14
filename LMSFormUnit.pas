unit LMSFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkUnit, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  LMSUsersTreeViewUnit;

type
  TLMSForm = class(TForm)
    TabControl1: TTabControl;
    Panel1: TPanel;
    Edit1: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit1Change(Sender: TObject);
  private
    fLMS: TLMS;
    fUsers: TLMSUsers;
    fLMSUsersTreeView: TLMSUsersTreeView;
    procedure SetTLMS(const Value: TLMS);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;

    property LMS: TLMS write SetTLMS;
  end;

var
  LMSForm: TLMSForm;

implementation

uses
  System.JSON,

  LMSLogUnit;

{$R *.dfm}

constructor TLMSForm.Create(Owner: TComponent);
begin
  inherited;

  fUsers := TLMSUsers.Create;

  fLMSUsersTreeView := TLMSUsersTreeView.Create(self);
  fLMSUsersTreeView.Parent := TabControl1;
  fLMSUsersTreeView.Align := alClient;
  fLMSUsersTreeView.LMSUsers := fUsers;
end;

procedure TLMSForm.Edit1Change(Sender: TObject);
var
  aUsersCount: integer;
begin
  if Length(Edit1.Text) > 2 then
  begin

    fUsers.Clear;

    aUsersCount := fLMS.GetUsersByFirstName(fUsers, '%' + Edit1.Text + '%');

    if aUsersCount > 0 then
      fLMSUsersTreeView.Refreshh;

    if aUsersCount = 0 then
      fLMSUsersTreeView.Clear;

  end;
end;

procedure TLMSForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSForm.SetTLMS(const Value: TLMS);
begin
  fLMS := Value;

  caption := fLMS.Id;
end;

end.

unit LMSFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkUnit, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  LMSUsersTreeViewUnit, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls;

type
  TLMSForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Button1: TButton;
    ActionManager1: TActionManager;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    ActionToolBar1: TActionToolBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
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

  LMSBrowserHelperUnit,
  LMSLogUnit;

{$R *.dfm}

procedure TLMSForm.Action1Execute(Sender: TObject);
begin
   OpenUsersInBrowser(fLMS);
end;

procedure TLMSForm.Action2Execute(Sender: TObject);
begin
   OpenCreateUserInBrowser(fLMS);
end;

procedure TLMSForm.Action3Execute(Sender: TObject);
begin
   OpenUploadUsersInBrowser(fLMS);
end;

procedure TLMSForm.Button1Click(Sender: TObject);
begin
  OpenExternalServices(self.fLMS);
end;

constructor TLMSForm.Create(Owner: TComponent);
begin
  inherited;

  PageControl1.ActivePageIndex := 0;

  fUsers := TLMSUsers.Create;

  fLMSUsersTreeView := TLMSUsersTreeView.Create(self);
  fLMSUsersTreeView.Parent := TabSheet1;
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

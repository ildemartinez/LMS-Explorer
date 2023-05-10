unit LMS.Form.LMS;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, System.ImageList,
  Vcl.ImgList,

  Generics.Collections,

  LMS.TreeView.Users,
  LMS.TreeView.Courses,
  LMS.TreeView.LMS,
  LMS._interface.LMS,
  LMS._class.LMS;

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
    ImageList1: TImageList;
    Action4: TAction;
    Action5: TAction;
    tsCourses: TTabSheet;
    tsCoursesWithCategories: TTabSheet;
    Button2: TButton;
    ActionToolBar2: TActionToolBar;
    Action6: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit1Change(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure Action4Update(Sender: TObject);
    procedure Action4Execute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
    procedure Action6Execute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fLMS: ILMS;
    fUsers: TList<IUser>;
    fLMSUsersTreeView: TLMSUsersTreeView;
    fLMSCoursesTreeView: TLMSCoursesTreeView;
    fLMSTreeView: TLMSTreeView;

    procedure SetTLMS(const Value: ILMS);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;

    property LMS: ILMS write SetTLMS;
  end;

var
  LMSForm: TLMSForm;

implementation

uses
  System.JSON,

  LMS.Helper.Browser,
  LMS.Helper.Reports;

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

procedure TLMSForm.Action4Execute(Sender: TObject);
begin
  OpenInBrowserByLMS(fLMS, fLMSUsersTreeView.SelectedUser);
end;

procedure TLMSForm.Action4Update(Sender: TObject);
begin
  Action4.enabled := fLMSUsersTreeView.SelectedUser <> nil;
end;

procedure TLMSForm.Action5Execute(Sender: TObject);
begin
  OpenEditProfileByLMS(fLMS, fLMSUsersTreeView.SelectedUser);
end;

procedure TLMSForm.Button1Click(Sender: TObject);
begin
  OpenExternalServices(fLMS);
end;

procedure TLMSForm.Button2Click(Sender: TObject);
begin
  OpenWebServiceDocumentation(fLMS);
end;

constructor TLMSForm.Create(Owner: TComponent);
begin
  inherited;

  PageControl1.ActivePageIndex := 0;

  fUsers := TList<IUser>.Create;

  fLMSUsersTreeView := TLMSUsersTreeView.Create(self);
  fLMSUsersTreeView.Parent := TabSheet1;
  fLMSUsersTreeView.Align := alClient;
  fLMSUsersTreeView.LMSUsers := fUsers;

  fLMSCoursesTreeView := TLMSCoursesTreeView.Create(self);
  fLMSCoursesTreeView.Parent := tsCourses;
  fLMSCoursesTreeView.Align := alClient;

  fLMSTreeView := TLMSTreeView.Create(self);
  fLMSTreeView.Parent := tsCoursesWithCategories;
  fLMSTreeView.Align := alClient;
  // fLMSCoursesTreeView.LMSUsers := fUsers;

end;

procedure TLMSForm.Action6Execute(Sender: TObject);
begin
exporttoexcelCourses(fLMS);
end;

procedure TLMSForm.Edit1Change(Sender: TObject);
var
  aUsersCount: integer;
begin
  if Length(Edit1.Text) > 5 then
  begin

    fUsers.Clear;

    aUsersCount := fLMS.GetUsersByAlmostAllFields(fUsers,
      '%' + Edit1.Text + '%');

    if aUsersCount > 0 then
      fLMSUsersTreeView.Refreshh;

    if aUsersCount = 0 then
      fLMSUsersTreeView.Clear;

  end
  else
  begin
    fLMSUsersTreeView.Clear;
  end;
end;

procedure TLMSForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSForm.SetTLMS(const Value: ILMS);
begin
  fLMS := Value;

  fLMSCoursesTreeView.LMS := fLMS;
  fLMSTreeView.LMS := fLMS;

  caption := fLMS.Id;
end;

// initialization
// ReportMemoryLeaksOnShutdown := True;
end.

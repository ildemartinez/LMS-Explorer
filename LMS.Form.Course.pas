unit LMS.Form.Course;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkUnit, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls,
  Vcl.ActnMenus, System.ImageList, Vcl.ImgList,

  LMS._interface.LMS,
  LMS.TreeView.CourseUsers;

type
  TLMSCourseForm = class(TForm)
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    Panel2: TPanel;
    TabControl1: TTabControl;
    Panel1: TPanel;
    Edit1: TEdit;
    Panel3: TPanel;
    Memo1: TMemo;
    Action6: TAction;
    ImageList1: TImageList;
    Action7: TAction;
    Action8: TAction;
    actExport: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure Action4Execute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
    procedure Action4Update(Sender: TObject);
    procedure Action3Update(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Action6Execute(Sender: TObject);
    procedure Action7Update(Sender: TObject);
    procedure Action7Execute(Sender: TObject);
    procedure Action8Update(Sender: TObject);
    procedure Action8Execute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
  private
    fUsersTreeView: TLMSCourseUsersTreeView;
    fCourse: ICourse;
    procedure SetaCourse(const Value: ICourse);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    property aCourse: ICourse read fCourse write SetaCourse;

  end;

implementation

uses
  Vcl.Clipbrd,
  ShellApi,

  LMS.Helper.Consts,
  LMS.Helper.Browser,
  LMS.Helper.Reports,
  LMS.Helper.Log;

{$R *.dfm}

procedure TLMSCourseForm.actExportExecute(Sender: TObject);
begin
  ExportToExcel(fCourse);
end;

procedure TLMSCourseForm.Action1Execute(Sender: TObject);
begin
  OpenInBrowser(fCourse);
end;

procedure TLMSCourseForm.Action2Execute(Sender: TObject);
begin
  OpenUsersInBrowser(fCourse);
end;

procedure TLMSCourseForm.Action3Execute(Sender: TObject);
var
  SelectedUser: IUser;
begin
  SelectedUser := fUsersTreeView.SelectedUser;

  if SelectedUser <> nil then
    OpenInBrowser(SelectedUser);
end;

procedure TLMSCourseForm.Action3Update(Sender: TObject);
begin
  Action3.Enabled := fUsersTreeView.SelectedUser <> nil;
end;

procedure TLMSCourseForm.Action4Execute(Sender: TObject);
var
  SelectedUser: IUser;
begin
  SelectedUser := fUsersTreeView.SelectedUser;

  if SelectedUser <> nil then
    OpenEditProfileInBrowser(SelectedUser, fCourse);

end;

procedure TLMSCourseForm.Action4Update(Sender: TObject);
begin
  Action4.Enabled := fUsersTreeView.SelectedUser <> nil;
end;

procedure TLMSCourseForm.Action5Execute(Sender: TObject);
begin
  OpenEditCourseInBrowser(fCourse);
end;

procedure TLMSCourseForm.Action6Execute(Sender: TObject);
begin
  // Refresh de users query
  aCourse := fCourse;

  // Requery the filter
  Edit1Change(Edit1);

end;

procedure TLMSCourseForm.Action7Execute(Sender: TObject);
var
  SelectedUser: IUser;
begin
  SelectedUser := fUsersTreeView.SelectedUser;

  if SelectedUser <> nil then
    OpenUserInCourseInBrowser(SelectedUser);
end;

procedure TLMSCourseForm.Action7Update(Sender: TObject);
begin
  Action7.Enabled := fUsersTreeView.SelectedUser <> nil;
end;

procedure TLMSCourseForm.Action8Execute(Sender: TObject);
begin
  ClipBoard.asText := fUsersTreeView.SelectedUser.UserName;
end;

procedure TLMSCourseForm.Action8Update(Sender: TObject);
begin
  Action8.Enabled := fUsersTreeView.SelectedUser <> nil;
end;

constructor TLMSCourseForm.Create(Owner: TComponent);
begin
  inherited;

  fUsersTreeView := TLMSCourseUsersTreeView.Create(self);
  fUsersTreeView.parent := TabControl1;
  fUsersTreeView.align := alClient;

end;

procedure TLMSCourseForm.Edit1Change(Sender: TObject);
begin
  fUsersTreeView.FilterByText(TEdit(Sender).text);
end;

procedure TLMSCourseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSCourseForm.LinkLabel1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(fCourse.LMS.Host + format(COURSE_VIEW,
    [fCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure TLMSCourseForm.SetaCourse(const Value: ICourse);
var
  aRole: string;
  aRolesList: TStringList;
begin
  fCourse := Value;

  Caption := fCourse.displaycontent;

  fUsersTreeView.LMSCourse := fCourse;

  aRolesList := TStringList.Create;
  fCourse.GetCourseRoles(aRolesList);

  Memo1.clear;
  Memo1.lines.BeginUpdate;
  for aRole in aRolesList do
  begin
    Memo1.lines.add(aRole + ' ' + fCourse.GetUserCountByRol(aRole).ToString);
  end;
  Memo1.lines.EndUpdate;

  aRolesList.free;
end;

end.

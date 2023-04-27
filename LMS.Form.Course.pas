unit LMS.Form.Course;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls,
  Vcl.ActnMenus, System.ImageList, Vcl.ImgList,

  LMS._interface.LMS,
  LMS.TreeView.CourseContent,
  LMS.TreeView.CourseUsers;

type
  TLMSCourseForm = class(TForm)
    ActionManager1: TActionManager;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    Panel2: TPanel;
    Action6: TAction;
    ImageList1: TImageList;
    Action7: TAction;
    Action8: TAction;
    actExport: TAction;
    ImageList2: TImageList;
    ActionToolBar1: TActionToolBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Edit1: TEdit;
    tsContent: TTabSheet;
    Memo1: TMemo;
    amCourseContent: TActionManager;
    acDownloadContent: TAction;
    ActionToolBar2: TActionToolBar;
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
    procedure PageControl1Change(Sender: TObject);
    procedure acDownloadContentExecute(Sender: TObject);
  private
    fCourseContentTreeView: TCourseContentTreeView;
    fUsersTreeView: TLMSCourseUsersTreeView;
    fCourse: ICourse;
    procedure SetaCourse(const Value: ICourse);
    procedure SetFilterUser(const Value: IUser);
  public
    constructor Create(Owner: TComponent); override;
    property Course: ICourse read fCourse write SetaCourse;
    property FilterUser: IUser write SetFilterUser;
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

procedure TLMSCourseForm.acDownloadContentExecute(Sender: TObject);
begin
  Course.LMS.DownloadAllCourseContent(Course)
end;

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
  Course := fCourse;

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
  fUsersTreeView.parent := TabSheet1;
  fUsersTreeView.align := alClient;

  fCourseContentTreeView := TCourseContentTreeView.Create(self);
  fCourseContentTreeView.parent := tsContent;
  fCourseContentTreeView.align := alClient;

  PageControl1.ActivePageIndex := 0;
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

procedure TLMSCourseForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0:
      ;
    1:
      begin
        fCourse.GetCourseContent;
        fCourseContentTreeView.LMSCourse := Course;
      end;
    2:
      begin
        fCourse.GetGradeBook;

        Memo1.Lines.Clear;
        for var gradeitem in fCourse.GradeItems do
        begin
          // Memo1.Lines.Add(gradeitem.ItemName);
        end;

      end;
  end;
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

  Memo1.Clear;
  Memo1.Lines.BeginUpdate;
  for aRole in aRolesList do
  begin
    Memo1.Lines.Add(aRole + ' ' + fCourse.GetUserCountByRol(aRole).ToString);
  end;
  Memo1.Lines.EndUpdate;

  aRolesList.free;
end;

procedure TLMSCourseForm.SetFilterUser(const Value: IUser);
begin
  Edit1.text := Value.Full_Name;
  Edit1Change(Edit1);
end;

end.

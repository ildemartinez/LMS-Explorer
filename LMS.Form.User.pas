unit LMS.Form.User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,

  LMS._interface.LMS,
  LMS.TreeView.UserCourses, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.PlatformDefaultStyleActnCtrls, System.Actions, Vcl.ActnList;

type
  TLMSUserForm = class(TForm)
    ActionManager1: TActionManager;
    ActionToolBar1: TActionToolBar;
    Action1: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Action1Execute(Sender: TObject);
  private
    fLMSUser: IUser;
    UserCoursesTreeView: TLMSUserCoursesTreeView;

    procedure SetLMSUser(const Value: IUser);
  public
    constructor Create(Owner: TComponent); override;
    property User: IUser read fLMSUser write SetLMSUser;
  end;

implementation

uses
  LMS.Helper.Browser;

{$R *.dfm}

procedure TLMSUserForm.Action1Execute(Sender: TObject);
begin
  for var aCourse in User.OtherEnrolledCourses do
  begin

    OpenInBrowser(aCourse);
  end;
end;

constructor TLMSUserForm.Create(Owner: TComponent);
begin
  inherited;

  UserCoursesTreeView := TLMSUserCoursesTreeView.Create(self);
  UserCoursesTreeView.parent := self;
  UserCoursesTreeView.Align := alClient;
end;

procedure TLMSUserForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSUserForm.SetLMSUser(const Value: IUser);
begin
  fLMSUser := Value;

  caption := fLMSUser.Full_Name;

  UserCoursesTreeView.User := Value;
  // . Courses := fLMSUser.OtherEnrolledCourses;
end;

end.

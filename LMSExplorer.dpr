program LMSExplorer;

{$R 'ResourcesFile.res' 'ResourcesFile.rc'}

uses
  Vcl.Forms,
  LMS.Form.Main in 'LMS.Form.Main.pas' {MainForm},
  LMS.TreeView.Network in 'LMS.TreeView.Network.pas',
  LMSNetworkUnit in 'LMSNetworkUnit.pas',
  LMSPopupMenuUnit in 'LMSPopupMenuUnit.pas',
  LMS.Rest.Moodle in 'LMS.Rest.Moodle.pas',
  LMS.Helper.Log in 'LMS.Helper.Log.pas',
  LMS.Form.LMS in 'LMS.Form.LMS.pas' {LMSForm},
  LMS.Form.About in 'LMS.Form.About.pas' {AboutForm},
  LMS.Helper.Consts in 'LMS.Helper.Consts.pas',
  LMS.Helper.Images in 'LMS.Helper.Images.pas',
  LMS.Form.UserPassword in 'LMS.Form.UserPassword.pas' {LMSUserPasswordForm},
  LMS.Form.Course in 'LMS.Form.Course.pas' {LMSCourseForm},
  LMS.Helper.Browser in 'LMS.Helper.Browser.pas',
  LMS.TreeView.Users in 'LMS.TreeView.Users.pas',
  LMS.Helper.Utils in 'LMS.Helper.Utils.pas',
  LMS.TreeView.CourseUsers in 'LMS.TreeView.CourseUsers.pas',
  LMS.TreeView.Custom in 'LMS.TreeView.Custom.pas',
  LMS.Form.Category in 'LMS.Form.Category.pas' {LMSCategoryForm},
  LMS.TreeView.CourseCategory in 'LMS.TreeView.CourseCategory.pas',
  LMS.Helper.Excel in 'LMS.Helper.Excel.pas',
  LMS.Helper.RTTI in 'LMS.Helper.RTTI.pas',
  LMS.Form.User in 'LMS.Form.User.pas' {LMSUserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.

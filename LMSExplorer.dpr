program LMSExplorer;

{$R 'ResourcesFile.res' 'ResourcesFile.rc'}

uses
  Vcl.Forms,
  LMS.Form.Main in 'LMS.Form.Main.pas' {MainForm},
  LMS.TreeView.Network in 'LMS.TreeView.Network.pas',
  LMS._class.UsersGroup in 'LMS._class.UsersGroup.pas',
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
  LMS.Form.User in 'LMS.Form.User.pas' {LMSUserForm},
  LMS.Helper.Reports in 'LMS.Helper.Reports.pas',
  LMS._class.Network in 'LMS._class.Network.pas',
  LMS._interface.LMS in 'LMS._interface.LMS.pas',
  LMS._class.LMS in 'LMS._class.LMS.pas',
  LMS._class.User in 'LMS._class.User.pas',
  LMS._class.Course in 'LMS._class.Course.pas',
  LMS._class.Category in 'LMS._class.Category.pas',
  LMS.TreeView.UserCourses in 'LMS.TreeView.UserCourses.pas',
  LMS.Helper.FormFactory in 'LMS.Helper.FormFactory.pas',
  LMS._class.GradeItem in 'LMS._class.GradeItem.pas',
  LMS.TreeView.Courses in 'LMS.TreeView.Courses.pas',
  LMS.TreeView.LMS in 'LMS.TreeView.LMS.pas',
  LMS.TreeView.CourseContent in 'LMS.TreeView.CourseContent.pas',
  LMS._class.Section in 'LMS._class.Section.pas',
  LMS._class.Content in 'LMS._class.Content.pas',
  LMS._class.Module in 'LMS._class.Module.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.

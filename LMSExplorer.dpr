program LMSExplorer;







{$R 'ResourcesFile.res' 'ResourcesFile.rc'}

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  LMSNetworkTreeViewUnit in 'LMSNetworkTreeViewUnit.pas',
  LMSNetworkUnit in 'LMSNetworkUnit.pas',
  LMSPopupMenuUnit in 'LMSPopupMenuUnit.pas',
  LMSRestMoodleUnit in 'LMSRestMoodleUnit.pas',
  LMSLogUnit in 'LMSLogUnit.pas',
  LMSFormUnit in 'LMSFormUnit.pas' {LMSForm},
  LMSAboutFormUnit in 'LMSAboutFormUnit.pas' {AboutForm},
  LMSConstsUnit in 'LMSConstsUnit.pas',
  LMS.Util.ImageListFromResource in 'LMS.Util.ImageListFromResource.pas',
  LMSUserPasswordFormUnit in 'LMSUserPasswordFormUnit.pas' {LMSUserPasswordForm},
  LMSCourseFormUnit in 'LMSCourseFormUnit.pas' {LMSCourseForm},
  LMSBrowserHelperUnit in 'LMSBrowserHelperUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

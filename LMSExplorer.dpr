program LMSExplorer;

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  LMSNetworkTreeViewUnit in 'LMSNetworkTreeViewUnit.pas',
  LMSNetworkUnit in 'LMSNetworkUnit.pas',
  LMSPopupMenuUnit in 'LMSPopupMenuUnit.pas',
  LMSRestMoodleUnit in 'LMSRestMoodleUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

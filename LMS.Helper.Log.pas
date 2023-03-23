unit LMS.Helper.Log;

interface

procedure Log(const message: string);
procedure LogError(const message: string);

implementation

uses
  LMS.Form.Main;

procedure Log(const message: string);
begin
  if MainForm.memo1.visible = false then
    MainForm.memo1.visible := true;

  MainForm.memo1.Lines.add(message);
end;

procedure LogError(const message: string);
begin
  MainForm.memo1.Lines.add('');
  MainForm.memo1.Lines.add('Error --------------');
  MainForm.memo1.Lines.add('    ' + message);
  MainForm.memo1.Lines.add('--------------------');
end;

end.

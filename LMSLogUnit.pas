unit LMSLogUnit;

interface

procedure Log(message: string);
procedure LogError(message: string);

implementation

uses
  MainFormUnit;

procedure Log(message: string);
begin
  MainForm.Memo1.Lines.add(message);
end;

procedure LogError(message: string);
begin
  MainForm.Memo1.Lines.add('');
  MainForm.Memo1.Lines.add('Error --------------');
  MainForm.Memo1.Lines.add('    '+message);
  MainForm.Memo1.Lines.add('--------------------');
end;

end.

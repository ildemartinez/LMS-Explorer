unit LMSLogUnit;

interface

procedure Log(message : string);

implementation

uses
  MainFormUnit;

procedure Log(message : string);
begin
  MainForm.Memo1.Lines.add(message);
end;

end.

unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TMainForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner : Tcomponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  inifiles;

constructor TMainForm.Create(Owner: Tcomponent);
  var
  aIniFile: TIniFile;
  aIniFilePath: string;
  aStrings: TStrings;
  k: Integer;
  aSectionName : string;
begin
  inherited;
  aIniFilePath := ExtractFilePath(ParamStr(0)) + 'config.ini';

  if not FileExists(aIniFilePath) then
  begin
    showmessage
      ('Please, create the config.ini file to continue. You can use the config.ini_dist as template');
    Application.Terminate;
  end
  else
  begin
    aIniFile := TIniFile.Create(aIniFilePath);

    for k := 0 to 100 do
    begin
      aSectionName := 'lms'+inttostr(k);
      if aIniFile.SectionExists(aSectionName) then
      begin
        if aIniFile.ValueExists(aSectionName,'url') then
        begin
        showmessage(aSectionName);
        end;
      end;
    end;

  end;

end;

end.

unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkTreeViewUnit,
  lmsnetworkunit,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    aLMSNetworkTreeView: TLMSNetworkTreeView;

    constructor Create(Owner: Tcomponent); override;
    destructor Destroy; override;
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
  aSectionName: string;
begin
  inherited;

  with TSplitter.Create(self) do
  begin
    parent := self;
    Align := alleft;
  end;

  aLMSNetworkTreeView := TLMSNetworkTreeView.Create(self);
  aLMSNetworkTreeView.parent := self;
  aLMSNetworkTreeView.Align := alleft;

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
      aSectionName := 'lms' + inttostr(k);
      if aIniFile.SectionExists(aSectionName) then
      begin
        var
          aLMS: TLMS := TLMS.Create;

        aLMS.id := aSectionName;

        aLMS.user := aIniFile.ReadString(aSectionName, 'user', '');
        aLMS.password := aIniFile.ReadString(aSectionName, 'password', '');
        aLMS.service := aIniFile.ReadString(aSectionName, 'service', '');

        GetGlobalNetwork.add(aLMS);

        aLMS.Free;
      end;
    end;

  end;

  aLMSNetworkTreeView.LMSNetwork := GetGlobalNetwork;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

end.

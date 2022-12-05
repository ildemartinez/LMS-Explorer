unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkTreeViewUnit,
  lmsnetworkunit,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus, System.Actions, Vcl.ActnList,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    Action1: TAction;
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
  inifiles, system.JSON;

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

        aLMS.url := aIniFile.ReadString(aSectionName, 'url', '');
        aLMS.user := aIniFile.ReadString(aSectionName, 'user', '');
        aLMS.password := aIniFile.ReadString(aSectionName, 'password', '');
        aLMS.service := aIniFile.ReadString(aSectionName, 'service', '');

        GetGlobalNetwork.add(aLMS);
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

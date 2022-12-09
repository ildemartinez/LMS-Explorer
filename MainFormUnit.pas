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

const
  WM_AFTER_SHOW = WM_USER + 300;

type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    Action1: TAction;
    Memo1: TMemo;

    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
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
  inifiles, System.JSON,
  lmslogUnit;

constructor TMainForm.Create(Owner: Tcomponent);

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

end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TMainForm.WmAfterShow(var Msg: TMessage);
var
  aIniFile: TIniFile;
  aIniFilePath: string;
  k: Integer;
  aSectionName: string;
begin
  aIniFilePath := ExtractFilePath(ParamStr(0)) + 'config.ini';
  log('Trying to load ' + aIniFilePath);

  if not FileExists(aIniFilePath) then
  begin
    log('Please, create the config.ini file to continue. You can use the config.ini_dist as template');
  end
  else
  begin
    aIniFile := TIniFile.Create(aIniFilePath);
    log('Config file loaded');

    for k := 0 to 100 do // refactor
    begin
      aSectionName := 'lms' + inttostr(k);
      if aIniFile.SectionExists(aSectionName) then
      begin
        var
          aLMS: TLMS := TLMS.Create(self);

        aLMS.id := aSectionName;

        aLMS.Host := aIniFile.ReadString(aSectionName, 'url', '');
        aLMS.user := aIniFile.ReadString(aSectionName, 'user', '');
        aLMS.password := aIniFile.ReadString(aSectionName, 'password', '');
        aLMS.service := aIniFile.ReadString(aSectionName, 'service', '');

        GetGlobalNetwork.add(aLMS);

        if aIniFile.ReadBool(aSectionName, 'autoconnect', false) = true then
          aLMS.Connect;
      end;
    end;

    aLMSNetworkTreeView.LMSNetwork := GetGlobalNetwork;
  end;

end;

end.

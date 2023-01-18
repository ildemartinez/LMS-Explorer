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
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.StdCtrls, Vcl.StdActns;

const
  WM_AFTER_SHOW = WM_USER + 300;

type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    Action1: TAction;
    Memo1: TMemo;
    WindowCascade1: TWindowCascade;
    WindowMinimizeAll1: TWindowMinimizeAll;
    Action2: TAction;

    procedure FormShow(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action2Update(Sender: TObject);
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
  inifiles,
  System.JSON,
  lmsaboutformunit,
  lmslogUnit;

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  with TAboutForm.create(self) do
  begin
    ShowModal;
    free;
  end;
end;

procedure TMainForm.Action2Execute(Sender: TObject);
begin
   //
   for var i := Screen.FormCount-1 downto 1 do
    Screen.forms[i].Close;

end;

procedure TMainForm.Action2Update(Sender: TObject);
begin
action2.Enabled := (screen.FormCount>1);
end;

constructor TMainForm.Create(Owner: Tcomponent);
begin
  inherited;

  with TSplitter.Create(self) do
  begin
    parent := self;
    Align := alLeft;
  end;

  var
  aPanel := TPanel.Create(self);
  with aPanel do
  begin
    parent := self;
    Align := alLeft;
  end;

  aLMSNetworkTreeView := TLMSNetworkTreeView.Create(self);
  aLMSNetworkTreeView.parent := aPanel;
  aLMSNetworkTreeView.Align := alClient;

  var
  aFilterEdit := TEdit.Create(self);
  with aFilterEdit do
  begin
    parent := aPanel;
    Align := alTop;
    OnChange := Edit1Change;
  end;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  aLMSNetworkTreeView.FilterByText(TEdit(Sender).text);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TMainForm.WmAfterShow(var Msg: TMessage);
var
  aIniFile: TIniFile;
  aSections: TStrings;
  aSection: string;
  aIniFilePath: string;
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

    aSections := TStringList.Create;
    aIniFile.ReadSections(aSections);

    for aSection in aSections do
    begin
      var
        aLMS: TLMS := TLMS.Create(self);

      aLMS.id := aSection;

      aLMS.Host := aIniFile.ReadString(aSection, 'url', '');
      aLMS.user := aIniFile.ReadString(aSection, 'user', '');
      aLMS.password := aIniFile.ReadString(aSection, 'password', '');
      aLMS.service := aIniFile.ReadString(aSection, 'service', '');
      aLMS.autoconnect := aIniFile.ReadBool(aSection, 'autoconnect', false);

      GetGlobalNetwork.add(aLMS);
    end;

    aLMSNetworkTreeView.LMSNetwork := GetGlobalNetwork;
  end;

end;

end.

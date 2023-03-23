unit LMS.Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  lmsnetworkunit,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus, System.Actions, Vcl.ActnList,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.StdCtrls, Vcl.StdActns,

  LMS.TreeView.Network;

const
  WM_AFTER_SHOW = WM_USER + 300;

type
  TMainForm = class(TForm)
    ActionMainMenuBar1: TActionMainMenuBar;
    MainActionManager: TActionManager;
    Action1: TAction;
    Memo1: TMemo;
    WindowCascade1: TWindowCascade;
    WindowMinimizeAll1: TWindowMinimizeAll;
    Action2: TAction;
    Panel1: TPanel;
    Splitter1: TSplitter;
    CoursesActionManager: TActionManager;
    ActionMainMenuBar2: TActionMainMenuBar;
    Edit1: TEdit;
    Action3: TAction;

    procedure FormShow(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action2Update(Sender: TObject);
    procedure Action3Update(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
  private
    aLMSNetworkTreeView: TLMSNetworkTreeView;

    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
  public
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

  LMS._class.Network,
  LMS.Helper.Log,
  LMS.Form.About;

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  with TAboutForm.Create(self) do
  begin
    ShowModal;
    free;
  end;
end;

procedure TMainForm.Action2Execute(Sender: TObject);
begin
  for var i := Screen.FormCount - 1 downto 1 do
    Screen.Forms[i].Close;
end;

procedure TMainForm.Action2Update(Sender: TObject);
begin
  Action2.Enabled := (Screen.FormCount > 1);
end;

procedure TMainForm.Action3Execute(Sender: TObject);
begin
  Edit1.Text := '';
  Edit1.OnChange(Edit1);

  aLMSNetworkTreeView.focusselectedNode;
end;

procedure TMainForm.Action3Update(Sender: TObject);
begin
  Action3.Enabled := Edit1.Text <> '';
end;

constructor TMainForm.Create(Owner: Tcomponent);
begin
  inherited;

  aLMSNetworkTreeView := TLMSNetworkTreeView.Create(self);
  aLMSNetworkTreeView.parent := Panel1;
  aLMSNetworkTreeView.Align := alClient;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  aLMSNetworkTreeView.FilterByText(TEdit(Sender).Text);
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
  // log('Trying to load ' + aIniFilePath);

  if not FileExists(aIniFilePath) then
  begin
    Log('Please, create the config.ini file to continue. You can use the config.ini_dist as template');
  end
  else
  begin
    aIniFile := TIniFile.Create(aIniFilePath);
    // log('Config file loaded');

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

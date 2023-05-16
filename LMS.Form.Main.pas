unit LMS.Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus, System.Actions, Vcl.ActnList,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.StdCtrls, Vcl.StdActns,

  LMS.TreeView.Network, Vcl.AppEvnts, System.ImageList, Vcl.ImgList;

const
  WM_AFTER_CREATE = WM_USER + 300;

type
  TMainForm = class(TForm)
    ActionMainMenuBar1: TActionMainMenuBar;
    MainActionManager: TActionManager;
    actAbout: TAction;
    Memo1: TMemo;
    WindowCascade1: TWindowCascade;
    WindowMinimizeAll1: TWindowMinimizeAll;
    Action2: TAction;
    Panel1: TPanel;
    Splitter1: TSplitter;
    CoursesActionManager: TActionManager;
    ActionMainMenuBar2: TActionMainMenuBar;
    edFilter: TEdit;
    Action3: TAction;
    actExit: TAction;
    ImageList1: TImageList;
    Button1: TButton;
    actCollapseAll: TAction;
    actExpandAll: TAction;

    procedure edFilterChange(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action2Update(Sender: TObject);
    procedure Action3Update(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
  private
    aLMSNetworkTreeView: TLMSNetworkTreeView;

    procedure WmAfterCreate(var Msg: TMessage); message WM_AFTER_CREATE;
  public
    constructor Create(Owner: Tcomponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  inifiles,
  System.JSON,
  urlmon,
  LMS._interface.LMS,
  LMS._class.LMS,
  LMS._class.Network,

  LMS.Helper.Log,
  LMS.Helper.Utils,
  LMS.Form.About;

procedure TMainForm.actAboutExecute(Sender: TObject);
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
  edFilter.Text := '';
  edFilter.OnChange(edFilter);

  aLMSNetworkTreeView.FocusSelectedNode;
end;

procedure TMainForm.Action3Update(Sender: TObject);
begin
  Action3.Enabled := edFilter.Text <> '';
end;

procedure TMainForm.actCollapseAllExecute(Sender: TObject);
begin
  self.aLMSNetworkTreeView.FullCollapse();
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  actExit.Checked := true;
  Close;
end;

procedure TMainForm.actExpandAllExecute(Sender: TObject);
begin
  aLMSNetworkTreeView.FullExpandLMS;
end;

constructor TMainForm.Create(Owner: Tcomponent);
begin
  inherited;

  top := 0;
  left := 0;
  ClientWidth := Screen.Width;
  ClientHeight := Screen.Height;

  aLMSNetworkTreeView := TLMSNetworkTreeView.Create(self);
  aLMSNetworkTreeView.parent := Panel1;
  aLMSNetworkTreeView.Align := alClient;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin

  hhide := not hhide;

  if hhide then
    Button1.caption := '*)'
  else
    Button1.caption := ':)';

end;

procedure TMainForm.edFilterChange(Sender: TObject);
begin
  aLMSNetworkTreeView.FilterByText(TEdit(Sender).Text);
  actExpandAllExecute(self);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PostMessage(self.Handle, WM_AFTER_CREATE, 0, 0);
end;

procedure TMainForm.WmAfterCreate(var Msg: TMessage);
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
        aLMS: ILMS := TLMS.Create(self);

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

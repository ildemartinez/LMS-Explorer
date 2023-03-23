unit LMS.Form.Category;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  LMSNetworkUnit, System.Actions, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls,

  Vcl.ComCtrls, Vcl.ExtCtrls,

  LMS.TreeView.CourseCategory;

type
  TLMSCategoryForm = class(TForm)
    ActionToolBar1: TActionToolBar;
    Panel2: TPanel;
    TabControl1: TTabControl;
    ActionManager1: TActionManager;
    Action1: TAction;
    Action2: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Action1Execute(Sender: TObject);
    procedure Action1Update(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action2Update(Sender: TObject);
  private
    fCategory: TLMSCategory;
    fCategoryTreeView :TLMSCategoryTreeView;

    procedure SetCategory(const Value: TLMSCategory);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    property aCategory: TLMSCategory read fCategory write SetCategory;
  end;

implementation

uses
  LMS.Helper.Browser;

{$R *.dfm}

procedure TLMSCategoryForm.Action1Execute(Sender: TObject);
var
  aCourse : TLMSCourse;
begin
  for aCourse in fCategory.fcourses do
    OpenInBrowser(aCourse);
end;

procedure TLMSCategoryForm.Action1Update(Sender: TObject);
begin
  Action1.Enabled := aCategory.SubCategoriesCount = 0;
end;

procedure TLMSCategoryForm.Action2Execute(Sender: TObject);
var
  aCourse : TLMSCourse;
begin
  for aCourse in fCategory.fcourses do
    OpenUsersInBrowser(aCourse);
end;

procedure TLMSCategoryForm.Action2Update(Sender: TObject);
begin
  Action2.Enabled := aCategory.SubCategoriesCount = 0;
end;

constructor TLMSCategoryForm.Create(Owner: TComponent);
begin
  inherited;

  fCategoryTreeView := TLMSCategoryTreeView.Create(self);
  fCategoryTreeView.parent := TabControl1;
  fCategoryTreeView.align := alClient;
end;

procedure TLMSCategoryForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSCategoryForm.SetCategory(const Value: TLMSCategory);
begin
  fCategory := Value;

  Caption := fCategory.name;
  fCategoryTreeView.Category := fCategory;

end;

end.

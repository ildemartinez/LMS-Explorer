unit LMSCategoryFormUnit;

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
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls;

type
  TLMSCategoryForm = class(TForm)
    ActionManager1: TActionManager;
    ActionToolBar1: TActionToolBar;
    Action1: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Action1Execute(Sender: TObject);
    procedure Action1Update(Sender: TObject);
  private
    fCategory: TLMSCategory;
    procedure SetCategory(const Value: TLMSCategory);
    { Private declarations }
  public
    { Public declarations }
    property aCategory: TLMSCategory read fCategory write SetCategory;
  end;

implementation

uses
  LMSBrowserHelperUnit,
  LMSLogUnit;

{$R *.dfm}

procedure TLMSCategoryForm.Action1Execute(Sender: TObject);
begin
  for var acourse in fCategory.fcourses do
    OpenInBrowser(acourse);
end;

procedure TLMSCategoryForm.Action1Update(Sender: TObject);
begin
  Action1.Enabled := aCategory.SubCategoriesCount = 0;
end;

procedure TLMSCategoryForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSCategoryForm.SetCategory(const Value: TLMSCategory);
begin
  fCategory := Value;
end;

end.

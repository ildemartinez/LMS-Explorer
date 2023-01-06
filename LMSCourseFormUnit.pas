unit LMSCourseFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkUnit, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
   LMSUsersTreeViewUnit;

type
  TLMSCourseForm = class(TForm)
    TabControl1: TTabControl;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1Click(Sender: TObject);
  private
    fLMS : TLMS;
    fUsersTreeView : TLMSUsersTreeView;
    fCourse: TLMSCourse;
    procedure SetaCourse(const Value: TLMSCourse);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner : TComponent); override;
    property aCourse: TLMSCourse read fCourse write SetaCourse;

  end;

var
  LMSCourseForm: TLMSCourseForm;

implementation

uses
    ShellApi,
    LMSConstsUnit;

{$R *.dfm}

constructor TLMSCourseForm.Create(Owner: TComponent);
begin
  inherited;

  fUsersTreeView := TLMSUsersTreeView.Create(self);
  fUsersTreeView.parent := TabControl1;
  fUsersTreeView.align := alClient;
end;

procedure TLMSCourseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSCourseForm.LinkLabel1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(fCourse.LMS.Host + format(COURSE_VIEW,
    [fCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure TLMSCourseForm.SetaCourse(const Value: TLMSCourse);
begin
  fCourse := Value;

  self.Caption := fCourse.displaycontent;

  fUsersTreeView.LMSCourse := value;
end;

end.

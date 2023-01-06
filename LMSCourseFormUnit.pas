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
    Label1: TLabel;
    TabControl1: TTabControl;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

procedure TLMSCourseForm.Button1Click(Sender: TObject);
var
  aUser : TLMSUser;
begin
  aCourse.RefreshEnrolledUsers;

  for aUser in self.aCourse.fUsers do
  begin
    Memo1.Lines.Add(aUser.fUserName);
  end;



end;

constructor TLMSCourseForm.Create(Owner: TComponent);
begin
  inherited;

  fUsersTreeView := TLMSUsersTreeView.Create(self);
  fUsersTreeView.parent := self;
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

  Label1.Caption := fCourse.displaycontent;

  fUsersTreeView.LMSCourse := value;
end;

end.

unit LMSCourseFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkUnit, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TLMSCourseForm = class(TForm)
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1Click(Sender: TObject);
  private
    fLMS : TLMS;
    fCourse: TLMSCourse;
    procedure SetaCourse(const Value: TLMSCourse);
    { Private declarations }
  public
    { Public declarations }
    property aLMS : TLMS read fLMS write fLMS;
    property aCourse: TLMSCourse read fCourse write SetaCourse;

  end;

var
  LMSCourseForm: TLMSCourseForm;

implementation

uses
    ShellApi,
    LMSConstsUnit;

{$R *.dfm}

procedure TLMSCourseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSCourseForm.LinkLabel1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + format(COURSE_VIEW,
    [fCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure TLMSCourseForm.SetaCourse(const Value: TLMSCourse);
begin
  fCourse := Value;

  Label1.Caption := fCourse.displaycontent;
end;

end.

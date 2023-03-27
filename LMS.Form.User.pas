unit LMS.Form.User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  LMS._interface.LMS, Vcl.StdCtrls;

type
  TLMSUserForm = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fLMSUser: IUser;

    procedure SetLMSUser(const Value: IUser);
  public
    property User: IUser read fLMSUser write SetLMSUser;
  end;

implementation

{$R *.dfm}

procedure TLMSUserForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSUserForm.SetLMSUser(const Value: IUser);
begin
  fLMSUser := Value;

  caption := fLMSUser.Full_Name + fLMSUser.OtherEnrolledCourses.Count.ToString;

  for var course in fLMSUser.OtherEnrolledCourses do
begin
  memo1.Lines.add(course.FullName);
  Memo1.Lines.add(course.Category.Name);
end;
end;

end.

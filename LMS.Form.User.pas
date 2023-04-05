unit LMS.Form.User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,

  LMS._interface.LMS,
  LMS.TreeView.UserCourses;

type
  TLMSUserForm = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fLMSUser: IUser;
    UserCoursesTreeView: TLMSUserCoursesTreeView;

    procedure SetLMSUser(const Value: IUser);
  public
    constructor Create(Owner: TComponent); override;
    property User: IUser read fLMSUser write SetLMSUser;
  end;

implementation

{$R *.dfm}

constructor TLMSUserForm.Create(Owner: TComponent);
begin
  inherited;

  UserCoursesTreeView := TLMSUserCoursesTreeView.Create(self);
  UserCoursesTreeView.parent := self;
  UserCoursesTreeView.Align := alClient;
end;

procedure TLMSUserForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSUserForm.SetLMSUser(const Value: IUser);
begin
  fLMSUser := Value;

  caption := fLMSUser.Full_Name;

  UserCoursesTreeView.User := value; // . Courses := fLMSUser.OtherEnrolledCourses;
end;

end.

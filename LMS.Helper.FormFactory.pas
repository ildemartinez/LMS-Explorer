unit LMS.Helper.FormFactory;

interface

uses
  LMS._interface.LMS;

procedure ViewForm(const aCourse: ICourse); overload;
procedure ViewForm(const aCategory: ICategory); overload;
procedure ViewForm(const aLMS: ILMS); overload;
procedure ViewForm(const aUser : IUser); overload;

implementation

uses
  LMS.Form.Main,
  LMS.Form.Course,
  LMS.Form.Category,
  LMS.Form.User,
  LMS.Form.LMS;

procedure ViewForm(const aCourse: ICourse);
begin
  with TLMSCourseForm.Create(MainForm) do
  begin
    Course := aCourse;
    show();
  end;
end;

procedure ViewForm(const aCategory: ICategory);
begin
  with TLMSCategoryForm.Create(MainForm) do
  begin
    Category := aCategory;
    show();
  end;
end;

procedure ViewForm(const aLMS: ILMS);
begin
  with TLMSForm.Create(MainForm) do
  begin
    LMS := aLMS;
    show();
  end;
end;

procedure ViewForm(const aUser : IUser); overload;
begin
  with TLMSUserForm.Create(MainForm) do
  begin
    User := aUser;
    show();
  end;
end;


end.

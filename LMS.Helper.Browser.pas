unit LMS.Helper.Browser;

interface

uses
  LMS._interface.LMS,
  LMSNetWorkUnit;

// Opens Moodle instace, category or course at the default system browser
procedure OpenInBrowser(const aURL: string); overload;
procedure OpenInBrowser(const aLMS: ILMS); overload;
procedure OpenInBrowser(const aCategory: ILMSCategory); overload;
procedure OpenInBrowser(const aCourse: ILMSCourse); overload;
procedure OpenInBrowser(const aUser: ILMSUser); overload;
procedure OpenInBrowser(const aUser: ILMSUser;
  const aCourse: ILMSCourse); overload;
//

procedure OpenUsersInBrowser(const aLMS: ILMS); overload;
procedure OpenUsersInBrowser(const aCourse: ILMSCourse); overload;
procedure OpenUploadUsersInBrowser(const aLMS: ILMS);
procedure OpenUserInCourseInBrowser(const aUser: ILMSUser);
procedure OpenEditProfileInBrowser(const aUser: ILMSUser;
  const aCourse: ILMSCourse);
procedure OpenCreateUserInBrowser(const aLMS: ILMS);

procedure OpenInBrowserByLMS(const aLMS: ILMS; const aUser: ILMSUser);
procedure OpenEditProfileByLMS(const aLMS: ILMS; const aUser: ILMSUser);

// Edit course
procedure OpenEditCourseInBrowser(const aCourse: TLMSCourse);

// Services
procedure OpenExternalServices(const aLMS: ILMS);

implementation

uses
  sysutils,
  Winapi.ShellAPI,

  LMS.Helper.Consts;

procedure OpenInBrowser(const aURL: string); overload;
begin
  ShellExecute(0, 'open', PChar(aURL), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aLMS: ILMS); overload;
begin
  ShellExecute(0, 'open', PChar(aLMS.Host), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aCategory: ILMSCategory); overload;
begin
  ShellExecute(0, 'open', PChar(aCategory.LMS.Host + format(CATEGORY_VIEW,
    [aCategory.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aCourse: ILMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aCourse.LMS.Host + format(COURSE_VIEW,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenUsersInBrowser(const aLMS: ILMS); overload;
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + ADMIN_USER), nil, nil, 0);
  // SW_SHOW);
end;

procedure OpenUsersInBrowser(const aCourse: ILMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aCourse.LMS.Host + format(USERS_VIEW,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aUser: ILMSUser); overload;
begin
  ShellExecute(0, 'open', PChar(aUser.Course.LMS.Host + format(PROFILE_VIEW,
    [aUser.Id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aUser: ILMSUser;
  const aCourse: ILMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aUser.Course.LMS.Host +
    format(PROFILE_VIEW_IN_COURSE, [aUser.Id, aCourse.id])), nil, nil, 0);
  // SW_SHOW);
end;

procedure OpenEditProfileInBrowser(const aUser: ILMSUser;
  const aCourse: ILMSCourse);
begin
  ShellExecute(0, 'open', PChar(aUser.Course.LMS.Host +
    format(EDIT_PROFILE_IN_COURSE, [aUser.Id, aCourse.id])), nil, nil, 0);
  // SW_SHOW);
end;

procedure OpenEditCourseInBrowser(const aCourse: TLMSCourse);
begin
  ShellExecute(0, 'open', PChar(aCourse.LMS.Host + format(EDIT_COURSE,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenExternalServices(const aLMS: ILMS);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + ADMIN_SETTINGS_EXTERNALSERVICES),
    nil, nil, 0); // SW_SHOW);
end;

procedure OpenUserInCourseInBrowser(const aUser: ILMSUser);
begin
  ShellExecute(0, 'open', PChar(aUser.Course.LMS.Host +
    format(USERS_VIEW_FIRSTNAME_LASTNAME, [aUser.Course.id,
    UpperCase(aUser.First_Name[1]), UpperCase(aUser.Last_Name[1])])), nil, nil,
    0); // SW_SHOW);
  // https://campusvirtual.unia.es/user/index.php?id=119&tifirst=J&tilast=A
end;

procedure OpenCreateUserInBrowser(const aLMS: ILMS);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + USER_CREATE), nil, nil, 0);
end;

procedure OpenUploadUsersInBrowser(const aLMS: ILMS);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + USERS_UPLOAD), nil, nil, 0);
end;

procedure OpenInBrowserByLMS(const aLMS: ILMS; const aUser: ILMSUser);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + format(PROFILE_VIEW, [aUser.Id])),
    nil, nil, 0); // SW_SHOW);
end;

procedure OpenEditProfileByLMS(const aLMS: ILMS; const aUser: ILMSUser);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + format(EDIT_PROFILE, [aUser.Id])),
    nil, nil, 0); // SW_SHOW);
end;

end.

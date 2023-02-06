unit LMSBrowserHelperUnit;

interface

uses
  LMSNetWorkUnit;

// Opens Moodle instace, category or course at the default system browser
procedure OpenInBrowser(const aURL: string); overload;
procedure OpenInBrowser(const aLMS: tlms); overload;
procedure OpenInBrowser(const aCategory: TLMSCategory); overload;
procedure OpenInBrowser(const aCourse: TLMSCourse); overload;
procedure OpenInBrowser(const aUser: TLMSUser); overload;
procedure OpenInBrowser(const aUser: TLMSUser;
  const aCourse: TLMSCourse); overload;
//

procedure OpenUsersInBrowser(const aLMS: tlms); overload;
procedure OpenUsersInBrowser(const aCourse: TLMSCourse); overload;
procedure OpenUploadUsersInBrowser(const aLMS: tlms);
procedure OpenUserInCourseInBrowser(const aUser: TLMSUser);
procedure OpenEditProfileInBrowser(const aUser: TLMSUser;
  const aCourse: TLMSCourse);
procedure OpenCreateUserInBrowser(const aLMS: tlms);

// Edit course
procedure OpenEditCourseInBrowser(const aCourse: TLMSCourse);

// Services
procedure OpenExternalServices(const aLMS: tlms);

implementation

uses
  sysutils,
  Winapi.ShellAPI,
  LMSconstsUnit;

procedure OpenInBrowser(const aURL: string); overload;
begin
  ShellExecute(0, 'open', PChar(aURL), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aLMS: tlms); overload;
begin
  ShellExecute(0, 'open', PChar(aLMS.Host), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aCategory: TLMSCategory); overload;
begin
  ShellExecute(0, 'open', PChar(aCategory.LMS.Host + format(CATEGORY_VIEW,
    [aCategory.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aCourse: TLMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aCourse.LMS.Host + format(COURSE_VIEW,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenUsersInBrowser(const aLMS: tlms); overload;
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + ADMIN_USER), nil, nil, 0);
  // SW_SHOW);
end;

procedure OpenUsersInBrowser(const aCourse: TLMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aCourse.LMS.Host + format(USERS_VIEW,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aUser: TLMSUser); overload;
begin
  ShellExecute(0, 'open', PChar(aUser.fCourse.LMS.Host + format(PROFILE_VIEW,
    [aUser.fid])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aUser: TLMSUser;
  const aCourse: TLMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aUser.fCourse.LMS.Host +
    format(PROFILE_VIEW_IN_COURSE, [aUser.fid, aUser.fCourse.id])), nil, nil,
    0); // SW_SHOW);
end;

procedure OpenEditProfileInBrowser(const aUser: TLMSUser;
  const aCourse: TLMSCourse);
begin
  ShellExecute(0, 'open', PChar(aUser.fCourse.LMS.Host +
    format(EDIT_PROFILE_IN_COURSE, [aUser.fid, aUser.fCourse.id])), nil, nil,
    0); // SW_SHOW);
end;

procedure OpenEditCourseInBrowser(const aCourse: TLMSCourse);
begin
  ShellExecute(0, 'open', PChar(aCourse.LMS.Host + format(EDIT_COURSE,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenExternalServices(const aLMS: tlms);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + ADMIN_SETTINGS_EXTERNALSERVICES),
    nil, nil, 0); // SW_SHOW);
end;

procedure OpenUserInCourseInBrowser(const aUser: TLMSUser);
begin
  ShellExecute(0, 'open', PChar(aUser.fCourse.LMS.Host +
    format(USERS_VIEW_FIRSTNAME_LASTNAME, [aUser.fCourse.id,
    UpperCase(aUser.fFirstName[1]), UpperCase(aUser.fLastName[1])])), nil, nil,
    0); // SW_SHOW);
  // https://campusvirtual.unia.es/user/index.php?id=119&tifirst=J&tilast=A
end;

procedure OpenCreateUserInBrowser(const aLMS: tlms);
begin
  ShellExecute(0, 'open', PChar(aLMS.Host + USER_CREATE), nil, nil, 0);
end;

procedure OpenUploadUsersInBrowser(const aLMS: tlms);
begin
ShellExecute(0, 'open', PChar(aLMS.Host + USERS_UPLOAD), nil, nil, 0);
end;
end.

unit LMSBrowserHelperUnit;

interface

uses
  LMSNetWorkUnit;

// Opens Moodle instace, category or course at the default system browser
procedure OpenInBrowser(const aLMS: tlms); overload;
procedure OpenInBrowser(const aCategory: TLMSCategory); overload;
procedure OpenInBrowser(const aCourse: TLMSCourse); overload;
procedure OpenInBrowser(const aUser: TLMSUser); overload;
procedure OpenInBrowser(const aUser: TLMSUser; const aCourse : TLMSCourse); overload;
//

procedure OpenUsersInBrowser(const aCourse: TLMSCourse); overload;

implementation

uses
  sysutils,
  Winapi.ShellAPI,
  LMSconstsUnit;

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

procedure OpenInBrowser(const aUser: TLMSUser; const aCourse : TLMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aUser.fCourse.LMS.Host + format(PROFILE_VIEW_IN_COURSE,
    [aUser.fid, aUser.fCourse.Id])), nil, nil, 0); // SW_SHOW);
end;

end.

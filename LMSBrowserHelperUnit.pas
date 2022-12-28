unit LMSBrowserHelperUnit;

interface

uses
  LMSNetWorkUnit;

procedure OpenInBrowser(const aLMS: tlms); overload;
procedure OpenInBrowser(const aCategory: TLMSCategory); overload;
procedure OpenInBrowser(const aCourse: TLMSCourse); overload;

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
  ShellExecute(0, 'open', PChar(aCategory.fLMS.Host + format(CATEGORY_VIEW,
    [aCategory.id])), nil, nil, 0); // SW_SHOW);
end;

procedure OpenInBrowser(const aCourse: TLMSCourse); overload;
begin
  ShellExecute(0, 'open', PChar(aCourse.fLMS.Host + format(COURSE_VIEW,
    [aCourse.id])), nil, nil, 0); // SW_SHOW);
end;

end.

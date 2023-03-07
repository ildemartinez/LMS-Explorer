unit LMSExcelUnit;

interface

uses
  LMSNetworkUnit,

  Excel2010;

procedure ExportToExcel(const aLMSCourse: TLMSCourse);

implementation

uses
  Winapi.Windows,
  System.Variants;

procedure ExportToExcel(const aLMSCourse: TLMSCourse);
var
  FXLA: TExcelApplication;
  FXLWB: TExcelWorkbook;
  FXLWS: TExcelWorkSheet;
  LCID: Integer;
begin
  FXLA := TExcelApplication.Create(nil);
  FXLWB := TExcelWorkbook.Create(nil);
  FXLWS := TExcelWorkSheet.Create(nil);

  FXLA.AutoConnect := True;

  // Get "local user ID"
  LCID := GetUserDefaultLCID;

  // Hide Excel while working
  FXLA.Visible[LCID] := False;
  try
    // no prompts
    FXLA.DisplayAlerts[LCID] := False;

    // add new workbook and connect WB wrapper
    FXLWB.ConnectTo(FXLA.Workbooks.Add(xlWBATWorksheet, LCID));

    // connect to sheet 1
    FXLWS.ConnectTo(FXLWB.Worksheets[1] as _Worksheet);

    // select the first worksheet
    (FXLWB.Worksheets[1] as _Worksheet).Select(EmptyParam, LCID);

    FXLWS.Cells.item[1, 1] := aLMSCourse.shortname;
    FXLWS.Cells.item[1, 3] := aLMSCourse.fullname;

    var
    aRow := 5;
    if aLMSCourse.fUserGroups.Count > 0 then
    begin
      for var aGroup in aLMSCourse.fUserGroups do
      begin
        FXLWS.Cells.item[aRow, 2] := aGroup.fName;
        inc(aRow);

        for var aUser in aGroup.fUsersInGroup do
        begin
          FXLWS.Cells.item[aRow, 3] := aUser.fFirstName;
          FXLWS.Cells.item[aRow, 4] := aUser.fLastName;
          FXLWS.Cells.item[aRow, 5] := aUser.fUserName;
          inc(aRow);
        end;

      end;
    end;

  finally
    // failsafe always leave excel visible when we quit/disconnect.
    FXLA.Visible[LCID] := True;

    FXLWS.Free;
    FXLWB.Free;
    FXLA.Free;
  end;
end;

end.

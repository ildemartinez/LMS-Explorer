unit LMS.Helper.Reports;

interface

uses
  LMSNetworkUnit;

procedure ExportToExcel(const aLMSCourse: TLMSCourse);

implementation

uses
  LMS.Helper.Excel;

procedure ExportToExcel(const aLMSCourse: TLMSCourse);
var
  ExcelWS: TExcelWorkSpace;
  LCID: Integer;
begin
  ExcelWS := TExcelWorkSpace.Create;

  try
    ExcelWS.fXLWS.Cells.item[1, 1] := aLMSCourse.shortname;
    ExcelWS.fXLWS.Cells.item[1, 3] := aLMSCourse.fullname;

    var
    aRow := 5;
    if aLMSCourse.fUserGroups.Count > 0 then
    begin
      for var aGroup in aLMSCourse.fUserGroups do
      begin
        ExcelWS.fXLWS.Cells.item[aRow, 2] := aGroup.Group;
        inc(aRow);

        for var aUser in aGroup.fUsersInGroup do
        begin
          ExcelWS.fXLWS.Cells.item[aRow, 3] := aUser.First_Name;
          ExcelWS.fXLWS.Cells.item[aRow, 4] := aUser.Last_Name;
          ExcelWS.fXLWS.Cells.item[aRow, 5] := aUser.fUserName;
          inc(aRow);
        end;

      end;
    end
    else
    begin
      for var aUser in aLMSCourse.fUsers do
      begin
        ExcelWS.fXLWS.Cells.item[aRow, 3] := aUser.First_Name;
        ExcelWS.fXLWS.Cells.item[aRow, 4] := aUser.Last_Name;
        ExcelWS.fXLWS.Cells.item[aRow, 5] := aUser.fUserName;
        inc(aRow);
      end;
    end;

  finally
    ExcelWS.ShowAndFinally;
  end;
end;

end.

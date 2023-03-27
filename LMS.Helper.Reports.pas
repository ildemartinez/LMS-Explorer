unit LMS.Helper.Reports;

interface

uses
  LMS._interface.LMS;

procedure ExportToExcel(const aLMSCourse: ICourse);

implementation

uses
  LMS.Helper.Excel;

procedure ExportToExcel(const aLMSCourse: ICourse);
var
  ExcelWS: TExcelWorkSpace;
begin
  ExcelWS := TExcelWorkSpace.Create;

  try
    ExcelWS.fXLWS.Cells.item[1, 1] := aLMSCourse.ShortName;
    ExcelWS.fXLWS.Cells.item[1, 3] := aLMSCourse.FullName;

    var
    aRow := 5;
    if aLMSCourse.UserGroups.Count > 0 then
    begin
      for var aGroup in aLMSCourse.UserGroups do
      begin
        ExcelWS.fXLWS.Cells.item[aRow, 2] := aGroup.GroupName;
        inc(aRow);

        for var aUser in aGroup.UsersInGroup do
        begin
          ExcelWS.fXLWS.Cells.item[aRow, 3] := aUser.First_Name;
          ExcelWS.fXLWS.Cells.item[aRow, 4] := aUser.Last_Name;
          ExcelWS.fXLWS.Cells.item[aRow, 5] := aUser.UserName;
          inc(aRow);
        end;

      end;
    end
    else
    begin
      for var aUser in aLMSCourse.Users do
      begin
        ExcelWS.fXLWS.Cells.item[aRow, 3] := aUser.First_Name;
        ExcelWS.fXLWS.Cells.item[aRow, 4] := aUser.Last_Name;
        ExcelWS.fXLWS.Cells.item[aRow, 5] := aUser.UserName;
        inc(aRow);
      end;
    end;

  finally
    ExcelWS.ShowAndFinally;
  end;
end;

end.

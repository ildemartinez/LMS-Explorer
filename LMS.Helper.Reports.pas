unit LMS.Helper.Reports;

interface

uses
  Generics.Collections,
  LMS._interface.LMS;

procedure ExportToExcel(const aLMSCourse: ICourse);
procedure ExportToExcelCourses(const aLMS: ILMS);

implementation

uses
  LMS.Helper.Utils,
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
        ExcelWS.fXLWS.Cells.item[aRow, 2] := aGroup.Group_Name;
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

procedure ExportToExcelCourses(const aLMS: ILMS);
var
  ExcelWS: TExcelWorkSpace;
  category: ICategory;
  course: ICourse;
begin
  ExcelWS := TExcelWorkSpace.Create;

  try
    ExcelWS.fXLWS.Cells.item[1, 1] := aLMS.Id;

    var
    aRow := 5;
    for category in aLMS.Categories do
    begin

      ExcelWS.fXLWS.Cells.item[aRow, 1] := category.Id;
      ExcelWS.fXLWS.Cells.item[aRow, 2] := category.Name;
      inc(aRow);

      for course in category.Courses do
      begin

        ExcelWS.fXLWS.Cells.item[aRow, 2] := course.Id;
        ExcelWS.fXLWS.Cells.item[aRow, 3] := course.DisplayName;
        ExcelWS.fXLWS.Cells.item[aRow, 4] := FormatDateTimeBlank(course.StartDate);
        ExcelWS.fXLWS.Cells.item[aRow, 5] := FormatDateTimeBlank(course.EndDate);

        inc(aRow);
      end;
    end;
  finally
    ExcelWS.ShowAndFinally;
  end;

end;

end.

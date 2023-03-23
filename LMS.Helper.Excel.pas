unit LMS.Helper.Excel;

interface

uses
  Excel2010;

type
  TExcelWorkSpace = class
  private
    LCID: Integer;
  public
    fXLA: TExcelApplication;
    fXLWB: TExcelWorkbook;
    fXLWS: TExcelWorkSheet;

    constructor Create;
    destructor Destroy; override;

    procedure ShowAndFinally;
  end;

implementation

uses
  Winapi.Windows,
  System.Variants;

{ TExcelWorkSpace }

constructor TExcelWorkSpace.Create;
begin
  inherited;

  fXLA := TExcelApplication.Create(nil);
  fXLA.AutoConnect := True;

  // Get "local user ID"
  LCID := GetUserDefaultLCID;

  // Hide Excel while working
  fXLA.Visible[LCID] := False;

  // no prompts
  fXLA.DisplayAlerts[LCID] := False;

  fXLWB := TExcelWorkbook.Create(nil);
  // add new workbook and connect WB wrapper
  fXLWB.ConnectTo(fXLA.Workbooks.Add(xlWBATWorksheet, LCID));

  fXLWS := TExcelWorkSheet.Create(nil);
  // connect to sheet 1
  fXLWS.ConnectTo(fXLWB.Worksheets[1] as _Worksheet);

  // select the first worksheet
  (fXLWB.Worksheets[1] as _Worksheet).Select(EmptyParam, LCID);
end;

destructor TExcelWorkSpace.Destroy;
begin
  fXLWS.free;
  fXLWB.free;
  fXLA.free;

  inherited;
end;

procedure TExcelWorkSpace.ShowAndFinally;
begin
  // failsafe always leave excel visible when we quit/disconnect.
  fXLA.Visible[LCID] := True;

  free;
end;

end.

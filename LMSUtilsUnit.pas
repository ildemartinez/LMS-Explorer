unit LMSUtilsUnit;

interface

function ShadowText(const aText: string): string;
function FormatDateTimeNever(const aDateTime: TDateTime): string;

implementation

uses
  System.SysUtils,
  dateutils;

function ShadowText(const aText: string): string;
var
  aChar: char;
begin
  for var k := 1 to Length(aText) do
  begin
    if (((k mod 2) = 0) or ((k mod 3) = 0)) then
      aChar := '*'
    else
      aChar := aText[k];

    result := result + aChar;

  end;

end;

function FormatDateTimeNever(const aDateTime: TDateTime): string;
begin
  if datetimetounix(aDateTime) = 0 then
    result := 'never'
  else
    result := DateTimeToStr(aDateTime);
end;

end.

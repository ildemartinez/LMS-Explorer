unit LMS.Helper.Utils;

interface

function ShadowText(const aText: string): string;
function FormatDateTimeNever(const aDateTime: TDateTime): string;
function FormatDateTimeBlank(const aDateTime: TDateTime): string;

// Property name has underscore so for show purpose better to change it for space
// p.e. Header column "Full name" -> property Full_name
function TextToPropertyName(const aText: string): string;

var
  hhide: boolean = true;

implementation

uses
  System.SysUtils,
  dateutils;

function ShadowText(const aText: string): string;
var
  aChar: char;
begin
  if hhide then
  begin
    for var k := 1 to Length(aText) do
    begin
      if (((k mod 3) = 0) or (((k mod 4) = 0))) then
        aChar := '*'
      else
        aChar := aText[k];

      result := result + aChar;

    end
  end
  else
    result := aText;

end;

function FormatDateTimeBlank(const aDateTime: TDateTime): string;
begin
  if datetimetounix(aDateTime) > 0 then
    result := DateTimeToStr(aDateTime)
  else
    result := '';
end;

function FormatDateTimeNever(const aDateTime: TDateTime): string;
begin
  if datetimetounix(aDateTime) = 0 then
    result := 'never'
  else
    result := DateTimeToStr(aDateTime);
end;

function TextToPropertyName(const aText: string): string;
begin
  result := StringReplace(aText, ' ', '_', [rfReplaceAll]);
end;

end.

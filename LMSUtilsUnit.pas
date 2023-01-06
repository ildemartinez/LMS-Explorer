unit LMSUtilsUnit;

interface

function ShadowText(const aText: string): string;

implementation

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

end.

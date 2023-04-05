unit LMS.Helper.RTTI;

interface

uses
  typinfo,
  System.RTTI;

function GetPropertyValue(Instance: TObject; const PropName: string): String;

implementation

var
  localContext: TRttiContext;

function GetPropertyValue(Instance: TObject; const PropName: string): String;
var
  Prop: TRttiProperty;
  aValue: TValue;
begin
  Prop := localContext.GetType(Instance.ClassType).GetProperty(PropName);
  if Assigned(Prop) and Prop.IsReadable then
  begin
    aValue := Prop.GetValue(Instance);

    case aValue.Kind of
      tkUString:
        result := aValue.AsString;
    else
      result := '';
    end;
  end;
  // else
  // raise Exception.CreateFmt('Property %s cannot be read', [PropName]);
end;

end.

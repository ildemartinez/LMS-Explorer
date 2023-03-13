unit LMSRTTIUnit;

interface

uses
  System.Rtti;

function GetPropertyValue(Instance: TObject; const PropName: string): String;

implementation

function GetPropertyValue(Instance: TObject; const PropName: string): String;
var
  Context: TRttiContext;
  Prop: TRttiProperty;
  aValue: TValue;
begin
  Prop := Context.GetType(Instance.ClassType).GetProperty(PropName);
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

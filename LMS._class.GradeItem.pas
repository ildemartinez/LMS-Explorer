unit LMS._class.GradeItem;

interface

uses
  System.JSON,
  Generics.Collections,

  LMS._interface.LMS;

type
  TGradeItem = class(TInterfacedObject, IGradeItem)
  private
    fItemName: string;
    function getItemname: string;
    procedure setItemName(const Value: string);
  public
    property ItemName: string read getItemname write setItemName;
  end;

implementation

{ TGradeItem }

function TGradeItem.getItemname: string;
begin
  result := fItemName;
end;

procedure TGradeItem.setItemName(const Value: string);
begin
  fItemName := Value;
end;

end.

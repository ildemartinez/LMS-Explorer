unit LMS._class.Section;

interface

uses
  Generics.Collections,

  LMS._interface.LMS;

type
  TSection = class(TInterfacedObject, ISection)
  private
    fName: string;
    fModules: TList<IModule>;

    function GetName: string;
    procedure SetName(const Value: string);
    function GetModules: TList<IModule>;
  public
    constructor Create;
  end;

implementation

{ TSection }

constructor TSection.Create;
begin
  fModules := TList<IModule>.Create;
end;

function TSection.GetModules: TList<IModule>;
begin
  result := fModules;
end;

function TSection.GetName: string;
begin
  result := fName;
end;

procedure TSection.SetName(const Value: string);
begin
  fName := Value;
end;

end.

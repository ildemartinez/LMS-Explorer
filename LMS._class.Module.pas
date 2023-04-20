unit LMS._class.Module;

interface

uses
  LMS._interface.LMS;

type
  TModule = class(TInterfacedObject, IModule)
  private
    fName: string;
    fModType: TModType;

    function GetName: string;
    function GetModType: TModType;
    procedure SetModName(const Value: string);
    procedure SetName(const Value: string);
  end;

implementation

uses
  LMS.Helper.Log;

function TModule.GetModType: TModType;
begin
  result := fModType;
end;

function TModule.GetName: string;
begin
  result := fName;
end;

procedure TModule.SetModName(const Value: string);
begin
  if Value = 'forum' then
    fModType := mnforum
  else if Value = 'label' then
    fModType := mnlabel
  else if Value = 'resource' then
    fModType := mnresource
  else
  begin
    fModType := mnunknow;
    Log(Value);
  end;

end;

procedure TModule.SetName(const Value: string);
begin
  fName := Value;
end;

end.

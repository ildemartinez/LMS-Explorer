unit LMS._class.Module;

interface

uses
  System.JSON,
  Generics.Collections,

  LMS._interface.LMS;

type
  TModule = class(TInterfacedObject, IModule)
  private
    fName: string;
    fSection: ISection;
    fModType: TModType;
    fContents: TList<IContent>;

    function GetName: string;
    function GetModType: TModType;
    procedure SetModName(const Value: string);
    procedure SetName(const Value: string);
    function GetContents: TList<IContent>;
    function GetSection : ISection;
  public
    constructor Create(const Section: ISection);
    destructor Destroy; override;
  end;

implementation

uses
  LMS.Helper.Log;

constructor TModule.Create(const Section: ISection);
begin
  fSection := Section;
  fContents := TList<IContent>.Create;
end;

destructor TModule.Destroy;
begin
  fContents.free;

  inherited;
end;

function TModule.GetContents: TList<IContent>;
begin
  result := fContents;
end;

function TModule.GetModType: TModType;
begin
  result := fModType;
end;

function TModule.GetName: string;
begin
  result := fName;
end;

function TModule.GetSection: ISection;
begin
result := fsection;
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

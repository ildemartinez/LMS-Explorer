unit LMS._class.Network;

interface

uses
  Generics.Collections,

  LMS._interface.LMS,
  LMS._class.LMS,
  lmsnetworkunit;

type

  TLMSNetwork = class
  private
    fLMSList: TList<ILMS>;
    function GetLMS(index: integer): ILMS;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(aLMS: ILMS);
    function Count: cardinal;

    property Items[index: integer]: ILMS read GetLMS; default;
  end;

function GetGlobalNetwork: TLMSNetwork;

implementation

var
  _GlobalLMSNetWork: TLMSNetwork;

function GetGlobalNetwork: TLMSNetwork;
begin
  if _GlobalLMSNetWork = nil then
    _GlobalLMSNetWork := TLMSNetwork.Create;

  result := _GlobalLMSNetWork
end;

procedure TLMSNetwork.Add(aLMS: ILMS);
begin
  fLMSList.Add(aLMS);
end;

function TLMSNetwork.Count: cardinal;
begin
  result := fLMSList.Count;
end;

constructor TLMSNetwork.Create;
begin
  fLMSList := TList<ILMS>.Create;
end;

destructor TLMSNetwork.Destroy;
begin
  fLMSList.free;

  inherited;
end;

function TLMSNetwork.GetLMS(index: integer): ILMS;
begin
  result := fLMSList[index];
end;

end.

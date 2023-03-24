unit LMS._class.Network;

interface

uses
  Generics.Collections,
  lmsnetworkunit;

type

  TLMSNetwork = class
  private
    fLMSList: TList<TLMS>;
    function GetLMS(index: integer): TLMS;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(aLMS: TLMS);
    function Count: cardinal;

    property Items[index: integer]: TLMS read GetLMS; default;
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

procedure TLMSNetwork.Add(aLMS: TLMS);
begin
  fLMSList.Add(aLMS);
end;

function TLMSNetwork.Count: cardinal;
begin
  result := fLMSList.Count;
end;

constructor TLMSNetwork.Create;
begin
  fLMSList := TList<TLMS>.Create;
end;

destructor TLMSNetwork.Destroy;
begin
  fLMSList.free;

  inherited;
end;

function TLMSNetwork.GetLMS(index: integer): TLMS;
begin
  result := fLMSList[index];
end;

end.

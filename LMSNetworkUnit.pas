unit LMSNetworkUnit;

interface

uses
  Generics.Collections, dialogs, sysutils;

type

  TLMS = class
  public
    id: string;
    url: string;
    user: string;
    password: string;
    service: string;
  end;

  TLMSNetwork = class
  private
    fLMSList: TList<TLMS>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure add(aLMS: TLMS);
    function count : cardinal;
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

{ TLMSNetwork }

procedure TLMSNetwork.add(aLMS: TLMS);
begin
  fLMSList.add(aLMS);
end;

function TLMSNetwork.count: cardinal;
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

end.

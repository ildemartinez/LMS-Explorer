unit LMSNetworkUnit;

interface

uses
  System.Classes,
  Generics.Collections, dialogs, sysutils, LMSRestmoodleunit;

type

  TLMSCategory = class
  public
    id: cardinal;
    fparent: cardinal;
  end;

  TLMS = class(TComponent)
  public
    id: string;
    url: string;
    user: string;
    password: string;
    service: string;

    categories: TList<TLMSCategory>;

    aLMSConnection: TLMSRestMoodle;

    constructor Create(Owner: TComponent); override;

    procedure Connect;
    function connected: boolean;

    function CategoriesLevel(level: cardinal): cardinal;
    function getcategorynbydepth(n: cardinal; depth: cardinal): TLMSCategory;

    procedure GetCategories;
  end;

  TLMSNetwork = class
  private
    fLMSList: TList<TLMS>;
    function GetLMS(index: integer): TLMS;
  public
    constructor Create;
    destructor Destroy; override;
    procedure add(aLMS: TLMS);
    function count: cardinal;

    property item[index: integer]: TLMS read GetLMS;
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

  // if autoconnect is set ... future feature ->
  aLMS.Connect;
end;

function TLMSNetwork.count: cardinal;
begin
  result := fLMSList.count;
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
  result := fLMSList.items[index];
end;

{ TLMS }

function TLMS.CategoriesLevel(level: cardinal): cardinal;
var
  k: integer;
begin
  result := 0;

  for k := 0 to categories.count - 1 do
  begin
    if categories.items[k].fparent = 0 then
      inc(result)
  end;
end;

procedure TLMS.Connect;
begin
  self.aLMSConnection.Connect;
end;

function TLMS.connected: boolean;
begin
  result := aLMSConnection.connected;
end;

constructor TLMS.Create(Owner: TComponent);
begin
  aLMSConnection := TLMSRestMoodle.Create(self);

  categories := TList<TLMSCategory>.Create;
end;

procedure TLMS.GetCategories;

begin
  self.aLMSConnection.GetCategories;
end;

function TLMS.getcategorynbydepth(n, depth: cardinal): TLMSCategory;
var
  k: cardinal;
begin
  for k := 0 to categories.count - 1 do
  begin
    if (n = 0) and (categories.Items[k]. then result :=

  end;
end;

{ TLMSCategory }

end.

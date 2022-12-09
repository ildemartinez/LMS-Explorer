unit LMSNetworkUnit;

interface

uses
  System.Classes,
  Generics.Collections, dialogs, sysutils, LMSRestmoodleunit;

type

  TLMSCategory = class
  public
    id: cardinal;
    name: string;
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

    function getcategorisbyparentcount(parent: cardinal): cardinal;
    function getcategoryidbyparent(index, parent: cardinal): cardinal;
    function GetCategoryById(id: cardinal): TLMSCategory;

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

function TLMS.getcategoryidbyparent(index, parent: cardinal): cardinal;
var
  cat: TLMSCategory;
begin
  result := 0;

  for cat in categories do
  begin
    if (cat.fparent = parent) then
    begin
      if index = 0 then
      begin
        result := cat.id;
        break;
      end
      else
        dec(index);
    end;
  end;
end;

function TLMS.getcategorisbyparentcount(parent: cardinal): cardinal;
var
  cat: TLMSCategory;
begin
  result := 0;

  for cat in categories do
  begin
    if (cat.fparent = parent) then
      inc(result);
  end;
end;

function TLMS.GetCategoryById(id: cardinal): TLMSCategory;
var
  cat: TLMSCategory;
begin
  for cat in categories do
  begin
    if (cat.id = id) then
    begin
      result := cat;
      break;
    end;
  end;

end;

end.

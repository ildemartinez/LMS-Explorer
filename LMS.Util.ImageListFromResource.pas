unit LMS.Util.ImageListFromResource;

interface

uses
  System.Classes, Vcl.Controls;

type
  {
    Esta lista devuelve las imágenes de los recursos cargados desde cualquier unidad
  }
  TISMImageListFromResource = class(TImageList)
  private
    FLoadedBitmaps: TStringList;
  public
    constructor Create(Onwer: TComponent); override;
    destructor Destroy; override;
    function GetImageIndexByName(const aImageName: String): integer;
  end;

function GetGlobalImageListFromResource: TISMImageListFromResource;

implementation

uses
  Winapi.windows, forms, Vcl.Graphics;

var
  _globalimagelistfromresource: TISMImageListFromResource;

function GetGlobalImageListFromResource: TISMImageListFromResource;
begin
  if _globalimagelistfromresource = nil then
    _globalimagelistfromresource := TISMImageListFromResource.Create
      (application);

  result := _globalimagelistfromresource;

end;

constructor TISMImageListFromResource.Create(Onwer: TComponent);
begin
  inherited;

  FLoadedBitmaps := TStringList.Create;
  Height := 16;
  Width := 16;

end;

destructor TISMImageListFromResource.Destroy;
begin
  FLoadedBitmaps.free;

  inherited;
end;

function TISMImageListFromResource.GetImageIndexByName(const aImageName
  : String): integer;
var
  aBitmap: TBitmap;
begin
  result := FLoadedBitmaps.IndexOf(aImageName);
  if result >= 0 then
    exit
  else
  begin
    if (FindResource(hInstance, PChar(aImageName), RT_BITMAP) <> 0) then
    begin
      aBitmap := TBitmap.Create;
      aBitmap.LoadFromResourceName(hInstance, aImageName);
      add(aBitmap, nil);
      result := FLoadedBitmaps.add(aImageName);
    end;
  end;
end;

initialization

_globalimagelistfromresource := nil;

end.

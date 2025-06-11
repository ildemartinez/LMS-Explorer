unit LMS.Helper.Images;

interface

uses
  System.Classes, Vcl.Controls;

type
  {
    Esta lista devuelve las im�genes de los recursos cargados desde cualquier unidad
  }
  TISMImageListFromResource = class(TImageList)
  private
    FLoadedBitmaps: TStringList;
  public
    constructor Create(Onwer: TComponent); override;
    destructor Destroy; override;
    function GetImageIndexByName(const aImageName: String): integer;
    function GetImageIndexByMimeType(const MimeType: String): integer;
  end;

function GetGlobalImageListFromResource: TISMImageListFromResource;

implementation

uses
  Winapi.windows, forms, Vcl.Graphics;

var
  _globalimagelistfromresource: TISMImageListFromResource;

function MimeTypeToImageName(const MimeType: string): string;
begin
  if MimeType = 'application/pdf' then
    result := 'res_modtype_pdf'
  else if MimeType = 'application/zip' then
    result := 'res_modtype_zip'
  else if MimeType = 'text/plain' then
    result := 'res_modtype_text'
  else if MimeType = 'application/json' then
    result := 'res_modtype_json';
end;

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

function TISMImageListFromResource.GetImageIndexByMimeType(const MimeType
  : String): integer;
begin
  result := GetImageIndexByName(MimeTypeToImageName(MimeType))
end;

function TISMImageListFromResource.GetImageIndexByName(const aImageName
  : String): integer;
var
  aBitmap: TBitmap;
begin
  result := FLoadedBitmaps.IndexOf(aImageName);
  if result < 0 then
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

unit LMS._class.Content;

interface

uses
  Generics.Collections,

  LMS._interface.LMS;

type
  TContent = class(TInterfacedObject, IContent)
  private
    fFileName: String;
    fFileType: string;
    fmimetype: string;
    fFileURL: string;
    fModule: IModule;

    procedure SetFilename(const value: string);
    function GetFileName: string;
    function GetFileURL: string;
    function GetMiMeType: string;
    procedure SetFileURL(const value: string);
    procedure SetMimeType(const value: string);
    function GetModule: IModule;
    function GetFileType: string;
    procedure SetFileType(const value: string);
  public
    constructor Create(const aModule: IModule);
  end;

implementation

constructor TContent.Create(const aModule: IModule);
begin
  fModule := aModule;
end;

function TContent.GetFileName: string;
begin
  result := fFileName;
end;

function TContent.GetFileType: string;
begin
  result := fFileType;
end;

function TContent.GetFileURL: string;
begin
  result := fFileURL;
end;

function TContent.GetMiMeType: string;
begin
  result := fmimetype;
end;

function TContent.GetModule: IModule;
begin
  result := fModule;
end;

procedure TContent.SetFilename(const value: string);
begin
  fFileName := value;
end;

procedure TContent.SetFileType(const value: string);
begin
  fFileType := value;
end;

procedure TContent.SetFileURL(const value: string);
begin
  fFileURL := value;
end;

procedure TContent.SetMimeType(const value: string);
begin
  fmimetype := value;
end;

end.

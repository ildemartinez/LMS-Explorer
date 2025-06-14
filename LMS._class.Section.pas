unit LMS._class.Section;

interface

uses
  Generics.Collections,

  LMS._interface.LMS;

type
  TSection = class(TInterfacedObject, ISection)
  strict private
    fCourse: ICourse;
    fModules: TList<IModule>;
    fName: string;
    function GetModules: TList<IModule>;
    function GetName: string;
    procedure SetName(const Value: string);
  public
    /// <summary>TSection.Create
    /// </summary>
    /// <param name="Course"> (ICourse) </param>
    constructor Create(const Course: ICourse);
  end;

implementation

constructor TSection.Create(const Course: ICourse);
begin
  fCourse := Course;

  fModules := TList<IModule>.Create;
end;

function TSection.GetModules: TList<IModule>;
begin
  result := fModules;
end;

function TSection.GetName: string;
begin
  result := fName;
end;

procedure TSection.SetName(const Value: string);
begin
  fName := Value;
end;

end.

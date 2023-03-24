unit LMS._interface.Course;

interface

uses
  LMS._interface.LMS;

type
  ILMSCourse = interface
    ['{5B363A62-53F5-4156-8920-D6338355446D}']
    function GetLMS: ILMS;
    function GetId: cardinal;

    property LMS: ILMS read GetLMS;
    property Id: cardinal read GetId;
  end;

implementation

end.

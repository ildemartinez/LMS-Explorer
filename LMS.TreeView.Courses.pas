unit LMS.TreeView.Courses;

interface

uses
  System.SysUtils,
  System.Classes,

  Generics.Collections,

  VirtualTrees,

  LMS.Helper.Log,
  LMS.TreeView.Custom,
  LMS._interface.LMS;

type
  TLMSCoursesTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMS: ILMS;
    procedure SetLMS(const Value: ILMS);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

  protected
    procedure DoDblClkCourse(const Course: ICourse); override;
  public
    constructor Create(Owner: TComponent); override;

    property LMS: ILMS read fLMS write SetLMS;
  end;

implementation

uses
  VirtualTrees.Types,
  VirtualTrees.BaseTree,

  LMS.Helper.FormFactory,
  LMS.Helper.Utils;

constructor TLMSCoursesTreeView.Create(Owner: TComponent);
begin
  inherited;

  OnGetText := MyDoGetText;

  with Header do
  begin
    Columns.Clear;
    Columns.Add.text := 'Shortname';
    Columns.Add.text := 'Fullname';
    Columns.Add.text := 'Start Date';
    Columns.Add.text := 'End Date';
    Columns.Add.text := 'Time Created';
    Columns.Add.text := 'Time Modified';

    Options := Options + [hovisible, hoAutoSpring, hoAutoResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;
  end;

end;

procedure TLMSCoursesTreeView.DoDblClkCourse(const Course: ICourse);
begin
  inherited;

  ViewForm(Course);
end;

procedure TLMSCoursesTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data: PTreeData;
begin
  inherited;

  data := GetNodeData(Node);
  data^.node_type := ntCourse;
  data^.Course := fLMS.FlatCourses[Node.Index];
end;

procedure TLMSCoursesTreeView.MyDoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  case Column of
    0:
      CellText := data^.Course.shortname;
    1:
      CellText := data^.Course.FullName;
    2:
      CellText := FormatDateTimeBlank(data^.Course.StartDate);
    3:
      CellText := FormatDateTimeBlank(data^.Course.EndDate);
    4:
      CellText := FormatDateTimeBlank(data^.Course.TimeCreated);
    5:
      CellText := FormatDateTimeBlank(data^.Course.TimeModified);
  end;
end;

procedure TLMSCoursesTreeView.SetLMS(const Value: ILMS);
begin
  fLMS := Value;

  RootNodeCount := fLMS.FlatCourses.Count;

  Header.AutoFitColumns(false, smaAllColumns, 0);
end;

end.

unit LMS.TreeView.UserCourses;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Generics.Collections,
  vcl.Menus,
  winapi.messages,

  VirtualTrees,
  LMS.TreeView.Custom,

  LMS._interface.LMS;

type

  TLMSUserCoursesTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fCourses: TList<ICourse>;

    procedure setCourses(const Value: TList<ICourse>);
    procedure MyDblClick(Sender: TObject);
    function GetSelectedCourse: ICourse;
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);

    property Courses: TList<ICourse> read fCourses write setCourses;
    property SelectedCourse: ICourse read GetSelectedCourse;

  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,

  LMS.Helper.FormFactory,
  LMS.Helper.Consts,
  LMS.Helper.Browser;

constructor TLMSUserCoursesTreeView.Create(Owner: TComponent);
begin
  inherited;

  NodeDataSize := SizeOf(TTreeData);

  TreeOptions.PaintOptions := TreeOptions.PaintOptions -
    [toShowRoot, toShowTreeLines, toHotTrack, tohidefocusrect,
    toshowhorzgridlines, toshowvertgridlines];

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toFullRowSelect];

  TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoSpanColumns];

  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];

  OnGetText := MyDoGetText;
  OnDblClick := MyDblClick;
end;

procedure TLMSUserCoursesTreeView.MyDblClick(Sender: TObject);
begin
  ViewForm(SelectedCourse);
end;

procedure TLMSUserCoursesTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);
  data^.node_type := ntCourse;
  data^.Course := Courses[Node.Index];
end;

procedure TLMSUserCoursesTreeView.FilterByText(const text: string);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
  aCompare: string;
  aParent: PVirtualNode;
begin
  aVirtualNodeEnumerator := initializednodes().GetEnumerator;

  // Not paint until finished
  BeginUpdate;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    aCompare := data^.User.FilterContent;

    if (Pos(UpperCase(text), UpperCase(aCompare)) > 0) or (text = '') then
    begin
      IsVisible[aVirtualNodeEnumerator.Current] := true;

      aParent := aVirtualNodeEnumerator.Current.Parent;
      while RootNode <> aParent do
      begin
        IsVisible[aParent] := true;
        aParent := aParent.Parent;
      end;

    end
    else
      IsVisible[aVirtualNodeEnumerator.Current] := false;

  end;

  // Please refresh
  EndUpdate;

end;

function TLMSUserCoursesTreeView.GetSelectedCourse: ICourse;
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  result := nil;

  aVirtualNodeEnumerator := SelectedNodes().GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntCourse then
    begin
      result := data^.Course;
      // refactor ... exit if not more
    end;
  end;
end;

procedure TLMSUserCoursesTreeView.MyDoGetText(Sender: TBaseVirtualTree;
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
  end;
end;

procedure TLMSUserCoursesTreeView.setCourses(const Value: TList<ICourse>);
begin

  fCourses := Value;
  Header.Columns.Clear;

  with Header do
  begin
    with Columns.add do
    begin
      text := 'Course';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'Fullname';
      Options := Options + [coAutoSpring, coResizable];
    end;

    Options := Options + [hovisible, hoAutoSpring, hoAutoResize,
      hoDblClickResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;
  end;

  RootNodeCount := Courses.Count;
  Header.AutoFitColumns(false, smaAllColumns, 0);
end;

end.

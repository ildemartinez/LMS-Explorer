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

    function GetSelectedUser: IUser;
        procedure setCourses(const Value: TList<ICourse>);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);
    procedure Refreshh;

    property Courses: TList<ICourse> read fCourses write setCourses;
    property SelectedUser: IUser read GetSelectedUser;

  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,

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
  oninitnode := MyDoInitNode;
  OnNodeClick := NodeClick;
end;

procedure TLMSUserCoursesTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  if parentdata = nil then
  begin
    data^.node_type := ntUser;
    data^.course := Courses[node.Index];
    Exclude(Node.States, vsHasChildren);
  end
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

function TLMSUserCoursesTreeView.GetSelectedUser: IUser;
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  result := nil;

  aVirtualNodeEnumerator := SelectedNodes().GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntUser then
    begin
      result := data^.User;
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

    2:
      CellText := data^.User.Last_Name;

    3:
      CellText := data^.User.Email;

  end;

end;

procedure TLMSUserCoursesTreeView.MyDoInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin

end;

procedure TLMSUserCoursesTreeView.NodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
  CtrlPressed: boolean;
  // ShiftPressed: boolean;
begin

  CtrlPressed := (GetKeyState(VK_CONTROL) and $8000) = $8000;
  // ShiftPressed := (GetKeyState(VK_SHIFT) and $8000) = $8000;

  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    case data^.node_type of
      ntUser:
        begin
          if CtrlPressed then
          begin
            OpenInBrowser(data^.User, data^.User.Course);
          end
          else
            { with TLMSForm.Create(self) do
              begin
              LMS := data^.aLMS;
              show();
              end; }
        end;
    end;
  end;
end;

procedure TLMSUserCoursesTreeView.Refreshh;
begin
  RootNodeCount := Courses.Count;
  Header.AutoFitColumns(false, smaAllColumns, 0);
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

    { with Columns.add do
      begin
      text := 'Last name';
      Options := Options + [coAutoSpring, coResizable];
      end;

      with Columns.add do
      begin
      text := 'Email';
      Options := Options + [coAutoSpring, coResizable];
      end; }

    Options := Options + [hovisible, hoAutoSpring, hoAutoResize,
      hoDblClickResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;
  end;

  RootNodeCount := Courses.Count;

  Header.AutoFitColumns(false, smaAllColumns, 0);

end;

end.

unit LMS.TreeView.CourseCategory;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  vcl.Menus,
  winapi.messages,

  VirtualTrees,
  LMS.TreeView.Custom,

  lmsnetworkunit;

type

  TLMSCategoryTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMSCategory: TLMSCategory;

    function GetSelectedUser: TLMSUser;
    procedure setLMSCategory(const Value: TLMSCategory);

  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: cardinal);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);

    property Category: TLMSCategory read fLMSCategory write setLMSCategory;
    property SelectedUser: TLMSUser read GetSelectedUser;
  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,
  generics.Collections,

  LMS.Helper.Consts,
  LMS.Helper.RTTI,
  LMS.Helper.Browser,
  LMS.Helper.Utils,
  LMS.Form.Course;

constructor TLMSCategoryTreeView.Create(Owner: TComponent);
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
  OnInitChildren := MyDoInitChildren;
  OnNodeClick := NodeClick;

end;

procedure TLMSCategoryTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  begin
    if parentdata = nil then
    begin

      data^.node_type := ntCourse;
      data^.Course := fLMSCategory.Courses[Node.Index];

      data^.Course.RefreshEnrolledUsers;

      if (data^.Course.UserGroups.count > 0) or (data^.Course.Users.count > 0)
      then
      begin
        Include(Node.States, vsHasChildren);
        Include(Node.States, vsExpanded);
      end

    end
    else if parentdata.node_type = ntCourse then
    begin
      if (parentdata.Course <> nil) and (parentdata.Course.fUserGroups.count > 0)
      then
      begin
        data^.node_type := ntGroup;
        data^.Group := parentdata.Course.fUserGroups[Node.Index];
        Include(Node.States, vsHasChildren);
        Include(Node.States, vsExpanded);
      end
      // Just users list
      else
      begin
        data^.node_type := ntUser;
        data^.User := parentdata.Course.fUsers[Node.Index];
        Exclude(Node.States, vsHasChildren);
      end;

      { if (data^.aLMS.autoconnect) then
        Include(Node.States, vsExpanded); }
    end
    else if parentdata.node_type = ntGroup then
    begin
      data^.node_type := ntUser;
      data^.User := parentdata.Group.fUsersInGroup[Node.Index];
      Exclude(Node.States, vsHasChildren);
    end;

    // Has a group
    { *if (LMSCourse <> nil) and (LMSCourse.fUserGroups.count > 0) then
      begin
      data^.node_type := ntGroup;
      data^.Group := fLMSCourse.fUserGroups[Node.Index];
      Include(Node.States, vsHasChildren);
      Include(Node.States, vsExpanded);
      end
      // Just users list
      else
      begin
      data^.node_type := ntUser;
      data^.User := fLMSCourse.fUsers[Node.Index];
      Exclude(Node.States, vsHasChildren);
      end;
      * }
    { if (data^.aLMS.autoconnect) then
      Include(Node.States, vsExpanded); }
  end
end;

procedure TLMSCategoryTreeView.FilterByText(const text: string);
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
    case data^.node_type of
      ntGroup:
        aCompare := data^.Group.FilterContent;
      ntUser:
        aCompare := data^.User.FilterContent;
    end;

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

function TLMSCategoryTreeView.GetSelectedUser: TLMSUser;
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

procedure TLMSCategoryTreeView.MyDoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  case data^.node_type of
    ntCourse:
      if Column = 0 then
        CellText := data^.Course.displayname
      else
        CellText := '';
    ntGroup:
      if Column = 1 then
        CellText := data^.Group.Group
      else
        CellText := '';
    ntUser:
      CellText := GetPropertyValue(data^.User,
        TextToPropertyName(Header.Columns[Column].text));
  end;
end;

procedure TLMSCategoryTreeView.MyDoInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: cardinal);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if data <> nil then
  begin
    case data^.node_type of
      ntCourse:
        if data^.Course.fUserGroups.count > 0 then
          ChildCount := data^.Course.fUserGroups.count
        else
          ChildCount := data^.Course.fUsers.count;
      ntGroup:
        ChildCount := data^.Group.fUsersInGroup.count;
    end;
  end;
end;

procedure TLMSCategoryTreeView.NodeClick(Sender: TBaseVirtualTree;
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
            OpenInBrowser(data^.User, data^.User.fCourse);
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

procedure TLMSCategoryTreeView.setLMSCategory(const Value: TLMSCategory);
begin
  fLMSCategory := Value;

  Header.Columns.Clear;

  with Header do
    with Columns.add do
    begin
      text := 'Course';
      Options := Options + [coAutoSpring, coResizable, coSmartResize];
    end;

  with Header do
    with Columns.add do
    begin
      text := 'Group';
      Options := Options + [coAutoSpring, coResizable, coSmartResize];
    end;

  with Header do
  begin
    with Columns.add do
    begin
      text := 'Full name';
      Options := Options + [coAutoSpring, coResizable, coSmartResize];
    end;

    with Columns.add do
    begin
      text := 'Email';
      Options := Options + [coAutoSpring, coResizable, coEditable];
    end;

    with Columns.add do
    begin
      text := 'Roles';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'Last access';
      Options := Options + [coAutoSpring, coResizable];
    end;

    Options := Options + [hovisible, hoAutoSpring, hoAutoResize,
      hoDblClickResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;

    RootNodeCount := fLMSCategory.CoursesCount;

  end;

  Header.AutoFitColumns(false, smaAllColumns, 0);
end;

end.

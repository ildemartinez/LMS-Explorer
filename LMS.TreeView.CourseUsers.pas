unit LMS.TreeView.CourseUsers;

interface

uses
  System.SysUtils,
  System.Classes,

  Generics.Collections,

  VirtualTrees,

  LMS.TreeView.Custom,
  LMS._interface.LMS;

type

  TLMSCourseUsersTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMSCourse: ICourse;
    fLMSUsers: TList<IUser>;

    procedure setLMSCourse(const Value: ICourse);

    function HasGroups: boolean;
    function GetSelectedUser: IUser;
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: cardinal);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);
    procedure Refreshh;

    property LMSCourse: ICourse read fLMSCourse write setLMSCourse;
    property SelectedUser: IUser read GetSelectedUser;
  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList,
  windows,
  dialogs,
  ShellApi,

  LMS.Helper.Consts,
  LMS.Helper.Browser,
  LMS.Helper.Utils,
  LMS.Helper.RTTI,
  LMS.Form.User,
  LMS.Form.Course;

function TLMSCourseUsersTreeView.HasGroups: boolean;
begin
  Result := fLMSCourse.UserGroups.count > 0;
end;

constructor TLMSCourseUsersTreeView.Create(Owner: TComponent);
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
  OnNodeDblClick := NodeDblClick;

end;

procedure TLMSCourseUsersTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  // if fAsTree then
  begin
    if parentdata = nil then
    begin

      // Has a group
      if (LMSCourse <> nil) and (LMSCourse.UserGroups.count > 0) then
      begin
        data^.node_type := ntGroup;
        data^.Group := fLMSCourse.UserGroups[Node.Index];
        Include(Node.States, vsHasChildren);
        Include(Node.States, vsExpanded);
      end
      // Just users list
      else
      begin
        data^.node_type := ntUser;
        data^.User := fLMSCourse.Users[Node.Index];
        Exclude(Node.States, vsHasChildren);
      end;

      { if (data^.aLMS.autoconnect) then
        Include(Node.States, vsExpanded); }
    end
    else
      case parentdata^.node_type of
        ntGroup:
          begin
            data^.node_type := ntUser;
            data^.User := parentdata.Group.UsersInGroup[Node.Index];
            Exclude(Node.States, vsHasChildren);
          end;
        { data^.node_type := ntCategory;
          data^.aLMS := parentdata^.aLMS; // cascade set lms (refactor)
          data^.Category := parentdata^.aLMS.categories.items[Node.Index];

          if parentdata^.aLMS.categories.count +
          parentdata^.aLMS.getcategorybyid(data^.Category.id).coursescount > 0
          then
          Node.States := Node.States + [vsHasChildren, vsExpanded];
          end;
        }
      end;
    {
      else
      begin
      data^.node_type := ntCourse;
      data^.aLMS := parentdata^.aLMS;
      data^.Course := parentdata^.Category.fcourses
      [Node.Index - parentdata^.Category.SubCategoriesCount];
      end;
      end;
      end
      { else
      begin
      if parentdata = nil then
      begin
      RootNodeCount := self.fCryptonetwork.count;

      if RootNodeCount > 0 then
      begin
      data^.node_type := ntnode;
      data^.nodedata := CryptoNetwork.nodes[Node.Index];
      AttachObserverToSubject(self, CryptoNetwork.nodes[Node.Index]);
      end;
      end
      end;
    }

  end;
end;

procedure TLMSCourseUsersTreeView.FilterByText(const text: string);
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
      IsVisible[aVirtualNodeEnumerator.Current] := True;

      aParent := aVirtualNodeEnumerator.Current.Parent;
      while RootNode <> aParent do
      begin
        IsVisible[aParent] := True;
        aParent := aParent.Parent;
      end;

    end
    else
      IsVisible[aVirtualNodeEnumerator.Current] := false;

  end;

  // Please refresh
  EndUpdate;

end;

function TLMSCourseUsersTreeView.GetSelectedUser: IUser;
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  Result := nil;

  aVirtualNodeEnumerator := SelectedNodes().GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntUser then
    begin
      Result := data^.User;
      // refactor ... exit if not more
    end;
  end;
end;

procedure TLMSCourseUsersTreeView.MyDoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  case data^.node_type of
    ntGroup:
      if Column = 0 then
        CellText := GetPropertyValue(TObject(data^.Group),
          TextToPropertyName(Header.Columns[Column].text))
        // CellText := data^.Group.fname
      else
        CellText := '';
    ntUser:
      CellText := GetPropertyValue(TObject(data^.User),
        TextToPropertyName(Header.Columns[Column].text));
  end;
end;

procedure TLMSCourseUsersTreeView.MyDoInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: cardinal);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if data <> nil then
  begin
    case data^.node_type of
      ntGroup:
        ChildCount := data^.Group.UsersInGroup.count;
    end;
  end;
end;

procedure TLMSCourseUsersTreeView.NodeClick(Sender: TBaseVirtualTree;
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

procedure TLMSCourseUsersTreeView.NodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin

  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    case data^.node_type of
      ntUser:
        begin
          with TLMSUserForm.Create(self) do
          begin
            User := data^.User;
            show();
          end;
        end;
    end;
  end;
end;

procedure TLMSCourseUsersTreeView.Refreshh;
begin
  RootNodeCount := fLMSUsers.count;
end;

procedure TLMSCourseUsersTreeView.setLMSCourse(const Value: ICourse);

  procedure CreateColums;
  begin
    with Header do
    begin
      with Columns.add do
      begin
        text := 'Full name';
        Options := Options + [coAutoSpring, coResizable, coSmartResize];
      end;

      with Columns.add do
      begin
        text := 'First name';
        Options := Options + [coAutoSpring, coResizable, coSmartResize];
      end;
      with Columns.add do
      begin
        text := 'Last name';
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
    end;
  end;

begin
  fLMSCourse := Value;

  fLMSCourse.RefreshEnrolledUsers;

  Header.Columns.Clear;

  // Create group column if has groups defined in course
  if HasGroups then
  begin
    with Header do
      with Columns.add do
      begin
        text := 'Group';
        Options := Options + [coAutoSpring, coResizable, coSmartResize];
      end;

    CreateColums;

    RootNodeCount := fLMSCourse.UserGroups.count;
  end
  // Not has groups so only shows users
  else
  begin
    CreateColums;

    RootNodeCount := 0;
    RootNodeCount := fLMSCourse.Users.count;
  end;

  Header.AutoFitColumns(false, smaAllColumns, 0);

end;

end.

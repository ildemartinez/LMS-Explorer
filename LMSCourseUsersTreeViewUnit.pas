unit LMSCourseUsersTreeViewUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  vcl.Menus,
  winapi.messages,

  VirtualTrees,
  LMSCustomLMSVirtualStringTreeUnit,

  lmsnetworkunit,
  LMSPopupMenuUnit;

type

  TLMSCourseUsersTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMSCourse: TLMSCourse;
    fLMSUsers: TLMSUsers;

    procedure setLMSCourse(const Value: TLMSCourse);

    function HasGroups: boolean;
    function GetSelectedUser: TLMSUser;

  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: cardinal);
    procedure MyDoInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);
    procedure Refreshh;

    property LMSCourse: TLMSCourse read fLMSCourse write setLMSCourse;
    property SelectedUser: TLMSUser read GetSelectedUser;
  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,
  LMSFormUnit,
  generics.Collections,
  LMSConstsUnit,
  LMSCourseFormUnit,
  LMSLogUnit,
  LMSBrowserHelperunit,
  LMSUtilsUnit;

function TLMSCourseUsersTreeView.HasGroups: boolean;
begin
  result := fLMSCourse.fUserGroups.count > 0;
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
  oninitnode := MyDoInitNode;
  OnNodeClick := NodeClick;

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
      if (LMSCourse <> nil) and (LMSCourse.fUserGroups.count > 0) then
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

      { if (data^.aLMS.autoconnect) then
        Include(Node.States, vsExpanded); }
    end
    else
      case parentdata^.node_type of
        ntGroup:
          begin
            data^.node_type := ntUser;
            data^.User := parentdata.Group.fUsersInGroup[Node.Index];
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

function TLMSCourseUsersTreeView.GetSelectedUser: TLMSUser;
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
        CellText := data^.Group.fname
      else
        CellText := '';
    ntUser:
      if HasGroups then
      begin
        case Column of
          0:
            CellText := '';
          1:
            CellText := data^.User.fFullName;
          2:
            CellText := data^.User.Email;
          3:
            CellText := data^.User.froles;
          4:
            CellText := FormatDateTimeNever(data^.User.flastcourseaccess);
        end;
      end
      else
        case Column of
          0:
            CellText := data^.User.fFullName;
          1:
            CellText := data^.User.Email;
          2:
            CellText := data^.User.froles;
          3:
            CellText := FormatDateTimeNever(data^.User.flastcourseaccess);
        end;

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
        ChildCount := data^.Group.fUsersInGroup.count;
    end;
  end;
end;

procedure TLMSCourseUsersTreeView.MyDoInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin

end;

{ procedure TLMSCourseUsersTreeView.MyDoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
  var
  data: PTreeData;
  begin
  data := GetNodeData(Node);

  case data^.node_type of
  ntLMS:
  begin
  if not data^.aLMS.connected then
  begin
  TargetCanvas.Font.Color := clGray;
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic]
  - [fsBold];
  end
  else
  begin
  TargetCanvas.Font.Color := clBlack;
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold] -
  [fsItalic];
  end;
  end
  end;
  end; }

{ procedure TLMSCourseUsersTreeView.MyGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: System.UITypes.TImageIndex);
  var
  data: PTreeData;
  begin
  data := GetNodeData(Node);

  if (Kind <> ikstate) then
  begin
  if fAsTree = true then
  begin
  if (data^.node_type = ntroot) and (Column = -1) then
  ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
  ('PROJECT')
  else if (data^.node_type = ntnode) then
  ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
  ('NODE_BTC');
  end
  else
  begin
  if (data^.node_type = ntLMS) and (Column = -1) then
  ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
  ('res_lms')
  else if (data^.node_type = ntCourse) then // and (Column = 0) then
  ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('MM');
  end;
  end;
}

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
            OpenInBrowser(data^.User, data^.User.fCourse);
          end
          else
            { with TLMSForm.Create(self) do
              begin
              LMS := data^.aLMS;
              show();
              end; }
        end;
      { ntCategory:
        begin
        if CtrlPressed then
        begin
        OpenInBrowser(data^.Category);
        end
        end;
        ntCourse:
        begin
        if CtrlPressed then
        begin
        OpenInBrowser(data^.Course);
        end
        else if ShiftPressed then
        begin
        OpenUsersInBrowser(data^.Course);
        end
        else
        with TLMSCourseForm.Create(self) do
        begin
        aCourse := data^.Course;
        show();
        end;
        end;
        end; }
      { else if data^.node_type = ntnetwork then
        begin
        with TNetworkForm.Create(self) do
        begin
        Network := data^.networkdata;
        show();
        end;
        end; }

    end;

  end;
end;

procedure TLMSCourseUsersTreeView.Refreshh;
begin
  RootNodeCount := fLMSUsers.count;
end;

procedure TLMSCourseUsersTreeView.setLMSCourse(const Value: TLMSCourse);

  procedure CreateColums;
  begin
    with Header do
    begin
      with Columns.add do
      begin
        text := 'FullName';
        Options := Options + [coAutoSpring, coResizable, coSmartResize];
      end;

      with Columns.add do
      begin
        text := 'Email';
        Options := Options + [coAutoSpring, coResizable,coEditable];
      end;

      with Columns.add do
      begin
        text := 'Rol';
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

    RootNodeCount := fLMSCourse.fUserGroups.count;
  end
  // Not has groups so only shows users
  else
  begin
    CreateColums;

    RootNodeCount := 0;
    RootNodeCount := fLMSCourse.fUsers.count;
  end;

  Header.AutoFitColumns(false, smaAllColumns, 0);

end;

end.

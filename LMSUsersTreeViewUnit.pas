unit LMSUsersTreeViewUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  vcl.Menus,
  {*
    isubjectunit,

    NodeObserverPattern,
    *}
  VirtualTrees,

  winapi.messages,
  lmsnetworkunit,
  LMSPopupMenuUnit;

type
  TNodeTypes = (ntGroup, ntUser);

  TTreeData = { packed } record

    // aLMS: tlms; // Pointer to LMS structure
    case node_type: TNodeTypes of
      ntGroup:
        (Group: TLMSUserGroup);
      ntUser:
        (User: TLMSUser);

  end;

  PTreeData = ^TTreeData;

  TLMSUsersTreeView = class(TCustomVirtualStringTree)
    // , INetworkObserver,    INodeObserver)
  private
    // fAsTree: boolean;
    fLMSNetwork: TLMSNetwork;
    fLMSCourse: TLMSCourse;

    // fPopupMenu: TLMSPopupMenu;
    procedure setLMSNetwork(const Value: TLMSNetwork);
    procedure setLMSCourse(const Value: TLMSCourse);
    // fCryptonetwork: TBTCNetwork;
    // procedure SetAsTree(const Value: boolean);

    // procedure setCryptoNetwork(const Value: TBTCNetwork);
    function HasGroups: boolean;
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    // procedure MenuItemClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    { procedure MenuItemClickGetPeers(Sender: TObject);
      procedure MyDoGetPopupmenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const P: TPoint; var AskParent: boolean;
      var PopupMenu: TPopupMenu);
    }
    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: cardinal);
    procedure MyDoPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
      var ImageIndex: TImageIndex);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

    procedure HandleMouseDblClick(var Message: TWMMouse;
      const HitInfo: THitInfo); override;
    {
      procedure Notification(AComponent: TComponent;
      Operation: TOperation); override; }
  public
    constructor Create(Owner: TComponent); override;
    // I
    // procedure DoNotify(const msgtype: TMSGType; const aNode: INode);
    // I
    // procedure NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
    // procedure NodeConnected(aBTCAgent: TBTCPeerNode);
    procedure FilterByText(const text: string);

    property LMSNetwork: TLMSNetwork read fLMSNetwork write setLMSNetwork;
    property LMSCourse: TLMSCourse read fLMSCourse write setLMSCourse;

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
  LMS.Util.ImageListFromResource,
  LMSBrowserHelperunit,
  LMSUtilsUnit;

function TLMSUsersTreeView.HasGroups: boolean;
begin
  result := fLMSCourse.fUserGroups.count > 0;
end;

constructor TLMSUsersTreeView.Create(Owner: TComponent);
var
  aMenuItem: TMenuItem;
begin
  inherited;

  NodeDataSize := SizeOf(TTreeData);

  PopupMenu := TLMSPopupMenu.Create(self);
  // por el momento ponemos aquí las acciones
  { aMenuItem := TMenuItem.Create(self);
    aMenuItem.caption := 'Connect';
    aMenuItem.OnClick := MenuItemClick;
    PopupMenu.items.add(aMenuItem); }
  //

  aMenuItem := TMenuItem.Create(self);
  aMenuItem.caption := 'Locate in LMS';
  aMenuItem.OnClick := MenuItem2Click;
  PopupMenu.items.add(aMenuItem);

  // TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +    [toRightClickSelect];
  // := TreeOptions.SelectionOptions +    [toRightClickSelect, tomultiselect];

  TreeOptions.PaintOptions := TreeOptions.PaintOptions -
    [toShowRoot, toShowTreeLines] + [toHotTrack, tohidefocusrect,
    toshowhorzgridlines, toshowvertgridlines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toFullRowSelect];
 // Header.Options := Header.Options - [hocolumnresize];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];

   Header.Options := header.Options + [hoAutoResize];

  OnGetText := MyDoGetText;
  OnInitChildren := MyDoInitChildren;
  OnNodeClick := NodeClick;
  OnPaintText := MyDoPaintText;

  {
    OnGetPopupMenu := MyDoGetPopupmenu; }
  OnGetImageIndex := MyGetImageIndex;
  Images := GetGlobalImageListFromResource();
end;

procedure TLMSUsersTreeView.DoInitNode(Parent, Node: PVirtualNode;
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

      if LMSCourse.fUserGroups.count > 0 then
      begin
        data^.node_type := ntGroup;
        data^.Group := fLMSCourse.fUserGroups[Node.Index];
        Include(Node.States, vsHasChildren);
        Include(Node.States, vsExpanded);
      end
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

procedure TLMSUsersTreeView.FilterByText(const text: string);
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
    { case data^.node_type of
      ntLMS:
      aCompare := data^.aLMS.id;
      ntCategory:
      aCompare := data^.Category.name;
      ntCourse:
      aCompare := data^.Course.FilterContent;

      end; }

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

procedure TLMSUsersTreeView.HandleMouseDblClick(var Message: TWMMouse;
  const HitInfo: THitInfo);
begin
  DoNodeDblClick(HitInfo);
end;

procedure TLMSUsersTreeView.MenuItem2Click(Sender: TObject);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    { case data^.node_type of
      ntLMS:
      OpenInBrowser(data^.aLMS);
      ntCategory:
      OpenInBrowser(data^.Category);
      ntCourse:
      OpenInBrowser(data^.Course);
      end; }
  end;
end;
{
  procedure TLMSUsersTreeView.MenuItemClick(Sender: TObject);
  var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
  begin
  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
  data := GetNodeData(aVirtualNodeEnumerator.Current);
  if data^.node_type = ntLMS then
  begin
  data^.aLMS.Connect;
  self.ReinitNode(aVirtualNodeEnumerator.Current, true);
  end
  end;
  end; }

procedure TLMSUsersTreeView.MyDoGetText(Sender: TBaseVirtualTree;
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
        end;
      end
      else
        case Column of
          0:
            CellText := data^.User.fFullName;
          1:
            CellText := data^.User.Email;
        end;

  end;
end;

procedure TLMSUsersTreeView.MyDoInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: cardinal);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if data <> nil then
  begin
    case data^.node_type of
      ntGroup:
        begin
          // data^.aLMS.GetCategories;
          // data^.aLMS.GetCourses;
          ChildCount := data^.Group.fUsersInGroup.count;
        end;
    end;
  end;
end;

procedure TLMSUsersTreeView.MyDoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  { case data^.node_type of
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
    end; }
end;

procedure TLMSUsersTreeView.MyGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: System.UITypes.TImageIndex);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if (Kind <> ikstate) then
  begin
    { if fAsTree = true then
      begin
      if (data^.node_type = ntroot) and (Column = -1) then
      ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
      ('PROJECT')
      else if (data^.node_type = ntnode) then
      ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
      ('NODE_BTC');
      end
      else
      begin }
    { if (data^.node_type = ntLMS) and (Column = -1) then
      ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
      ('res_lms')
      else if (data^.node_type = ntCourse) then // and (Column = 0) then
      ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('MM'); }
  end;
end;
{
  procedure TCryptoNetworkTreeView.NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
  begin
  if fAsTree then
  begin
  ReinitNode(GetFirst(), true);
  end
  else
  begin
  Clear;
  BeginUpdate;
  RootNodeCount := 1;
  FullExpand;
  EndUpdate;
  end;
  end;

}

procedure TLMSUsersTreeView.NodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
  CtrlPressed, ShiftPressed: boolean;
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
{
  procedure TCryptoNetworkTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
  begin
  inherited;

  if Operation = opRemove then
  if AComponent = CryptoNetwork then
  CryptoNetwork := nil;
  end;

  procedure TCryptoNetworkTreeView.SetAsTree(const Value: boolean);
  begin
  fAsTree := Value;

  // As Tree
  if Value = true then
  begin
  TreeOptions.PaintOptions := TreeOptions.PaintOptions +
  [toShowRoot, toShowTreeLines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions -
  [toextendedfocus];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleondblclick];

  end
  // as grid
  else
  begin
  TreeOptions.PaintOptions := TreeOptions.PaintOptions -
  [toShowRoot, toShowTreeLines] + [toHotTrack, tohidefocusrect,
  toshowhorzgridlines, toshowvertgridlines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
  [toFullRowSelect];
  Header.Options := Header.Options - [hocolumnresize];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];
  end;
  end;

  procedure TCryptoNetworkTreeView.setCryptoNetwork(const Value: TBTCNetwork);
  begin
  fCryptonetwork := Value;
  if Value <> nil then
  begin
  RootNodeCount := 1;

  Value.FreeNotification(self);
  Value.RegisterObserver(self);
  end;
  end;

}

procedure TLMSUsersTreeView.setLMSCourse(const Value: TLMSCourse);
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
        Width := 150;
        text := 'Group';
      end;

    self.RootNodeCount := fLMSCourse.fUserGroups.count
  end
  // Not has groups so only shows users
  else
  begin
    self.RootNodeCount := fLMSCourse.fUsers.count;
  end;

  // Build header columns
  with Header do
  begin
    with Columns.add do
    begin
      Width := 150;
      text := 'FullName';
    end;

    with Columns.add do
    begin
      Width := 150;
      text := 'Email';
    end;

    Options := Options + [hovisible, hoAutoResize, hoFullRepaintOnResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;
  end;

end;

procedure TLMSUsersTreeView.setLMSNetwork(const Value: TLMSNetwork);
begin
  fLMSNetwork := Value;

  self.RootNodeCount := fLMSNetwork.count;
end;

end.

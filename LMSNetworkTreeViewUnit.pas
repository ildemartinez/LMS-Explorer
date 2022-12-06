unit LMSNetworkTreeViewUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  vcl.Menus,
  {*  BTCNetworkUnit,
    isubjectunit,
    BTCPeerNodeUnit,
    CryptoNetworkPopupMenuUnit,
    NodeObserverPattern,
    ipeernodeunit,*}
  VirtualTrees,

  lmsnetworkunit,
  LMSPopupMenuUnit;

type
  TNodeTypes = (ntLMS, ntCategory);

  TTreeData = { packed } record
    aLMS: tlms; // Pointer to LMS structure
    case node_type: TNodeTypes of
      ntCategory:
        (categoryId: cardinal);
  end;

  PTreeData = ^TTreeData;

  TLMSNetworkTreeView = class(TCustomVirtualStringTree)
    // , INetworkObserver,    INodeObserver)
  private
    // fAsTree: boolean;
    fLMSNetwork: TLMSNetwork;

    // fPopupMenu: TLMSPopupMenu;
    procedure setLMSNetwork(const Value: TLMSNetwork);
    // fCryptonetwork: TBTCNetwork;
    // procedure SetAsTree(const Value: boolean);

    // procedure setCryptoNetwork(const Value: TBTCNetwork);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure MenuItemClick(Sender: TObject);
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
    { procedure MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
      var ImageIndex: TImageIndex); }
    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
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

    property LMSNetwork: TLMSNetwork read fLMSNetwork write setLMSNetwork;

  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList,
  dialogs,
  LMSFormUnit,
  generics.Collections;
// NodeFormUnit,
// networkformunit,
// st4makers.Util.ImageListFromResource;

constructor TLMSNetworkTreeView.Create(Owner: TComponent);
var
  aMenuItem: TMenuItem;
begin
  inherited;

  NodeDataSize := SizeOf(TTreeData);

  PopupMenu := TLMSPopupMenu.Create(self);
  // por el momento ponemos aquí las acciones
  aMenuItem := TMenuItem.Create(self);
  aMenuItem.caption := 'Connect';
  aMenuItem.OnClick := MenuItemClick;
  PopupMenu.items.add(aMenuItem);
  //

  { // por el momento ponemos aquí las acciones
    aMenuItem := TMenuItem.Create(self);
    aMenuItem.caption := 'Get Peers';
    aMenuItem.OnClick := MenuItemClickGetPeers;
    PopupMenu.items.add(aMenuItem);
    //
  }

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toRightClickSelect,
  // toLevelSelectConstraint,
  tomultiselect, toSiblingSelectConstraint];

  OnGetText := MyDoGetText;
  OnInitChildren := MyDoInitChildren;
  OnNodeDblClick := NodeDblClick;
  OnPaintText := MyDoPaintText;

  {
    OnGetPopupMenu := MyDoGetPopupmenu;
    OnGetImageIndex := MyGetImageIndex;

    Images := GetGlobalImageListFromResource(); }
end;

procedure TLMSNetworkTreeView.DoInitNode(Parent, Node: PVirtualNode;
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
      data^.node_type := ntLMS;
      data^.aLMS := fLMSNetwork.item[Node.Index];

      data^.aLMS.GetCategories;

      if data^.aLMS.getcategorisbyparentcount(0) > 0 then
        Node.States := Node.States + [vsHasChildren, vsExpanded]
    end
    else
      case parentdata^.node_type of
        ntLMS:
          begin
            data^.node_type := ntCategory;
            data^.aLMS := parentdata^.aLMS; // cascade set lms (refactor)

            // childcat := parentdata^.aLMS.getcategorisbyparent(0);
            data^.categoryId := parentdata^.aLMS.getcategoryidbyparent
              (Node.Index, 0);

            if parentdata^.aLMS.getcategorisbyparentcount(data^.categoryId) > 0
            then
              Node.States := Node.States + [vsHasChildren]; // , vsExpanded];
          end;
        ntCategory:
          begin
            data^.node_type := ntCategory;
            data^.aLMS := parentdata^.aLMS; // cascade set lms (refactor)

            data^.categoryId := parentdata^.aLMS.getcategoryidbyparent
              (Node.Index, parentdata^.categoryId);

            if parentdata^.aLMS.getcategorisbyparentcount(data^.categoryId) > 0
            then
              Node.States := Node.States + [vsHasChildren]; // , vsExpanded];
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

  {
    procedure TCryptoNetworkTreeView.DoNotify(const msgtype: TMSGType;
    const aNode: INode);
    var
    aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
    data: PTreeData;
    begin
    // todo optimizar la salida
    aVirtualNodeEnumerator := nodes.GetEnumerator;

    while aVirtualNodeEnumerator.MoveNext do
    begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntnode then
    begin
    if data^.nodedata.PeerIp = aNode.GetIP then
    InvalidateNode(aVirtualNodeEnumerator.Current)
    end;
    end;

    end;
  }
end;

procedure TLMSNetworkTreeView.MenuItemClick(Sender: TObject);
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
      data^.aLMS.Connect; // .connected := true;
    end
    { else if data^.node_type = ntnetwork then
      begin
      data^.networkdata.Connect;
      end }
  end;
end;

{
  procedure TCryptoNetworkTreeView.MenuItemClickGetPeers(Sender: TObject);
  var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
  begin
  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
  data := GetNodeData(aVirtualNodeEnumerator.Current);
  if data^.node_type = ntnode then
  begin
  data^.nodedata.GetPeers();
  end
  end;

  end;

  procedure TCryptoNetworkTreeView.MyDoGetPopupmenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: boolean; var PopupMenu: TPopupMenu);
  var
  data: PTreeData;
  begin
  data := GetNodeData(Node);

  case data^.node_type of
  ntroot:
  ;
  ntnetwork:
  ;
  ntnode:
  ;
  end;
  end;

}
procedure TLMSNetworkTreeView.MyDoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PTreeData;

begin
  data := GetNodeData(Node);

  case data^.node_type of
    ntLMS:
      begin
        CellText := data^.aLMS.id;
      end;
    ntCategory:
      begin
        CellText := data^.aLMS.GetCategoryById(data^.categoryId).name;
      end;
  end;
end;

{ else
  case Column of
  0:
  begin
  case data^.node_type of
  ntnode:
  CellText := data^.nodedata.PeerIp;
  end;
  end;
  1:
  begin
  case data^.node_type of
  ntnode:
  CellText := data^.nodedata.Agent;
  end;
  end;
  end; }

procedure TLMSNetworkTreeView.MyDoInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: cardinal);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if data <> nil then
  begin
    case data^.node_type of
      ntLMS:
        ChildCount := data^.aLMS.CategoriesLevel(0);
      ntCategory:
        begin
          ChildCount := data^.aLMS.getcategorisbyparentcount(data^.categoryId);
        end;
    end;

  end;
end;

procedure TLMSNetworkTreeView.MyDoPaintText(Sender: TBaseVirtualTree;
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
end;

{
  if data^.nodedata.Connected then
  begin
  TargetCanvas.Font.Color := clBlack;
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold] -
  [fsItalic];

  end
  else if data^.nodedata.serverconnected then
  begin

  TargetCanvas.Font.Color := clBlack;
  TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsItalic]
  - [fsBold];
  end
  else
  begin
  TargetCanvas.Font.Color := clGray;
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic]
  - [fsBold]; }

{
  procedure TCryptoNetworkTreeView.MyGetImageIndex(Sender: TBaseVirtualTree;
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
  { if (data^.node_type = ntroot) and (Column = -1) then
  ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
  ('PROJECT')
  else
  if (data^.node_type = ntnode) and (Column = 0) then
  ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
  ('NODE_BTC');
  end;

  end;
  end;

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

  procedure TCryptoNetworkTreeView.NodeConnected(aBTCAgent: TBTCPeerNode);
  begin

  end;
}

procedure TLMSNetworkTreeView.NodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
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
      with TLMSForm.Create(self) do
      begin
        LMS := data^.aLMS;
        show();
      end;
    end
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
{ TLMSNetworkTreeView }

procedure TLMSNetworkTreeView.setLMSNetwork(const Value: TLMSNetwork);
begin
  fLMSNetwork := Value;

  self.RootNodeCount := fLMSNetwork.count;
end;

end.

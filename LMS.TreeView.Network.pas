unit LMS.TreeView.Network;

interface

uses
  winapi.messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  VirtualTrees,

  LMS.TreeView.Custom,
  LMS._class.Network;

type
  TLMSNetworkTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMSNetwork: TLMSNetwork;
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure SetLMSNetwork(const Value: TLMSNetwork);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: cardinal);
    procedure MyDoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure MyBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure FilterByText(const text: string);
    procedure FullExpandLMS;
    property LMSNetwork: TLMSNetwork write SetLMSNetwork;

  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,
  generics.Collections,

  LMS.Helper.Log,
  LMS.Helper.Consts,
  LMS._interface.LMS,
  LMS.Helper.Browser,
  LMS.Form.LMS,
  LMS.Helper.FormFactory,
  LMS.Form.Category;

constructor TLMSNetworkTreeView.Create(Owner: TComponent);
begin
  inherited;

  NodeDataSize := SizeOf(TTreeData);

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toRightClickSelect];
  // := TreeOptions.SelectionOptions +    [toRightClickSelect, tomultiselect];

  OnGetText := MyDoGetText;
  OnInitChildren := MyDoInitChildren;
  OnNodeDblClick := NodeDblClick;
  OnNodeClick := NodeClick;
  OnPaintText := MyDoPaintText;
end;

procedure TLMSNetworkTreeView.DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
  aCat: ICategory;
  ind: cardinal;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  if parentdata = nil then
  begin
    data^.node_type := ntLMS;
    data^.aLMS := fLMSNetwork[Node.Index];

    if (data^.aLMS.connected) then
    begin
      data^.aLMS.GetCategories;
      data^.aLMS.GetCourses;
    end;

    Include(Node.States, vsHasChildren);

    if (data^.aLMS.autoconnect) then
      Include(Node.States, vsExpanded);
  end
  else
    case parentdata^.node_type of
      ntLMS:
        begin
          data^.node_type := ntCategory;
          data^.aLMS := parentdata^.aLMS; // cascade set lms (refactor)

          // Search main categories (parentcategory = 0)
          ind := 0;
          for aCat in parentdata^.aLMS.categories do
          begin
            if (aCat.ParentCategory = 0) then
            begin
              if (ind = Node.Index) then
              begin
                data^.Category := aCat;
                break;
              end;

              inc(ind)
            end;
          end;

          if (parentdata^.aLMS.categories.Count > 0) or (parentdata^.aLMS.getcategorybyid(data^.Category.id).coursescount > 0) then
            Node.States := Node.States + [vsHasChildren, vsExpanded];
        end;
      ntCategory:
        begin

          // It is a category
          if Node.Index < parentdata^.Category.SubCategoriesCount then
          begin
            data^.node_type := ntCategory;
            data^.aLMS := parentdata^.aLMS; // cascade set lms (refactor)
            data^.Category := parentdata^.aLMS.getcategorybyid(parentdata^.Category.id).categories.items[Node.Index];

            if parentdata^.Category.SubCategoriesCount > 0 then
              Node.States := Node.States + [vsHasChildren, vsExpanded];
          end
          else
          begin
            data^.node_type := ntCourse;
            data^.aLMS := parentdata^.aLMS;
            data^.Course := parentdata^.Category.courses[Node.Index - parentdata^.Category.SubCategoriesCount];
          end;
        end;
    end
end;

procedure TLMSNetworkTreeView.FilterByText(const text: string);
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
      ntLMS:
        aCompare := data^.aLMS.id;
      ntCategory:
        aCompare := data^.Category.name;
      ntCourse:
        aCompare := data^.Course.FilterContent;
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

procedure TLMSNetworkTreeView.FullExpandLMS;
var
  Node: PVirtualNode;
  data: PTreeData;
begin
  BeginUpdate;
  try
    Node := GetFirst;
    while Assigned(Node) do
    begin
      data := GetNodeData(Node);
      if data^.node_type = ntLMS then
      begin
        if data^.aLMS.connected then
          FullExpand(Node);
      end;

      // Selected[Node] := true;

      Node := GetNextSibling(Node);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLMSNetworkTreeView.HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
begin
  // Avoid to collapse/expand category node at dbclick
  DoNodeDblClick(HitInfo);
end;

procedure TLMSNetworkTreeView.MyBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if (PTreeData(Sender.GetNodeData(Node))^.node_type = ntLMS) then
    // TargetCanvas.Brush.Color := $00F7E6D5 ;
    // else
    TargetCanvas.Brush.Color := $00FBF2EA;

  TargetCanvas.FillRect(CellRect);

end;

procedure TLMSNetworkTreeView.MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  case data^.node_type of
    ntLMS:
      CellText := data^.aLMS.id;
    ntCategory:
      CellText := data^.Category.name;
    ntCourse:
      CellText := data^.Course.DisplayContent;
  end;
end;

procedure TLMSNetworkTreeView.MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: cardinal);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if data <> nil then
  begin
    case data^.node_type of
      ntLMS:
        begin
          data^.aLMS.GetCategoriesFromconnection;
          data^.aLMS.GetCourses;
          ChildCount := data^.aLMS.FirstLevelCategoriesCount;
        end;
      ntCategory:
        begin
          ChildCount := data^.Category.SubCategoriesCount + data^.Category.coursescount;
        end;
    end;

  end;
end;

procedure TLMSNetworkTreeView.MyDoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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
          TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic] - [fsBold];
        end
        else
        begin
          TargetCanvas.Font.Color := clBlack;
          TargetCanvas.Font.Size := 10;
          TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold] - [fsItalic];
        end;
      end
  end;
end;

procedure TLMSNetworkTreeView.NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
  CtrlPressed, ShiftPressed: boolean;
begin

  CtrlPressed := (GetKeyState(VK_CONTROL) and $8000) = $8000;
  ShiftPressed := (GetKeyState(VK_SHIFT) and $8000) = $8000;

  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    case data^.node_type of
      ntLMS:
        begin
          if CtrlPressed then
          begin
            OpenInBrowser(data^.aLMS);
          end
        end;
      ntCategory:
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

        end;
    end;
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

procedure TLMSNetworkTreeView.NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin

  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    case data^.node_type of
      ntLMS:
        begin
          Expanded[aVirtualNodeEnumerator.Current] := true;
          ViewForm(data^.aLMS);
        end;
      ntCategory:
        ViewForm(data^.Category);
      ntCourse:
        ViewForm(data^.Course);
    end;
  end;
end;

procedure TLMSNetworkTreeView.SetLMSNetwork(const Value: TLMSNetwork);
begin
  fLMSNetwork := Value;

  Rootnodecount := fLMSNetwork.Count;
end;

end.

unit LMS.TreeView.LMS;

interface

uses
  winapi.messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  VirtualTrees,

  LMS._interface.LMS,
  LMS.TreeView.Custom;

type
  TLMSTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMS: ILMS;

    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure SetLMS(const Value: ILMS);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: cardinal);
    procedure MyDoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure FilterByText(const text: string);

    property LMS: ILMS write SetLMS;
  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,
  generics.Collections,

  LMS.Helper.Consts,
  LMS.Helper.Browser,
  LMS.Form.LMS,
  LMS.Helper.FormFactory,
  LMS.Form.Category;

constructor TLMSTreeView.Create(Owner: TComponent);
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

procedure TLMSTreeView.DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  if parentdata = nil then
  begin
    data^.node_type := ntCategory;
    data^.Category := fLMS.categories.items[Node.Index];

    if (fLMS.categories.Count > 0) or (fLMS.getcategorybyid(data^.Category.id).coursescount > 0) then
      Node.States := Node.States + [vsHasChildren, vsExpanded];
  end
  else
    case parentdata^.node_type of

      ntCategory:
        begin
          // It is a category
          if Node.Index < parentdata^.Category.SubCategoriesCount then
          begin
            data^.node_type := ntCategory;
            data^.Category := fLMS.getcategorybyid(parentdata^.Category.id).categories.items[Node.Index];

            if parentdata^.Category.SubCategoriesCount > 0 then
              Node.States := Node.States + [vsHasChildren, vsExpanded];
          end
          else
          begin
            data^.node_type := ntCourse;
            data^.Course := parentdata^.Category.courses[Node.Index - parentdata^.Category.SubCategoriesCount];
          end;
        end;
    end
end;

procedure TLMSTreeView.FilterByText(const text: string);
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

procedure TLMSTreeView.HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
begin
  // Avoid to collapse/expand category node at dbclick
  DoNodeDblClick(HitInfo);
end;

procedure TLMSTreeView.MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
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

procedure TLMSTreeView.MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: cardinal);
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

procedure TLMSTreeView.MyDoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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
          TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold] - [fsItalic];
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

procedure TLMSTreeView.NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
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

procedure TLMSTreeView.SetLMS(const Value: ILMS);
begin
  fLMS := Value;

  RootNodeCount := fLMS.FirstLevelCategoriesCount;
end;

procedure TLMSTreeView.NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
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

end.

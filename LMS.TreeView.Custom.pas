unit LMS.TreeView.Custom;

interface

uses
  System.classes,
  System.UITypes,
  System.Types,
  VirtualTrees,

  LMS._interface.LMS,
  LMS._class.LMS;

type
  TNodeTypes = (ntLMS, ntCategory, ntCourse, ntGroup, ntUser, ntSection, ntModule, ntModuleOne, ntContent);

  TTreeData = { packed } record
    aLMS: ILMS; // Pointer to LMS structure
    Course: ICourse;
    User: IUser;
    Category: ICategory;
    Group: IUsersGroup;
    node_type: TNodeTypes;
    Section: ISection;
    Module: IModule;
    Content: IContent;
    // of
    // ntCategory:

    // (Category: TLMSCategory);
    // ntGroup:
    // (Group: TLMSUserGroup);
    // ntUser:
    // (User: TLMSUser);
  end;

  PTreeData = ^TTreeData;

  TLMSCustomLMSVirtualStringTree = class(TCustomVirtualStringTree)
  private
    procedure CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: System.UITypes.TImageIndex);
    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  protected
    procedure DoDblClkCourse(const Course: ICourse); virtual;
    procedure DoDblClkUser(const User: IUser); virtual;
    procedure MyBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); virtual;
  public
    constructor Create(Owner: TComponent); override;
    // Focus a selected node in tree
    procedure FocusSelectedNode;
  end;

implementation

uses
  System.SysUtils,

  VirtualTrees.Types,
  VirtualTrees.BaseTree,

  LMS.Helper.Images,
  LMS.Helper.RTTI,
  LMS.Helper.Utils;

constructor TLMSCustomLMSVirtualStringTree.Create(Owner: TComponent);
begin
  inherited;

  NodeDataSize := SizeOf(TTreeData);

  OnNodeDblClick := NodeDblClick;

  Images := GetGlobalImageListFromResource();
  OnGetImageIndex := MyGetImageIndex;
  OnCompareNodes := CompareNodes;
  OnHeaderClick := HeaderClick;
  OnBeforeCellPaint := MyBeforePaint;

  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines, toHotTrack, tohidefocusrect, toshowhorzgridlines, toshowvertgridlines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoSpanColumns];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];
end;

procedure TLMSCustomLMSVirtualStringTree.MyBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if (Node.Index mod 2 = 1) or (PTreeData(Sender.GetNodeData(Node))^.node_type = ntCategory) then
    // TargetCanvas.Brush.Color := $00F7E6D5 ;
    // else
    TargetCanvas.Brush.Color := $00FBF2EA;

  TargetCanvas.FillRect(CellRect);
end;

procedure TLMSCustomLMSVirtualStringTree.CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PTreeData;
begin
  if Column = -1 then
    exit;

  data1 := Sender.GetNodeData(Node1);
  data2 := Sender.GetNodeData(Node2);

  if (data1.node_type = data2.node_type) and (data1.node_type = ntUser) then
  begin
    Result := comparetext(GetPropertyValue(TObject(data1.User), TextToPropertyName(header.Columns.Items[header.SortColumn].Text)),
      GetPropertyValue(TObject(data2.User), TextToPropertyName(header.Columns.Items[header.SortColumn].Text)))
  end
  else if (data1.node_type = data2.node_type) and (data1.node_type = ntCourse) then
  begin
    Result := comparetext(GetPropertyValue(TObject(data1.Course), TextToPropertyName(header.Columns.Items[header.SortColumn].Text)),
      GetPropertyValue(TObject(data2.Course), TextToPropertyName(header.Columns.Items[header.SortColumn].Text)))
  end;
end;

procedure TLMSCustomLMSVirtualStringTree.DoDblClkCourse(const Course: ICourse);
begin

end;

procedure TLMSCustomLMSVirtualStringTree.DoDblClkUser(const User: IUser);
begin

end;

procedure TLMSCustomLMSVirtualStringTree.FocusSelectedNode;
begin
  if SelectedCount = 1 then
    for var aNode in SelectedNodes() do
      FocusedNode := aNode;
end;

procedure TLMSCustomLMSVirtualStringTree.HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  Sender.SortColumn := HitInfo.Column;
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;

  SortTree(0, Sender.SortDirection, false);

  // Update the sort indicator
  InvalidateColumn(HitInfo.Column);
end;

procedure TLMSCustomLMSVirtualStringTree.MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
  var ImageIndex: System.UITypes.TImageIndex);
var
  data: PTreeData;
begin
  if (Kind <> ikstate) and (Column = 0) then
  begin
    data := GetNodeData(Node);

    if (data^.node_type = ntLMS) and (Column = -1) then
      ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_lms')
    else if (data^.node_type = ntCourse) then // and (Column = 0) then
    begin
      case data^.Course.groupmode of
        // 0: ;
        // ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_groups_no_groups');
        1:
          ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_groups_separate_groups');
        2:
          ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_groups_visible_groups');
      end;

    end
    else if (data^.node_type = ntModule) then
    begin
      if data^.Module.ModType = mnresource then
        ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_modtype_pdf')
      else if data^.Module.ModType = mnlabel then
        ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_modtype_label');

    end;

  end;

end;

procedure TLMSCustomLMSVirtualStringTree.NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    case data^.node_type of
      ntCourse:
        DoDblClkCourse(data^.Course);
      ntUser:
        DoDblClkUser(data^.User);
    end;
  end;

end;

end.

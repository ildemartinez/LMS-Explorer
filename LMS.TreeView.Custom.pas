unit LMS.TreeView.Custom;

interface

uses
  System.classes,
  System.UITypes,
  VirtualTrees,

  LMS._interface.LMS,
  LMS._class.LMS;

type
  TNodeTypes = (ntLMS, ntCategory, ntCourse, ntGroup, ntUser);

  TTreeData = { packed } record
    aLMS: ILMS; // Pointer to LMS structure
    Course: ICourse;
    User: IUser;
    Category: ICategory;
    Group: IUsersGroup;
    node_type: TNodeTypes;
    // of
      // ntCategory:

      // (Category: TLMSCategory);
      // ntGroup:
      // (Group: TLMSUserGroup);
      // ntUser:
      // (User: TLMSUser);
    end;

    PTreeData = ^TTreeData;

    TLMSCustomLMSVirtualStringTree = class(TCustomVirtualStringTree)private
    procedure MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
      var ImageIndex: System.UITypes.TImageIndex);

    procedure HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
  public
    constructor Create(Owner: TComponent); override;

    // Focus a selected node in tree
    procedure FocusSelectedNode;
  end;

implementation

uses
  System.SysUtils,

  LMS.Helper.Images,
  LMS.Helper.RTTI,
  LMS.Helper.Utils;

procedure TLMSCustomLMSVirtualStringTree.CompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PTreeData;
begin
  if Column = -1 then
    exit;

  data1 := Sender.GetNodeData(Node1);
  data2 := Sender.GetNodeData(Node2);

  if (data1.node_type = data2.node_type) and (data1.node_type = ntUser) then
  begin
    Result := comparetext(GetPropertyValue(TObject(data1.User),
      TextToPropertyName(header.Columns.Items[header.SortColumn].Text)),
      GetPropertyValue(TObject(data2.User),
      TextToPropertyName(header.Columns.Items[header.SortColumn].Text)))
  end;
end;

constructor TLMSCustomLMSVirtualStringTree.Create(Owner: TComponent);
begin
  inherited;

  Images := GetGlobalImageListFromResource();
  OnGetImageIndex := MyGetImageIndex;
  OnCompareNodes := CompareNodes;
  OnHeaderClick := HeaderClick;
end;

procedure TLMSCustomLMSVirtualStringTree.FocusSelectedNode;
begin
  if SelectedCount = 1 then
    for var aNode in SelectedNodes() do
      FocusedNode := aNode;
end;

procedure TLMSCustomLMSVirtualStringTree.HeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
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

procedure TLMSCustomLMSVirtualStringTree.MyGetImageIndex
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean;
  var ImageIndex: System.UITypes.TImageIndex);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if (Kind <> ikstate) then
  begin
    if (data^.node_type = ntLMS) and (Column = -1) then
      ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
        ('res_lms')
    else if (data^.node_type = ntCourse) then // and (Column = 0) then
    begin
      case data^.Course.groupmode of
        // 0: ;
        // ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName('res_groups_no_groups');
        1:
          ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
            ('res_groups_separate_groups');
        2:
          ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
            ('res_groups_visible_groups');
      end;

    end;
  end;

end;

end.

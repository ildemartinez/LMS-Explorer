unit LMSCustomLMSVirtualStringTreeUnit;

interface

uses
  System.classes,
  System.UITypes,
  VirtualTrees,

  LMSNetworkUnit;

type
  TNodeTypes = (ntLMS, ntCategory, ntCourse, ntGroup, ntUser);

  TTreeData = { packed } record
    aLMS: tlms; // Pointer to LMS structure
    case node_type: TNodeTypes of
      ntCategory:
        (Category: TLMSCategory);
      ntCourse:
        (Course: TLMSCourse);
      ntGroup:
        (Group: TLMSUserGroup);
      ntUser:
        (User: TLMSUser);
  end;

  PTreeData = ^TTreeData;

  TLMSCustomLMSVirtualStringTree = class(TCustomVirtualStringTree)
  private
      procedure MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
      var ImageIndex: System.UITypes.TImageIndex);
  public
    constructor Create(Owner: TComponent); override;
  end;

implementation

uses
  LMS.Util.ImageListFromResource;

constructor TLMSCustomLMSVirtualStringTree.Create(Owner: TComponent);
begin
  inherited;

  Images := GetGlobalImageListFromResource();
  OnGetImageIndex := MyGetImageIndex;
end;

procedure TLMSCustomLMSVirtualStringTree.MyGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
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
        0:
          ;
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

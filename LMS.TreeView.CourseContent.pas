unit LMS.TreeView.CourseContent;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Generics.Collections,
  vcl.Menus,
  winapi.messages,

  VirtualTrees,
  LMS.TreeView.Custom,

  LMS._interface.LMS;

type

  TCourseContentTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMSUsers: TList<IUser>;
    fLMSCourse: ICourse;

    procedure setLMSCourse(const Value: ICourse);
    procedure MyGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
      var ImageIndex: System.UITypes.TImageIndex);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: cardinal);
  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);
    procedure Refreshh;

    property LMSCourse: ICourse read fLMSCourse write setLMSCourse;
  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,

  LMS.Helper.Images,
  LMS.Helper.Consts,
  LMS.Helper.Browser;

constructor TCourseContentTreeView.Create(Owner: TComponent);
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
  OnGetImageIndex := MyGetImageIndex;

end;

procedure TCourseContentTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  if parentdata = nil then
  begin
    data^.node_type := ntSection;
    data^.Section := fLMSCourse.Sections[Node.Index];

    if data^.Section.Modules.Count > 0 then
      Node.States := Node.States + [vsHasChildren, vsExpanded]
    else
      Exclude(Node.States, vsHasChildren);
  end
  else if parentdata.node_type = ntSection then
  begin
    data^.node_type := ntmodule;
    data^.Module := parentdata^.Section.Modules[Node.Index];

    if data^.Module.Contents.Count > 0 then
      Node.States := Node.States + [vsHasChildren, vsExpanded]
    else
      Exclude(Node.States, vsHasChildren);
  end
  else if parentdata.node_type = ntmodule then
  begin
    data^.node_type := ntcontent;
    data^.content := parentdata^.Module.Contents[Node.Index];

    Exclude(Node.States, vsHasChildren);
  end;

end;

procedure TCourseContentTreeView.FilterByText(const text: string);
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
    aCompare := data^.User.FilterContent;

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

procedure TCourseContentTreeView.MyDoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  case data^.node_type of
    ntSection:
      if Column = 0 then
        CellText := data^.Section.name
      else
        CellText := '';
    ntmodule:
      begin
        { if Column = 1 then
          begin
          case data^.Module.ModType of
          mnforum:
          CellText := 'Forum';
          mnlabel:
          CellText := 'Label';
          else
          CellText := 'uknown';
          end;
          end
          else }
        if Column = 1 then
          CellText := data^.Module.name
        else
          CellText := '';
      end;
    ntcontent:
      begin
        if Column = 2 then
          CellText := data^.content.MimeType
        else if Column = 3 then
          CellText := data^.content.Fileurl
        else
          CellText := '';
      end;
  end;

end;

procedure TCourseContentTreeView.MyDoInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: cardinal);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  if data <> nil then
  begin
    case data^.node_type of
      ntSection:
        begin
          ChildCount := data^.Section.Modules.Count;
        end;
      ntmodule:
        begin
          ChildCount := data^.Module.Contents.Count;
        end;
    end;
  end;
end;

procedure TCourseContentTreeView.MyGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: System.UITypes.TImageIndex);
var
  data: PTreeData;
begin
  if (Kind <> ikstate) and (Column = 1) then
  begin
    data := GetNodeData(Node);

    if (data^.node_type = ntmodule) then
    begin
      if data^.Module.ModType = mnresource then
        ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
          ('res_modtype_pdf')
      else if data^.Module.ModType = mnlabel then
        ImageIndex := GetGlobalImageListFromResource.GetImageIndexByName
          ('res_modtype_label');
    end;
  end;
end;

procedure TCourseContentTreeView.Refreshh;
begin
  RootNodeCount := fLMSUsers.Count;
  Header.AutoFitColumns(false, smaAllColumns, 0);
end;

procedure TCourseContentTreeView.setLMSCourse(const Value: ICourse);
begin
  fLMSCourse := Value;

  Header.Columns.Clear;

  with Header do
  begin
    with Columns.add do
    begin
      text := 'Section';
      Options := Options + [coAutoSpring, coResizable];
    end;

    { with Columns.add do
      begin
      text := 'Type';
      Options := Options + [coAutoSpring, coResizable];
      end; }

    with Columns.add do
    begin
      text := 'Module';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'Content';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'File';
      Options := Options + [coAutoSpring, coResizable];
    end;

    Options := Options + [hovisible, hoAutoSpring, hoAutoResize,
      hoDblClickResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;
  end;

  RootNodeCount := fLMSCourse.Sections.Count;

  Header.AutoFitColumns(false, smaAllColumns, 0);

end;

end.

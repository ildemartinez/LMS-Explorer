unit LMS.TreeView.Users;

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

  TLMSUsersTreeView = class(TLMSCustomLMSVirtualStringTree)
  private
    fLMSUsers: TList<IUser>;

    function GetSelectedUser: IUser;
    procedure setLMSUsers(const Value: TList<IUser>);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure MyDoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure MyDoInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  public
    constructor Create(Owner: TComponent); override;

    procedure FilterByText(const text: string);
    procedure Refreshh;

    property LMSUsers: TList<IUser> read fLMSUsers write setLMSUsers;
    property SelectedUser: IUser read GetSelectedUser;

  end;

implementation

uses
  vcl.Graphics,
  vcl.ImgList, windows,
  dialogs,
  ShellApi,

  VirtualTrees.Types,
  VirtualTrees.BaseTree,

  LMS.Helper.Consts,
  LMS.Helper.Browser;

constructor TLMSUsersTreeView.Create(Owner: TComponent);
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
  oninitnode := MyDoInitNode;
end;

procedure TLMSUsersTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  if parentdata = nil then
  begin
    data^.node_type := ntUser;
    data^.User := fLMSUsers[Node.Index];
    Exclude(Node.States, vsHasChildren);
  end
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

function TLMSUsersTreeView.GetSelectedUser: IUser;
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

procedure TLMSUsersTreeView.MyDoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PTreeData;
begin
  data := GetNodeData(Node);

  case Column of
    0:
      CellText := data^.User.Full_Name;
    1:
      CellText := data^.User.First_Name;

    2:
      CellText := data^.User.Last_Name;

    3:
      CellText := data^.User.Email;

  end;

end;

procedure TLMSUsersTreeView.MyDoInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin

end;

procedure TLMSUsersTreeView.Refreshh;
begin
  RootNodeCount := fLMSUsers.count;
  Header.AutoFitColumns(false, smaAllColumns, 0);
end;

procedure TLMSUsersTreeView.setLMSUsers(const Value: TList<IUser>);
begin
  fLMSUsers := Value;

  Header.Columns.Clear;

  with Header do
  begin
    with Columns.add do
    begin
      text := 'FullName';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'First name';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'Last name';
      Options := Options + [coAutoSpring, coResizable];
    end;

    with Columns.add do
    begin
      text := 'Email';
      Options := Options + [coAutoSpring, coResizable];
    end;

    Options := Options + [hovisible, hoAutoSpring, hoAutoResize,
      hoDblClickResize];
    AutoSizeIndex := Columns.GetLastVisibleColumn;
  end;

  RootNodeCount := fLMSUsers.count;

  Header.AutoFitColumns(false, smaAllColumns, 0);

end;

end.

unit LMS.Form.About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    lblLMSExplorer: TLabel;
    LinkLabel1: TLinkLabel;
    LinkLabel2: TLinkLabel;
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  LMS.Helper.Browser;

{$R *.dfm}

procedure TAboutForm.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  OpenInBrowser(Link);
end;

end.

unit LMSAboutFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TAboutForm = class(TForm)
    Label1: TLabel;
    Image1: TImage;
  private
    { Private declarations }
  public
constructor Create(Owner : TComponent); override;
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

{ TAboutForm }

constructor TAboutForm.Create(Owner: TComponent);
begin
  inherited;


end;

end.

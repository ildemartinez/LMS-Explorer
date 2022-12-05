unit LMSFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  LMSNetworkUnit, Vcl.StdCtrls;

type
  TLMSForm = class(TForm)
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    fLMS: TLMS;
    procedure SetTLMS(const Value: TLMS);
    { Private declarations }
  public
    { Public declarations }
    property LMS: TLMS write SetTLMS;
  end;

var
  LMSForm: TLMSForm;

implementation

{$R *.dfm}

procedure TLMSForm.Button1Click(Sender: TObject);
begin
  fLMS.aLMSConnection.GetCategories;
end;

procedure TLMSForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLMSForm.SetTLMS(const Value: TLMS);
begin
  fLMS := Value;
end;

end.

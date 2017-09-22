unit SvgViewerUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  SVG;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Button1: TButton;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    FSVG: TSVG;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Winapi.GDIPAPI, System.IOUtils, System.Types,
  SVGImage;

const
  CPath = '..\..\..\examples';

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute(Handle) then
  begin
    FSVG.LoadFromFile(OpenDialog1.FileName);
  end;
  PaintBox1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Files: TStringDynArray;
  F: TArray<string>;
  FileName: string;
begin
  FSVG := TSVG.Create;
  Files := TDirectory.GetFiles(CPath, '*.svg');
  for FileName in Files do
  begin
    F := F + [ExtractFileName(FileName)];
  end;

  ListBox1.Items.AddStrings(F);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSVG.Free;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  FSVG.LoadFromFile(TPath.Combine(CPath, ListBox1.Items[ListBox1.ItemIndex]));
  PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  if FSVG.Count > 0 then
  begin
    FSVG.PaintTo(PaintBox1.Canvas.Handle,
      MakeRect(0.0, 0.0, FSVG.Width, FSVG.Height), nil, 0);
  end;
end;

end.

unit FmxSVGViewerUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.ListBox,
  SVG;

type
  TForm2 = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  strict private
    { Private declarations }
    FSVG: TSVG;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses System.IOUtils, Winapi.GDIPOBJ, Winapi.GDIPUTIL, Winapi.GDIPAPI, SVGTypes;

{$R *.fmx}
const
  CPath = '..\..\..\examples\';

procedure TForm2.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FSVG.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Files: TStringDynArray;
  FileName: string;
  F: TArray<string>;
begin
  FSVG := TSVG.Create;

  Files := TDirectory.GetFiles(CPath, '*.svg');
  for FileName in Files do
  begin
    F := F + [ExtractFileName(FileName)];
  end;
  ListBox1.Items.AddStrings(F);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FSVG.Free;
end;

procedure TForm2.ListBox1Change(Sender: TObject);
begin
  FSVG.LoadFromFile(TPath.Combine(CPath, ListBox1.Selected.Text));
  PaintBox1.Repaint;
end;

procedure PaintToCanvas(const ASVG: TSVG; Canvas: TCanvas);
var
  GPGraphics: TGPGraphics;
  GPBitmap: TGPBitmap;
  GPRectF: TGPRectF;
  RectArray: TRectarray;
  GPRect: TGPRect;
  GPBitmapData: Winapi.GDIPAPI.TBitmapData;
  BitmapData: FMX.Graphics.TBitmapData;
  Bitmap: TBitmap;
  Source: PByte;
  Dest: PByte;
  Y: Integer;
begin
  GPBitmap := TGPBitmap.Create(Canvas.Width, Canvas.Height);
  GPGraphics := TGPGraphics.Create(GPBitmap);
  try
    GPGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    GPRectF.X := 0;
    GPRectF.Y := 0;
    GPRectF.Width := ASVG.Width;
    GPRectF.Height := ASVG.Height;

    RectArray := TRectArray.Create(TRect.Create(0, 0, Canvas.Width, Canvas.Height));
    ASVG.PaintTo(GPGraphics, GPRectF, @RectArray, 1);

    GPRect.X := 0;
    GPRect.Y := 0;
    GPRect.Width := GPBitmap.GetWidth;
    GPRect.Height := GPBitmap.GetHeight;

    GPBitmap.LockBits(GPRect, ImageLockModeRead, PixelFormat32bppPARGB, GPBitmapData);

    Bitmap := TBitmap.Create(GPRect.Width, GPRect.Height);
    try
      Bitmap.Map(TMapAccess.Write, BitmapData);

      Source := GPBitmapData.Scan0;
      Dest := BitmapData.Data;
      for Y := 0 to GPBitmapData.Height - 1 do
      begin
        Move(Source^, Dest^, GPBitmapData.Stride);
        Source := Source + GPBitmapData.Stride;
        Dest := Dest + BitmapData.Pitch;
      end;

      Bitmap.Unmap(BitmapData);
      Canvas.DrawBitmap(Bitmap, TRectF.Create(0, 0, Canvas.Width, Canvas.Height),
        TRectF.Create(0, 0, Canvas.Width, Canvas.Height), 100);
    finally
      Bitmap.Free;
    end;

    GPBitmap.UnlockBits(GPBitmapData);
  finally
    GPGraphics.Free;
    GPBitmap.Free;
  end;
end;

procedure TForm2.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  PaintToCanvas(FSVG, Canvas);
end;

end.

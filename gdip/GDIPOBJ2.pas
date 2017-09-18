      { *****************************************************************}
      { Added Support for RoundRect (GraphicsPath + TGPGraphics)         }
      {                                                                  }
      { date  : 05-11-2006                                               }
      {                                                                  }
      { email : martin.walter@winningcubed.de                            }
      {                                                                  }
      { *****************************************************************}
unit GDIPOBJ2;

interface

uses
  Vcl.Graphics,
  Winapi.GDIPAPI, Winapi.GDIPOBJ;

type
  TGPGraphicsPath2 = class(TGPGraphicsPath)
  public
    function AddRoundRect(Rect: TGPRectF; RX, RY: Single): TStatus; overload;
    function AddRoundRect(X, Y, Width, Height, RX, RY: Single): TStatus; overload;
    function Clone: TGPGraphicsPath2;
  end;

function TGPImageToBitmap(Image: TGPImage): TBitmap;

implementation

function TGPImageToBitmap(Image: TGPImage): TBitmap;
var
  Graphics: TGPGraphics;
  Bitmap: TBitmap;
  P: Pointer;
  W, H: Cardinal;
begin
  Bitmap := nil;
  if Assigned(Image) then
  begin
    W := Image.GetWidth;
    H := Image.GetHeight;
    if (W > 0) and (H > 0) then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Width := W;
      Bitmap.Height := H;
      P := Bitmap.ScanLine[H - 1];
      FillChar(P^, (W * H) shl 2, 0);
      Graphics := TGPGraphics.Create(Bitmap.Canvas.Handle);
      try
        Graphics.DrawImage(Image, 0, 0);
      finally
        Graphics.Free;
      end;
    end;
  end;
  Result := Bitmap;
end;

{ TGPGraphicsPath2 }

function TGPGraphicsPath2.AddRoundRect(Rect: TGPRectF; RX, RY: Single): TStatus;
begin
  Result := AddRoundRect(Rect.X, Rect.Y, Rect.Width, Rect.Height, RX, RY);
end;

function TGPGraphicsPath2.AddRoundRect(X, Y, Width, Height, RX, RY: Single) : TStatus;
begin
  Result := AddLine(X + RX, Y, X + Width - RX, Y);
  if Result <> OK then
    Exit;
  Result := AddArc(X + Width - 2 * RX, Y, 2 * RX, 2 * RY, 270, 90);
  if Result <> OK then
    Exit;

  Result := AddLine(X + Width, Y + RY,X + Width, Y + Height - RY);
  if Result <> OK then
    Exit;
  Result := AddArc(X + Width - 2 * RX, Y + Height - 2 * RY, 2 * RX, 2 * RY, 0, 90);
  if Result <> OK then
    Exit;

  Result := AddLine(X + Width - RX, Y + Height, X + RX, Y + Height);
  if Result <> OK then
    Exit;
  Result := AddArc(X, Y + Height - 2 * RY, 2 * RX, 2 * RY, 90, 90);
  if Result <> OK then
    Exit;

  Result := AddLine(X, Y + Height - RY, X, Y + RY);
  if Result <> OK then
    Exit;
  Result := AddArc(X, Y, 2 * RX, 2 * RY, 180, 90);
  if Result <> OK then
    Exit;
  Result := CloseFigure;
end;

function TGPGraphicsPath2.Clone: TGPGraphicsPath2;
var
  ClonePath: GpPath;
begin
  Clonepath := nil;
  SetStatus(GdipClonePath(nativePath, Clonepath));
  result := TGPGraphicsPath2.Create(ClonePath);
end;

end.

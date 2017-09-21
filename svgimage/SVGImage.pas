      {******************************************************************}
      { SVG Image in TPicture                                            }
      {                                                                  }
      { home page: http://www.mwcs.de                                    }
      { email    : martin.walter@mwcs.de                                 }
      {                                                                  }
      { date     : 05-04-2008                                            }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2005, 2008 Martin Walter                           }
      {                                                                  }
      { Thanks to:                                                       }
      { Elias Zurschmiede (imagelist error)                              }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVGImage;

interface

uses
  Winapi.Windows, Winapi.GDIPOBJ,
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics,
  SVG, SVGImageList;

type
  TSVGImage = class(TGraphicControl)
  strict private
    FSVGImage: TSVG;
    FStream: TMemoryStream;

    FCenter: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    FAutoSize: Boolean;
    FScale: Double;

    FOpacity: Byte;
    FFileName: TFileName;
    FImageList: TSVGImageList;
    FImageIndex: Integer;

    procedure SetCenter(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure SetImageIndex(const Value: Integer);
    procedure SetStretch(const Value: Boolean);
    procedure SetScale(const Value: Double);
    procedure SetAutoSizeImage(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckAutoSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Empty: Boolean;
    procedure Paint; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure Assign(Source: TPersistent); override;
    property SVG: TSVG read FSVGImage;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSizeImage;
    property Center: Boolean read FCenter write SetCenter;
    property Proportional: Boolean read FProportional write SetProportional;
    property Stretch: Boolean read FStretch write SetStretch;
    property Opacity: Byte read FOpacity write SetOpacity;
    property Scale: Double read FScale write SetScale;
    property FileName: TFileName read FFileName write SetFileName;
    property ImageList: TSVGImageList read FImageList write FImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Enabled;
    property Visible;
    property Constraints;
    property Anchors;
    property Align;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  TSVGGraphic = class(TGraphic)
  strict private
    FSVGImage: TSVG;
    FStream: TMemoryStream;

    FOpacity: Byte;
    FFileName: TFileName;

    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    function GetEmpty: Boolean; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure AssignSVG(SVG: TSVG);

    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;

    procedure SaveToStream(Stream: TStream); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;

    property Opacity: Byte read FOpacity write SetOpacity;
    property FileName: TFileName read FFileName write SetFileName;
  end;

function TGPImageToBitmap(Image: TGPImage): TBitmap;

implementation

uses
  Vcl.Dialogs,
  Winapi.GDIPAPI;

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

constructor TSVGImage.Create(AOwner: TComponent);
begin
  inherited;
  FSVGImage := TSVG.Create;
  FProportional := False;
  FCenter := True;
  FStretch := True;
  FOpacity := 255;
  FScale := 1;
  FImageIndex := -1;
  FStream := TMemoryStream.Create;
end;

destructor TSVGImage.Destroy;
begin
  FSVGImage.Free;
  FStream.Free;
  inherited;
end;

procedure TSVGImage.CheckAutoSize;
begin
  if FAutoSize and (FSVGImage.Width > 0) and (FSVGImage.Height > 0) then
  begin
    SetBounds(Left, Top,  Round(FSVGImage.Width), Round(FSVGImage.Height));
  end;
end;

procedure TSVGImage.Clear;
begin
  FSVGImage.Clear;
  FFileName := '';
  Repaint;
end;

function TSVGImage.Empty: Boolean;
begin
  Empty := FSVGImage.Count = 0;
end;

procedure TSVGImage.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TSVGImage.Paint;
var
  Bounds: TGPRectF;

  procedure CalcWidth(const ImageWidth, ImageHeight: Double);
  var
    R: Double;
  begin
    Bounds.Width := ImageWidth * FScale;
    Bounds.Height := ImageHeight * FScale;

    if FProportional then
    begin
      if ImageHeight > 0 then
        R :=  ImageWidth / ImageHeight
      else
        R := 1;

      if Width / Height > R then
      begin
        Bounds.Width := Height * R;
        Bounds.Height := Height;
      end else
      begin
        Bounds.Width := Width;
        Bounds.Height := Width / R;
      end;
      Exit;
    end;

    if FStretch then
    begin
      Bounds := MakeRect(0.0, 0, Width, Height);
      Exit;
    end;
  end;

  procedure CalcOffset;
  begin
    Bounds.X := 0;
    Bounds.Y := 0;
    if FCenter then
    begin
      Bounds.X := (Width - Bounds.Width) / 2;
      Bounds.Y := (Height - Bounds.Height) / 2;
    end;
  end;

var
  SVG: TSVG;
begin
  if Assigned(FImageList) and (FImageIndex >= 0) and
     (FImageIndex < FImagelist.Count) then
    SVG := FImageList.Images[FImageIndex]
  else
    SVG := FSVGImage;

  if SVG.Count > 0 then
  begin
    CalcWidth(SVG.Width, SVG.Height);
    CalcOffset;

    SVG.SVGOpacity := FOpacity / 255;
    SVG.PaintTo(Canvas.Handle, Bounds, nil, 0);
    SVG.SVGOpacity := 1;
  end;

  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TSVGImage.LoadFromFile(const FileName: string);
begin
  if csLoading in ComponentState then
    Exit;
  try
    FStream.Clear;
    FStream.LoadFromFile(FileName);
    FSVGImage.LoadFromStream(FStream);
    FFileName := FileName;
  except
    Clear;
  end;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGImage.LoadFromStream(Stream: TStream);
begin
  try
    FFileName := '';
    FStream.Clear;
    FStream.LoadFromStream(Stream);
    FSVGImage.LoadFromStream(FStream);
  except
  end;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TSVGImage.Assign(Source: TPersistent);
var
  SVG: TSVG;
begin
  if (Source is TSVGImage) then
  begin
    SVG := (Source as TSVGImage).FSVGImage;
    FSVGImage.LoadFromText(SVG.Source);
    FImageIndex := -1;
    CheckAutoSize;
  end;

  if (Source.ClassType = TSVG) then
  begin
    SVG := TSVG(Source);
    FSVGImage.LoadFromText(SVG.Source);
    FImageIndex := -1;
  end;

  Repaint;
end;

procedure TSVGImage.SetAutoSizeImage(const Value: Boolean);
begin
  if (Value = FAutoSize) then
    Exit;
  FAutoSize := Value;

  CheckAutoSize;
end;

procedure TSVGImage.SetCenter(Value: Boolean);
begin
  if Value = FCenter then
    Exit;

  FCenter := Value;
  Repaint;
end;

procedure TSVGImage.SetProportional(Value: Boolean);
begin
  if Value = FProportional then
    Exit;

  FProportional := Value;
  Repaint;
end;

procedure TSVGImage.SetScale(const Value: Double);
begin
  if Value = FScale then
    Exit;
  FScale := Value;
  FAutoSize := False;
  Repaint;
end;

procedure TSVGImage.SetStretch(const Value: Boolean);
begin
  if Value = FStretch then
    Exit;

  FStretch := Value;
  if FStretch then
    FAutoSize := False;
  Repaint;
end;

procedure TSVGImage.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Repaint;
end;

procedure TSVGImage.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;

  LoadFromFile(Value);
end;

procedure TSVGImage.ReadData(Stream: TStream);
var
  Size: LongInt;
begin
  Stream.Read(Size, SizeOf(Size));
  FStream.Clear;
  if Size > 0 then
  begin
    FStream.CopyFrom(Stream, Size);
    FSVGImage.LoadFromStream(FStream);
  end else
    FSVGImage.Clear;
end;

procedure TSVGImage.WriteData(Stream: TStream);
var
  Size: LongInt;
begin
  Size := FStream.Size;
  Stream.Write(Size, SizeOf(Size));
  FStream.Position := 0;
  if FStream.Size > 0 then
    FStream.SaveToStream(Stream);
end;


constructor TSVGGraphic.Create;
begin
  inherited;
  FSVGImage := TSVG.Create;
  FOpacity := 255;
  FStream := TMemoryStream.Create;
end;

destructor TSVGGraphic.Destroy;
begin
  FSVGImage.Free;
  FStream.Free;
  inherited;
end;

procedure TSVGGraphic.Clear;
begin
  FSVGImage.Clear;
  FFileName := '';
  Changed(Self);
end;

procedure TSVGGraphic.Assign(Source: TPersistent);
begin
  if (Source is TSVGGraphic) then
  begin
    try
      FSVGImage.Free;
      FSVGImage := TSVG(TSVGGraphic(Source).FSVGImage.Clone(nil));
      FStream.Clear;
      FStream.LoadFromStream(TSVGGraphic(Source).FStream);
    except
    end;
    Changed(Self);
  end;
end;

procedure TSVGGraphic.AssignSVG(SVG: TSVG);
begin
  FSVGImage.LoadFromText(SVG.Source);
  Changed(Self);
end;

procedure TSVGGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGGraphic then
    TSVGGraphic(Dest).Assign(Self);
end;

procedure TSVGGraphic.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Changed(Self);
end;

procedure TSVGGraphic.SetWidth(Value: Integer);
begin
  inherited;

end;

procedure TSVGGraphic.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;

  LoadFromFile(Value);
end;

procedure TSVGGraphic.SetHeight(Value: Integer);
begin
  inherited;

end;

procedure TSVGGraphic.ReadData(Stream: TStream);
var
  Size: LongInt;
begin
  Stream.Read(Size, SizeOf(Size));
  FStream.Clear;
  FStream.CopyFrom(Stream, Size);
  FSVGImage.LoadFromStream(FStream);
end;

procedure TSVGGraphic.WriteData(Stream: TStream);
var
  Size: LongInt;
begin
  Size := FStream.Size;
  Stream.Write(Size, SizeOf(Size));
  FStream.Position := 0;
  FStream.SaveToStream(Stream);
end;

procedure TSVGGraphic.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TSVGGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Bounds: TGPRectF;
begin
  if Empty then
    Exit;

  Bounds := MakeRect(Rect.Left + 0.0, Rect.Top,
    Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);

  FSVGImage.SVGOpacity := FOpacity / 255;
  FSVGImage.PaintTo(ACanvas.Handle, Bounds, nil, 0);
end;


function TSVGGraphic.GetEmpty: Boolean;
begin
  Result := FSVGImage.Count = 0;
end;

function TSVGGraphic.GetWidth: Integer;
begin
  Result := Round(FSVGImage.Width);
end;

function TSVGGraphic.GetHeight: Integer;
begin
  Result := Round(FSVGImage.Height);
end;

procedure TSVGGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.LoadFromFile(const Filename: String);
begin
  FStream.Clear;
  FStream.LoadFromFile(FileName);
  FSVGImage.LoadFromStream(FStream);
  Changed(Self);
end;

procedure TSVGGraphic.LoadFromStream(Stream: TStream);
begin
  try
    FFileName := '';
    FStream.LoadFromStream(Stream);
    FSVGImage.LoadFromStream(FStream);
  except
  end;
  Changed(Self);
end;

procedure TSVGGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.SaveToStream(Stream: TStream);
begin
  FStream.Position := 0;
  FStream.SaveToStream(Stream);
end;


procedure TSVGImage.SetImageIndex(const Value: Integer);
begin
  if FImageIndex = Value then
    Exit;
  FImageIndex := Value;
  CheckAutoSize;
  Repaint;
end;

initialization
  TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TSVGGraphic);

finalization
  TPicture.UnregisterGraphicClass(TSVGGraphic);
end.

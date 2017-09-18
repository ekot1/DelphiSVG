unit SVGImageList;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Graphics, Vcl.ImgList,
  SVG;

type
  TSVGCollectionItem = class(TCollectionItem)
  strict private
    FName: string;
    FSVG: TSVG;
    procedure SetName(const Value: string);
    procedure SetSVG(const Value: TSVG);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property SVG: TSVG read FSVG write SetSVG;
    property Name: string read FName write SetName;
  end;

  TSVGCollectionItems = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSVGCollectionItem;
    procedure SetItem(Index: Integer; const Value: TSVGCollectionItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSVGCollectionItem;
    procedure Assign(Source: TPersistent); override;
    property Items[index: Integer]: TSVGCollectionItem read GetItem write SetItem; default;
  end;

  TSVGImageList = class(TCustomImageList)
  strict private
    FImages: TSVGCollectionItems;
    FOpacity: Byte;
    FUpdating: Boolean;
    function GetImages(Index: Integer): TSVG;
    function GetNames(Index: Integer): string;
    procedure SetImages(Index: Integer; const Value: TSVG);
    procedure SetNames(Index: Integer; const Value: string);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function SVGToIcon(const SVG: TSVG): HICON;
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);
    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);
  private
    procedure RecreateBitmaps;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const SVG: TSVG; const Name: string): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Name: string);
    function IndexOf(const Name: string): Integer;
    procedure Clear;
    procedure PaintTo(const DC: HDC; const Index: Integer;
      const X, Y, Width, Height: Double); overload;
    procedure PaintTo(const DC: HDC; const Name: string;
      const X, Y, Width, Height: Double); overload;
    property Images[Index: Integer]: TSVG read GetImages write SetImages;
    property Names[Index: Integer]: string read GetNames write SetNames;
    property Count: Integer read GetCount;
  published
    property Items: TSVGCollectionItems read FImages;
    property Opacity: Byte read FOpacity write SetOpacity;
    property Width: Integer read GetWidth write SetWidth default 16;
    property Height: Integer read GetHeight write SetHeight default 16;
  end;

implementation

uses
  Winapi.CommCtrl, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  Vcl.ComCtrls,
  GDIPUtils, SVGTypes;

{ TSVGImageList }

function TSVGImageList.Add(const SVG: TSVG;
  const Name: string): Integer;
var
  Item: TSVGCollectionItem;
  Updating: Boolean;
begin
  Updating := FUpdating;
  try
    FUpdating := True;
    Item := FImages.Add;

    Item.SVG := SVG;
    Item.Name := Name;
  finally
    FUpdating := Updating;
    RecreateBitmaps;
  end;
  Result := FImages.Count - 1;
end;

procedure TSVGImageList.AssignTo(Dest: TPersistent);
begin
  Clear;
  inherited;
  if Dest is TSVGImageList then
  begin
    TSVGImageList(Dest).FOpacity := FOpacity;
    TSVGImageList(Dest).Width := Width;
    TSVGImageList(Dest).Height := Height;
    FImages.AssignTo(TSVGImageList(Dest).FImages);
  end;
end;

procedure TSVGImageList.Clear;
begin
  inherited Clear;
  FUpdating := True;
  try
    FImages.Clear;
  finally
    FUpdating := False;
    RecreateBitmaps;
  end;
end;

constructor TSVGImageList.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TSVGCollectionItems.Create(Self);
  FOpacity := 255;
end;

procedure TSVGImageList.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
  Info: Longint;
begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then
    Info := Ancestor.DesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft, LongRec(DesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop, LongRec(DesignInfo).Hi <> LongRec(Info).Hi);
  Filer.DefineBinaryProperty('Images', ReadImageData, WriteImageData, True);
end;

procedure TSVGImageList.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FImages.Count) then
    FImages.Delete(Index);
end;

destructor TSVGImageList.Destroy;
begin
  Clear;
  FImages.Free;
  inherited;
end;

procedure TSVGImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
  Style: Cardinal; Enabled: Boolean);
begin
  PaintTo(Canvas.Handle, Index, X, Y, Width, Height);
end;

function TSVGImageList.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TSVGImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGImageList.GetImages(Index: Integer): TSVG;
begin
  if (Index >= 0) and (Index < FImages.Count) then
    Result := FImages[Index].SVG
  else
    Result := nil;
end;

function TSVGImageList.GetNames(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FImages.Count) then
    Result := FImages[Index].Name
  else
    Result := '';
end;

function TSVGImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TSVGImageList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to FImages.Count - 1 do
    if FImages[Result].Name = Name then
      Exit;
  Result := -1;
end;

procedure TSVGImageList.PaintTo(const DC: HDC; const Index: Integer;
  const X, Y, Width, Height: Double);
var
  R: TGPRectF;
  SVG: TSVG;
begin
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    SVG := FImages[Index].SVG;
    SVG.SVGOpacity := FOpacity / 255;
    R := CalcRect(MakeRect(X, Y, Width, Height), SVG.Width, SVG.Height, baCenterCenter);
    SVG.PaintTo(DC, R, nil, 0);
    SVG.SVGOpacity := 1;
  end;
end;

procedure TSVGImageList.PaintTo(const DC: HDC; const Name: string;
  const X, Y, Width, Height: Double);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  PaintTo(DC, Index, X, Y, Width, Height);
end;


procedure TSVGImageList.ReadImageData(Stream: TStream);
var
  FStream: TMemoryStream;
  Count, Size: Integer;
  SVG: TSVG;
  Name: string;
  C: Integer;
begin
  try
    FUpdating := True;
    FStream := TMemoryStream.Create;
    Stream.Read(Count, SizeOf(Integer));
    SVG := TSVG.Create(nil);
    for C := 0 to Count - 1 do
    begin
      Stream.Read(Size, SizeOf(Integer));
      SetLength(Name, Size);
      Stream.Read(PWideChar(Name)^, Size * SizeOf(WideChar));

      Stream.Read(Size, SizeOf(Integer));
      FStream.CopyFrom(Stream, Size);
      SVG.LoadFromStream(FStream);
      FStream.Clear;
      Add(SVG, Name);
    end;
    FStream.Free;
    SVG.Free;
  finally
    FUpdating := False;
    RecreateBitmaps;
  end;
end;

procedure TSVGImageList.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGImageList.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGImageList.RecreateBitmaps;
var
  C: Integer;
  SVG: TSVG;
  Icon: HIcon;
begin
  if not FUpdating then
  begin
    ImageList_Remove(Handle, -1);
    Handle := ImageList_Create(Width, Height,
      ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);

    for C := 0 to FImages.Count - 1 do
    begin
      SVG := FImages[C].SVG;
      if Assigned(SVG) then
      begin
        Icon := SVGToIcon(SVG);
        ImageList_AddIcon(Handle, Icon);
        DestroyIcon(Icon);
      end;
    end;
  end;
end;

procedure TSVGImageList.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TSVGImageList.SetHeight(const Value: Integer);
begin
  inherited Height := Value;
  RecreateBitmaps;
end;

procedure TSVGImageList.SetImages(Index: Integer; const Value: TSVG);
begin
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    if FImages[Index].SVG <> Value then
      FImages[Index].SVG := Value;
  end;
end;

procedure TSVGImageList.SetNames(Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index < FImages.Count) then
    FImages[Index].Name := Value;
end;

procedure TSVGImageList.SetOpacity(const Value: Byte);
begin
  FOpacity := Value;
  RecreateBitmaps;
end;

procedure TSVGImageList.SetWidth(const Value: Integer);
begin
  inherited Width := Value;
  RecreateBitmaps;
end;

procedure PaintToBitmap(SVG: TSVG; Bitmap: TBitmap; Bounds: TGPRectF;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TGPGraphics;
begin
  Graphics := TGPGraphics.Create(Bitmap.Canvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    SVG.PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;
end;

function TSVGImageList.SVGToIcon(const SVG: TSVG): HICON;
var
  R: TGPRectF;

  function SVGToIcon24(SVG: TSVG): HIcon;
  var
    ColorBitmap, MaskBitmap: TBitmap;
    X: Integer;
    Y: Integer;
    Bits: PRGBQuad;
    IconInfo: TIconInfo;
    TransparentBitmap: TBitmap;
    BF: TBlendFunction;
    DC: THandle;
  begin
    ColorBitmap := TBitmap.Create;
    MaskBitmap := TBitmap.Create;
    TransparentBitmap := TBitmap.Create;
    try
      TransparentBitmap.PixelFormat := pf32bit;
      TransparentBitmap.Width := Width;
      TransparentBitmap.Height := Height;
      FillChar(TransparentBitmap.Scanline[Height - 1]^, Width * Height * 4, 0);

      PaintToBitmap(SVG, TransparentBitmap, R, nil, 0);


      ColorBitmap.PixelFormat := pf32bit;
      ColorBitmap.Width := Width;
      ColorBitmap.Height := Height;
      MaskBitmap.PixelFormat := pf32bit;
      MaskBitmap.Width := Width;
      MaskBitmap.Height := Height;


      ColorBitmap.Canvas.Brush.Color := BkColor;
      ColorBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));

      BF.BlendOp := AC_SRC_OVER;
      BF.BlendFlags := 0;
      BF.SourceConstantAlpha := 255;
      BF.AlphaFormat := AC_SRC_ALPHA;
      AlphaBlend(ColorBitmap.Canvas.Handle, 0, 0, Width, Height,
        TransparentBitmap.Canvas.Handle, 0, 0, Width, Height, BF);

      DC := MaskBitmap.Canvas.Handle;
      for Y := 0 to Height - 1 do
      begin
        Bits := TransparentBitmap.ScanLine[Y];
        for X := 0 to Width - 1 do
        begin
          if Bits.rgbReserved = 0 then
            SetPixelV(DC, X, Y, clWhite)
          else
            SetPixelV(DC, X, Y, clBlack);
          Inc(Bits);
        end;
      end;

      IconInfo.fIcon := True;
      IconInfo.hbmColor := ColorBitmap.Handle;
      IconInfo.hbmMask := MaskBitmap.Handle;
      Result := CreateIconIndirect(IconInfo);
    finally
      TransparentBitmap.Free;
      ColorBitmap.Free;
      MaskBitmap.Free;
    end;
  end;

  function SVGToIcon32(SVG: TSVG): HICON;
  var
    Bitmap: TGPBitmap;
    Graphics: TGPGraphics;
  begin
    Bitmap := TGPBitmap.Create(Width, Height);
    Graphics := TGPGraphics.Create(Bitmap);
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    SVG.PaintTo(Graphics, R, nil, 0);
    Graphics.Free;

    Bitmap.GetHICON(Result);
    Bitmap.Free;
  end;

begin
  SVG.SVGOpacity := FOpacity / 255;
  R := CalcRect(MakeRect(0.0, 0, Width, Height), SVG.Width, SVG.Height, baCenterCenter);

  if GetFileVersion(comctl32) >= ComCtlVersionIE6 then
    Result := SVGToIcon32(SVG)
  else
    Result := SVGToIcon24(SVG);
    
  SVG.SVGOpacity := 1;
end;

procedure TSVGImageList.WriteImageData(Stream: TStream);
var
  Count, Size: Integer;
  SVG: TSVG;
  Name: string;
  C: Integer;
  SVGStream: TMemoryStream;
begin
  Count := FImages.Count;
  Stream.Write(Count, SizeOf(Integer));

  SVGStream := TMemoryStream.Create;
  for C := 0 to Count - 1 do
  begin
    Name := FImages[C].Name;
    SVG := FImages[C].SVG;
    Size := Length(Name);
    Stream.Write(Size, SizeOf(Integer));
    Stream.WriteBuffer(PWideChar(Name)^, Size * SizeOf(WideChar));

    SVG.SaveToStream(SVGStream);
    Size := SVGStream.Size;
    Stream.Write(Size, SizeOf(Integer));
    SVGStream.Position := 0;
    Stream.CopyFrom(SVGStream, Size);
    SVGStream.Clear;
  end;
  SVGStream.Free;
end;

procedure TSVGImageList.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGImageList.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{ TSVGImageCollectionItem }

procedure TSVGCollectionItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGCollectionItem then
  begin
    TSVGCollectionItem(Dest).FName := FName;
    TSVGCollectionItem(Dest).FSVG.LoadFromText(FSVG.Source);
  end;
end;

constructor TSVGCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSVG := TSVG.Create;
end;

destructor TSVGCollectionItem.Destroy;
begin
  FreeAndNil(FSVG);
  inherited;
end;

procedure TSVGCollectionItem.SetName(const Value: string);
begin
  FName := Value;
  TSVGCollectionItems(Collection).Update(Self);
end;

procedure TSVGCollectionItem.SetSVG(const Value: TSVG);
begin
  FSVG.LoadFromText(Value.Source);
  TSVGCollectionItems(Collection).Update(Self);
end;

{ TSVGImageListCollection }

function TSVGCollectionItems.Add: TSVGCollectionItem;
begin
  Result := TSVGCollectionItem(inherited Add);
end;

procedure TSVGCollectionItems.Assign(Source: TPersistent);
var
  C: Integer;
  Item: TSVGCollectionItem;
begin
  inherited;
  if Source is TSVGCollectionItems then
  try
    BeginUpdate;
    Clear;
    for C := 0 to TSVGCollectionItems(Source).Count - 1 do
    begin
      Item := Add;
      TSVGCollectionItems(Source)[C].AssignTo(Item);
    end;
  finally
    EndUpdate;
    Update(nil);
  end;
end;

constructor TSVGCollectionItems.Create(AOwner: TPersistent);
begin
  inherited Create(TSVGCollectionItem);
  FOwner := AOwner;
end;

function TSVGCollectionItems.GetItem(
  Index: Integer): TSVGCollectionItem;
begin
  Result := TSVGCollectionItem(inherited GetItem(Index));
end;

function TSVGCollectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSVGCollectionItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  if FOwner is TSVGImageList then
    TSVGImageList(FOwner).RecreateBitmaps;
end;

procedure TSVGCollectionItems.SetItem(Index: Integer;
  const Value: TSVGCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSVGCollectionItems.Update(Item: TCollectionItem);
begin
  inherited;
  if FOwner is TSVGImageList then
    TSVGImageList(FOwner).RecreateBitmaps;
end;

end.

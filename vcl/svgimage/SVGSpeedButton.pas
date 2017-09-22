unit SVGSpeedButton;

interface

uses
  System.Classes, VCL.Buttons, SVG;

type
  TSVGSpeedButton = class(TSpeedButton)
  private
    FSVG: TSVG;
    function GetSVG: TSVG;
    procedure SetSVG(const Value: TSVG);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SVG: TSVG read GetSVG write SetSVG;
  end;

implementation

uses
  Winapi.Windows,
  Vcl.Themes, Vcl.Controls, Vcl.ActnList, Vcl.Graphics,
  SVGImageList,
  Winapi.GDIPAPI, GDIPUtils,
  System.Types, System.Math;

{ TSVGSpeedButton }

constructor TSVGSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FSVG := TSVG.Create;
end;

destructor TSVGSpeedButton.Destroy;
begin
  FSVG.Free;
  inherited;
end;

function TSVGSpeedButton.GetSVG: TSVG;
begin
  Result := FSVG;
end;

procedure TSVGSpeedButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
  Image: TSVG;
  FMargin: Integer;
  R: TGPRectF;
begin
  if Assigned(Action) and (Action is TCustomAction) and
     (TCustomAction(Action).ImageIndex <> -1) and
     (TCustomAction(Action).ActionList.Images is TSVGImageList) then
    Image := TSVGImageList(TCustomAction(Action).ActionList.Images).Images[TCustomAction(Action).ImageIndex]
  else
    Image := FSVG;

  if not Enabled then
  begin
    FState := bsDisabled;
  end
  else if FState = bsDisabled then
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;

  if StyleServices.Enabled then
  begin
    //PerformEraseBackground(Self, Canvas.Handle);

    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if FState in [bsDown, bsExclusive] then
        Button := tbPushButtonPressed
      else
        if MouseInControl then
          Button := tbPushButtonHot
        else
          Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if Flat then
    begin
      case Button of
        tbPushButtonDisabled:
          Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:
          Toolbutton := ttbButtonPressed;
        tbPushButtonHot:
          Toolbutton := ttbButtonHot;
        tbPushButtonNormal:
          Toolbutton := ttbButtonNormal;
      end;
    end;

    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then
    begin
      Details := StyleServices.GetElementDetails(Button);
      StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
      StyleServices.GetElementContentRect(Canvas.Handle, Details, ClientRect, PaintRect);
    end
    else
    begin
      Details := StyleServices.GetElementDetails(ToolButton);
      StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
      StyleServices.GetElementContentRect(Canvas.Handle, Details, ClientRect, PaintRect);
    end;

    // A pressed speed button has a white text. This applies however only to flat buttons.
    Offset := Point(IfThen(Button = tbPushButtonPressed, 1, 0), 0);
  end
  else
  begin
    PaintRect := Rect(0, 0, Width, Height);
    if not Flat then
    begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
        DrawFlags := DrawFlags or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end
    else
    begin
      if (FState in [bsDown, bsExclusive]) or
        (MouseInControl and (FState <> bsDisabled)) or
        (csDesigning in ComponentState) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[Transparent] or BF_RECT)
      else if not Transparent then
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(PaintRect);
      end;
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) and (not Flat or not MouseInControl) then
      begin
        Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        Canvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;
  end;

  if Assigned(Image) then
  begin
    FMargin := 2 + Margin;
    R.X := PaintRect.Left + Margin + Offset.X + 1;
    R.Y := PaintRect.Top + Margin + Offset.Y + 1;
    R.Width := (PaintRect.Right - PaintRect.Left + 1) - 2 * FMargin;
    R.Height := (PaintRect.Bottom - PaintRect.Top + 1) - 2 * FMargin;
    R := CalcRect(R, Image.Width, Image.Height, baCenterCenter);
    Image.PaintTo(Canvas.Handle, R, nil, 0);
  end;
end;

procedure TSVGSpeedButton.SetSVG(const Value: TSVG);
begin
  FSVG.LoadFromText(Value.Source);
  Invalidate;
end;

end.

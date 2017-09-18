      {******************************************************************}
      { SVG path classes                                                 }
      {                                                                  }
      { home page : http://www.mwcs.de                                   }
      { email     : martin.walter@mwcs.de                                }
      {                                                                  }
      { date      : 05-04-2008                                           }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2005, 2008 Martin Walter                           }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVGPath;

interface

uses
  Winapi.Windows, Winapi.GDIPOBJ,
  System.Classes,
  SVGTypes, SVG;

type
  TSVGPathElement = class(TSVGObject)
  private
    FStartX: TFloat;
    FStartY: TFloat;
    FStopX: TFloat;
    FStopY: TFloat;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; virtual; abstract;
    procedure AddToPath(Path: TGPGraphicsPath); virtual; abstract;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); virtual;

    procedure PaintToGraphics(Graphics: TGPGraphics); override;
    procedure PaintToPath(Path: TGPGraphicsPath); override;

    property StartX: TFloat read FStartX write FStartX;
    property StartY: TFloat read FStartY write FStartY;
    property StopX: TFloat read FStopX write FStopX;
    property StopY: TFloat read FStopY write FStopY;
  end;

  TSVGPathMove = class(TSVGPathElement)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TGPGraphicsPath); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;
  end;

  TSVGPathLine = class(TSVGPathElement)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TGPGraphicsPath); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;
  end;

  TSVGPathCurve = class(TSVGPathElement)
  private
    FControl1X: TFloat;
    FControl1Y: TFloat;
    FControl2X: TFloat;
    FControl2Y: TFloat;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TGPGraphicsPath); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;

    property Control1X: TFloat read FControl1X write FControl1X;
    property Control1Y: TFloat read FControl1Y write FControl1Y;
    property Control2X: TFloat read FControl2X write FControl2X;
    property Control2Y: TFloat read FControl2Y write FControl2Y;
  end;

  TSVGPathEllipticArc = class(TSVGPathElement)
  private
    FRX: TFloat;
    FRY: TFloat;
    FXRot: TFloat;
    FLarge: Integer;
    FSweep: Integer;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TGPGraphicsPath); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;

    property RX: TFloat read FRX write FRX;
    property RY: TFloat read FRY write FRY;
    property XRot: TFloat read FXRot write FXRot;
    property Large: Integer read FLarge write FLarge;
    property Sweep: Integer read FSweep write FSweep;
  end;

  TSVGPathClose = class(TSVGPathElement)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TGPGraphicsPath); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;
  end;

implementation

uses
  System.SysUtils, System.Math,
  Winapi.GDIPAPI,
  SVGCommon, SVGParse;

// TSVGPathElement

procedure TSVGPathElement.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathElement then
  begin
    FStartX := TSVGPathElement(SVG).FStartX;
    FStartY := TSVGPathElement(SVG).FStartY;
    FStopX :=  TSVGPathElement(SVG).FStopX;
    FStopY :=  TSVGPathElement(SVG).FStopY;
  end;
end;

function TSVGPathElement.New(Parent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;

procedure TSVGPathElement.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
begin
  if Assigned(Previous) then
  begin
    FStartX := Previous.FStopX;
    FStartY := Previous.FStopY;
  end;
end;

procedure TSVGPathElement.PaintToGraphics(Graphics: TGPGraphics);
begin
end;

procedure TSVGPathElement.PaintToPath(Path: TGPGraphicsPath);
begin
end;

// TSVGPathMove

function TSVGPathMove.GetBounds: TFRect;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

function TSVGPathMove.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathMove.Create(Parent);
end;

procedure TSVGPathMove.AddToPath(Path: TGPGraphicsPath);
begin
  Path.StartFigure;
end;

procedure TSVGPathMove.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
begin
  inherited;
  if not TryStrToTFloat(SL[Position + 1], FStopX) then
    FStopX := 0;
  if not TryStrToTFloat(SL[Position + 2], FStopY) then
    FStopY := 0;

  if SL[Position] = 'm' then
  begin
    FStopX := FStartX + FStopX;
    FStopY := FStartY + FStopY;
  end;

  Inc(Position, 2);
end;


// TSVGPathLine

function TSVGPathLine.GetBounds: TFRect;
begin
  Result.Left := Min(FStartX, FStopX);
  Result.Top := Min(FStartY, FStopY);
  Result.Width := Abs(FStartX - FStopX);
  Result.Height := Abs(FStartY - FStopY);
end;

function TSVGPathLine.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathLine.Create(Parent);
end;

procedure TSVGPathLine.AddToPath(Path: TGPGraphicsPath);
begin
  Path.AddLine(FStartX, FStartY, FStopX, FStopY);
end;

procedure TSVGPathLine.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
var
  Command: string;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'L') or (Command = 'l') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopX) then
      FStopX := 0;
    if not TryStrToTFloat(SL[Position + 2], FStopY) then
      FStopY := 0;

    if SL[Position] = 'l' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;

    Inc(Position, 2);
  end;

  if (Command = 'H') or (Command = 'h') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopX) then
      FStopX := 0;

    if Command = 'h' then
      FStopX := FStartX + FStopX;
    FStopY := FStartY;
    Inc(Position);
  end;


  if (Command = 'V') or (Command = 'v') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopY) then
      FStopY := 0;

    if Command = 'v' then
      FStopY := FStartY + FStopY;
    FStopX := FStartX;
    Inc(Position);
  end;
end;


// TSVGPathCurve

procedure TSVGPathCurve.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathCurve then
  begin
    FControl1X := TSVGPathCurve(SVG).FControl1X;
    FControl1Y := TSVGPathCurve(SVG).FControl1Y;
    FControl2X := TSVGPathCurve(SVG).FControl2X;
    FControl2Y := TSVGPathCurve(SVG).FControl2Y;
  end;
end;

function TSVGPathCurve.GetBounds: TFRect;
var
  Right, Bottom: TFloat;
begin
  Result.Left := Min(FStartX, Min(FStopX, Min(FControl1X, FControl2X)));
  Result.Top := Min(FStartY, Min(FStopY, Min(FControl1Y, FControl2Y)));

  Right := Max(FStartX, Max(FStopX, Max(FControl1X, FControl2X)));
  Bottom := Max(FStartY, Max(FStopY, Max(FControl1Y, FControl2Y)));
  Result.Width := Right - Result.Left;
  Result.Height := Bottom - Result.Top;
end;

function TSVGPathCurve.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathCurve.Create(Parent);
end;

procedure TSVGPathCurve.AddToPath(Path: TGPGraphicsPath);
begin
  Path.AddBezier(FStartX, FStartY, FControl1X, FControl1Y,
    FControl2X, FControl2Y, FStopX, FStopY);
end;

procedure TSVGPathCurve.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
var
  Command: string;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'C') or (Command = 'c') then
  begin
    TryStrToTFloat(SL[Position + 1], FControl1X);
    TryStrToTFloat(SL[Position + 2], FControl1Y);
    TryStrToTFloat(SL[Position + 3], FControl2X);
    TryStrToTFloat(SL[Position + 4], FControl2Y);
    TryStrToTFloat(SL[Position + 5], FStopX);
    TryStrToTFloat(SL[Position + 6], FStopY);
    Inc(Position, 6);

    if Command = 'c' then
    begin
      FControl1X := FStartX + FControl1X;
      FControl1Y := FStartY + FControl1Y;
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'S') or (Command = 's') then
  begin
    FControl1X := FStartX;
    FControl1Y := FStartY;
    TryStrToTFloat(SL[Position + 1], FControl2X);
    TryStrToTFloat(SL[Position + 2], FControl2Y);
    TryStrToTFloat(SL[Position + 3], FStopX);
    TryStrToTFloat(SL[Position + 4], FStopY);
    Inc(Position, 4);

    if Previous is TSVGPathCurve then
    begin
      FControl1X := FStartX + (FStartX - TSVGPathCurve(Previous).FControl2X);
      FControl1Y := FStartY + (FStartY - TSVGPathCurve(Previous).FControl2Y);
    end;

    if Command = 's' then
    begin
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'Q') or (Command = 'q') then
  begin
    TryStrToTFloat(SL[Position + 1], FControl1X);
    TryStrToTFloat(SL[Position + 2], FControl1Y);
    TryStrToTFloat(SL[Position + 3], FStopX);
    TryStrToTFloat(SL[Position + 4], FStopY);
    FControl2X := FControl1X;
    FControl2Y := FControl1Y;
    Inc(Position, 4);

    if Command = 'q' then
    begin
      FControl1X := FStartX + FControl1X;
      FControl1Y := FStartY + FControl1Y;
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'T') or (Command = 't') then
  begin
    FControl1X := FStartX;
    FControl1Y := FStartY;
    TryStrToTFloat(SL[Position + 1], FStopX);
    TryStrToTFloat(SL[Position + 2], FStopY);
    Inc(Position, 2);

    if Previous is TSVGPathCurve then
    begin
      FControl1X := FStartX + (FStartX - TSVGPathCurve(Previous).FControl2X);
      FControl1Y := FStartY + (FStartY - TSVGPathCurve(Previous).FControl2Y);
    end;

    FControl2X := FControl1X;
    FControl2Y := FControl1Y;

    if Command = 't' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;
end;


// TSVGPathEllipticArc

procedure TSVGPathEllipticArc.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathEllipticArc then
  begin
    FRX := TSVGPathEllipticArc(SVG).FRX;
    FRY := TSVGPathEllipticArc(SVG).FRY;
    FXRot := TSVGPathEllipticArc(SVG).FXRot;
    FLarge := TSVGPathEllipticArc(SVG).FLarge;
    FSweep := TSVGPathEllipticArc(SVG).FSweep;
  end;
end;

function TSVGPathEllipticArc.GetBounds: TFRect;
begin
  Result.Left := Min(FStartX, FStopX);
  Result.Top := Min(FStartY, FStopY);
  Result.Width := Abs(FStartX - FStopX);
  Result.Height := Abs(FStartY - FStopY);
end;

function TSVGPathEllipticArc.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathEllipticArc.Create(Parent);
end;

procedure TSVGPathEllipticArc.AddToPath(Path: TGPGraphicsPath);
var
  R: TGPRectF;
  X1, Y1: TFloat;
  dx2, dy2: TFloat;
  angle: TFloat;
  SinAngle: TFloat;
  CosAngle: TFloat;
  RX: TFloat;
  RY: TFloat;
  PRX: TFloat;
  PRY: TFloat;
  PX1: TFloat;
  PY1: TFloat;
  radiiCheck: TFloat;
  sign: TFloat;
  sq: TFloat;
  coef: TFloat;
  cx1: TFloat;
  cy1: TFloat;
  sx2: TFloat;
  sy2: TFloat;
  cx: TFloat;
  cy: TFloat;
  ux: TFloat;
  uy: TFloat;
  vx: TFloat;
  vy: TFloat;
  p: TFloat;
  n: TFloat;
  angleStart: TFloat;
  angleExtent: TFloat;
  ArcPath: TGPGraphicsPath;
  Matrix: TGPMatrix;
  Center: TGPPointF;
begin
  if (FStartX = FStopX) and (FStartY = FStopY) then
    Exit;

  if (FRX = 0) or (FRY = 0) then
  begin
    Path.AddLine(FStartX, FStartY, FStopX, FStopY);
    Exit;
  end;

  //
  // Elliptical arc implementation based on the SVG specification notes
  //

  // Compute the half distance between the current and the final point
  dx2 := (FStartX - FStopX) / 2.0;
  dy2 := (FStartY - FStopY) / 2.0;

  // Convert angle from degrees to radians
  angle := DegToRad(FMod(FXRot, 360.0));
  cosAngle := cos(angle);
  sinAngle := sin(angle);

  //
  // Step 1 : Compute (x1, y1)
  //
  x1 := (cosAngle * dx2 + sinAngle * dy2);
  y1 := (-sinAngle * dx2 + cosAngle * dy2);
  // Ensure radii are large enough
  rx := abs(Frx);
  ry := abs(Fry);
  Prx := rx * rx;
  Pry := ry * ry;
  Px1 := x1 * x1;
  Py1 := y1 * y1;

  // check that radii are large enough
  radiiCheck := Px1/Prx + Py1/Pry;
  if (radiiCheck > 1) then
  begin
    rx := sqrt(radiiCheck) * rx;
    ry := sqrt(radiiCheck) * ry;
    Prx := rx * rx;
    Pry := ry * ry;
  end;

  //
  // Step 2 : Compute (cx1, cy1)
  //
  sign := IfThen(FLarge = FSweep, -1, 1);
  sq := ((Prx*Pry)-(Prx*Py1)-(Pry*Px1)) / ((Prx*Py1)+(Pry*Px1));
  sq := IfThen(sq < 0, 0.0, sq);
  coef := (sign * sqrt(sq));
  cx1 := coef * ((rx * y1) / ry);
  cy1 := coef * -((ry * x1) / rx);

  //
  // Step 3 : Compute (cx, cy) from (cx1, cy1)
  //
  sx2 := (FStartX + FStopX) / 2.0;
  sy2 := (FStartY + FStopY) / 2.0;
  cx := sx2 + (cosAngle * cx1 - sinAngle * cy1);
  cy := sy2 + (sinAngle * cx1 + cosAngle * cy1);

  //
  // Step 4 : Compute the angleStart (angle1) and the angleExtent (dangle)
  //
  ux := (x1 - cx1) / rx;
  uy := (y1 - cy1) / ry;
  vx := (-x1 - cx1) / rx;
  vy := (-y1 - cy1) / ry;

  // Compute the angle start
  n := (ux * ux) + (uy * uy);
  n := sqrt(n);
//  n := sqrt((ux * ux) + (uy * uy));
  p := ux; // (1 * ux) + (0 * uy)
  sign := IfThen(uy < 0, -1, 1);
  angleStart := RadToDeg(sign * arccos(p / n));

  // Compute the angle extent
  n := sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy));
  p := ux * vx + uy * vy;
  sign := IfThen(ux * vy - uy * vx < 0, -1, 1);
  angleExtent := RadToDeg(sign * arccos(p / n));
  if ((Fsweep = 0) and (angleExtent > 0)) then
  begin
    angleExtent := angleExtent - 360;
  end else if ((FSweep = 1) and (angleExtent < 0)) then
  begin
    angleExtent := angleExtent + 360;
  end;

  angleStart := FMod(angleStart, 360.0);
  angleExtent := FMod(angleExtent, 360.0);

  R.x := cx - rx;
  R.y := cy - ry;
  R.width := rx * 2.0;
  R.height := ry * 2.0;

  ArcPath := TGPGraphicsPath.Create;
  try
    ArcPath.AddArc(R, angleStart, AngleExtent);
    Matrix := TGPMatrix.Create;
    try
      Center.X := cx;
      Center.Y := cy;
      Matrix.RotateAt(FXRot, Center);
      ArcPath.Transform(Matrix);
    finally
      Matrix.Free;
    end;
    Path.AddPath(ArcPath, True);
  finally
    ArcPath.Free;
  end;
end;

procedure TSVGPathEllipticArc.Read(SL: TStrings; var Position: Integer; Previous: TSVGPathElement);
var
  Command: string;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'A') or (Command = 'a') then
  begin
    TryStrToTFloat(SL[Position + 1], FRX);
    TryStrToTFloat(SL[Position + 2], FRY);
    TryStrToTFloat(SL[Position + 3], FXRot);
    TryStrToInt(SL[Position + 4], FLarge);
    TryStrToInt(SL[Position + 5], FSweep);
    TryStrToTFloat(SL[Position + 6], FStopX);
    TryStrToTFloat(SL[Position + 7], FStopY);
    Inc(Position, 7);

    FRX := Abs(FRX);
    FRY := Abs(FRY);

    if FLarge <> 0 then
      FLarge := 1;

    if FSweep <> 0 then
      FSweep := 1;

    if Command = 'a' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;
end;

// TSVGPathClose

function TSVGPathClose.GetBounds: TFRect;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

function TSVGPathClose.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathClose.Create(Parent);
end;

procedure TSVGPathClose.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
begin
  FStartX := Previous.FStopX;
  FStartY := Previous.FStopY;
  FStopX := FStartX;
  FStopY := FStartY;
end;

procedure TSVGPathClose.AddToPath(Path: TGPGraphicsPath);
begin
  Path.CloseFigure;
end;

end.

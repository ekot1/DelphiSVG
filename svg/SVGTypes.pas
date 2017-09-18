      {******************************************************************}
      { SVG types                                                        }
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

unit SVGTypes;

interface

uses
  System.Math,
  Winapi.Windows, Winapi.GDIPAPI;

const
  INHERIT = -1;

  FontNormal = 0;
  FontItalic = 1;

  MaxTFloat = MaxSingle;

type
  TFloat = single;

  TFPoint = record
    X, Y: TFloat;
    constructor Create(const AX: TFloat; const AY: TFloat);
  end;

  TFRect = record
    Left, Top,
    Width, Height: TFloat;
  end;

  TListOfPoints = array of TFPoint;
  PListOfPoints = ^TListOfPoints;

  TSingleA = array of Single;
  PSingleA = ^TSingleA;

  TRectarray = packed array of TRect;
  PRectArray = ^TRectArray;

  TTextDecoration = set of (tdInherit, tdUnderLine, tdOverLine, tdStrikeOut);

  TTextPathMethod = (tpmAlign, tpmStretch);

  TTextPathSpacing = (tpsAuto, tpsExact);

  TSVGUnit = (suNone, suPX, suPT, suPC, suMM, suCM, suIN, suEM, suEX, suPercent);

  TGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);

  TBounds = record
    TopLeft: TFPoint;
    TopRight: TFPoint;
    BottomLeft: TFPoint;
    BottomRight: TFPoint;
  end;

function ToGPPoint(const Point: TFPoint): TGPPointF;

function Intersect(const Bounds: TBounds; const Rect: TRect): Boolean;

implementation

constructor TFPoint.Create(const AX: TFloat; const AY: TFloat);
begin
  X := AX;
  Y := AY;
end;

function ToGPPoint(const Point: TFPoint): TGPPointF;
begin
  Result := MakePoint(Point.X, Point.Y);
end;

function Intersect(const Bounds: TBounds; const Rect: TRect): Boolean;
var
  R1, R2: THandle;
  P: array[0..3] of TPoint;
begin
  P[0].X := Round(Bounds.TopLeft.X);
  P[0].Y := Round(Bounds.TopLeft.Y);

  P[1].X := Round(Bounds.TopRight.X);
  P[1].Y := Round(Bounds.TopRight.Y);

  P[2].X := Round(Bounds.BottomRight.X);
  P[2].Y := Round(Bounds.BottomRight.Y);

  P[3].X := Round(Bounds.BottomLeft.X);
  P[3].Y := Round(Bounds.BottomLeft.Y);

  R1 := CreatePolygonRgn(P, 4, ALTERNATE);
  R2 := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);

  Result := CombineRgn(R1, R1, R2, RGN_AND) <> NULLREGION;

  DeleteObject(R1);
  DeleteObject(R2);
end;

end.

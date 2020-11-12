      {******************************************************************}
      { Parse of SVG property values                                     }
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

unit SVGParse;

interface

uses
  System.Types, System.Classes, System.Math.Vectors,
  SVGTypes;

function ParseAngle(const Angle: string): TFloat;

function ParsePercent(const S: string): TFloat;

function ParseInteger(const S: string): Integer;

function ParseLength(const S: string): TFloat;

function ParseUnit(const S: string): TSVGUnit;

function ParseDRect(const S: string): TRectF;

function ParseURI(const URI: string): string;

function ParseTransform(const ATransform: string): TMatrix;

implementation

uses
  System.SysUtils, System.Math, System.StrUtils,
  SVGCommon;

function ParseAngle(const Angle: string): TFloat;
var
  D: TFloat;
  C: Integer;
  S: string;
begin
  if Angle <> '' then
  begin
    S := Angle;
    C := Pos('deg', S);
    if C <> 0 then
    begin
      S := LeftStr(S, C - 1);
      if TryStrToTFloat(S, D) then
        Result := DegToRad(D)
      else
        Result := 0;
      Exit;
    end;

    C := Pos('rad', S);
    if C <> 0 then
    begin
      TryStrToTFloat(S, Result);
      Exit;
    end;

    C := Pos('grad', S);
    if C <> 0 then
    begin
      S := LeftStr(S, C - 1);
      if TryStrToTFloat(S, D) then
        Result := GradToRad(D)
      else
        Result := 0;
      Exit;
    end;

    if TryStrToTFloat(S, D) then
      Result := DegToRad(D)
    else
      Result := 0;
  end else
    Result := 0;
end;

function ParsePercent(const S: string): TFloat;
begin
  if Length(S) = 0 then
  begin
    Result := -1;
  end
  else
  begin
    if S[Length(S)] = '%' then
      Result := StrToTFloat(LeftStr(S, Length(S) - 1)) / 100
    else
      Result := StrToTFloat(S);
  end;
end;

function ParseInteger(const S: string): Integer;
begin
  Result := StrToInt(S);
end;

const
  CFactors: array [TSVGUnit] of TFloat =
    (1,       // suNone
     1,       // suPX
     1.33,    // suPT
     12*1.33, // suPC
     96/25.4, // suMM
     96/2.54, // suCM
     96,      // suIN
     16,      // suEM
     1,       // suEX
     1);      // suPercent

function ParseLength(const S: string): TFloat;
var
  SVGUnit: TSVGUnit;
  Number: TFloat;
  N: string;
begin
  SVGUnit := ParseUnit(S);
  case SVGUnit of
    suPercent: N := Copy(S, 1, Length(S) - 1);
    suNone   : N := S;
  else
    N := Copy(S, 1, Length(S) - 2);
  end;
  Number := StrToTFloat(N);

  Result := Number * CFactors[SVGUnit];
end;

function ParseUnit(const S: string): TSVGUnit;
var
  Suffix: string;
begin
  Suffix := RightStr(S, 2);
  if Suffix = 'px' then
  begin
    Result := suPx;
  end
  else if Suffix = 'pt' then
  begin
    Result := suPt;
  end
  else if Suffix = 'pc' then
  begin
    Result := suPC;
  end
  else if Suffix = 'mm' then
  begin
    Result := suMM;
  end
  else if Suffix = 'cm' then
  begin
    Result := suCM;
  end
  else  if Suffix = 'in' then
  begin
    Result := suIN;
  end
  else if Suffix = 'em' then
  begin
    Result := suEM;
  end
  else if Suffix = 'ex' then
  begin
    Result := suEX;
  end
  else if Suffix.EndsWith('%') then
  begin
    Result := suPercent;
  end
  else
  begin
    Result := suNone;
  end;
end;

function GetValues(const S: string; const Delimiter: Char): TStrings;
var
  C: Integer;
begin
  Result := TStringList.Create;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := S;

  for C := Result.Count - 1 downto 0 do
  begin
    if Result[C] = '' then
    begin
      Result.Delete(C);
    end;
  end;
end;

function ParseDRect(const S: string): TRectF;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(Trim(S), ' ');

  try
    if SL.Count = 4 then
    begin
      Result.Left := ParseLength(SL[0]);
      Result.Top := ParseLength(SL[1]);
      Result.Width := ParseLength(SL[2]);
      Result.Height := ParseLength(SL[3]);
    end;
  finally
    SL.Free;
  end;
end;

function ParseURI(const URI: string): string;
const
  CUriPrefix = 'url(#';
  CUriPrefixLength = Length(CUriPrefix);
var
  S: string;
begin
  if Length(URI) = 0 then
  begin
    Result := '';
  end
  else
  begin
    S := Trim(URI);

    if S.StartsWith(CUriPrefix) and S.EndsWith(')') then
    begin
      Result := Copy(S, CUriPrefixLength + 1, Length(S) - (CUriPrefixLength + 1));
    end
    else
    begin
      Result := '';
    end;
  end;
end;

function GetMatrix(const S: string): TMatrix;
var
  SL: TStrings;
begin
  Result := TMatrix.Identity;
  SL := GetValues(S, ',');
  try
    if SL.Count = 6 then
    begin
      Result.m11 := StrToTFloat(SL[0]);
      Result.m12 := StrToTFloat(SL[1]);
      Result.m21 := StrToTFloat(SL[2]);
      Result.m22 := StrToTFloat(SL[3]);
      Result.m31 := StrToTFloat(SL[4]);
      Result.m32 := StrToTFloat(SL[5]);
    end;
  finally
    SL.Free;
  end;
end;

function GetTranslate(const S: string): TMatrix;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);
  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
      SL.Add('0');

    if SL.Count = 2 then
    begin
      Result := TMatrix.CreateTranslation(StrToTFloat(SL[0]), StrToTFloat(SL[1]));
    end;
  finally
    SL.Free;
  end;
end;

function GetScale(const S: string): TMatrix;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);
  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
      SL.Add(SL[0]);
    if SL.Count = 2 then
    begin
      Result := TMatrix.CreateScaling(StrToTFloat(SL[0]), StrToTFloat(SL[1]));
    end;
  finally
    SL.Free;
  end;
end;

function GetRotation(const S: string): TMatrix;
var
  SL: TStrings;
  X, Y, Angle: TFloat;
begin
  SL := GetValues(S, ',');
  try
    Angle := ParseAngle(SL[0]);

    if SL.Count = 3 then
    begin
      X := StrToTFloat(SL[1]);
      Y := StrToTFloat(SL[2]);
    end else
    begin
      X := 0;
      Y := 0;
    end;
  finally
    SL.Free;
  end;

  Result := TMatrix.CreateTranslation(X, Y);
  Result := TMatrix.CreateRotation(Angle) * Result;
  Result := TMatrix.CreateTranslation(-X, -Y) * Result;
end;

function GetSkewX(const S: string): TMatrix;
var
  SL: TStrings;
  Angle: TFloat;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
    begin
      Result := TMatrix.Identity;
      Angle := ParseAngle(SL[0]);
      Result.m21 := Tan(Angle);
    end;
  finally
    SL.Free;
  end;
end;

function GetSkewY(const S: string): TMatrix;
var
  SL: TStrings;
  Angle: TFloat;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
    begin
      Result := TMatrix.Identity;
      Angle := ParseAngle(SL[0]);
      Result.m12 := Tan(Angle);
    end;
  finally
    SL.Free;
  end;
end;

function ParseTransform(const ATransform: string): TMatrix;
var
  Start: Integer;
  Stop: Integer;
  TType: string;
  Values: string;
  S: string;
  M: TMatrix;
begin
  FillChar(Result, SizeOf(Result), 0);

  S := Trim(ATransform);

  while S <> '' do
  begin
    Start := Pos('(', S);
    Stop := Pos(')', S);
    if (Start = 0) or (Stop = 0) then
      Exit;
    TType := Copy(S, 1, Start - 1);
    Values := Trim(Copy(S, Start + 1, Stop - Start - 1));
    Values := StringReplace(Values, ' ', ',', [rfReplaceAll]);
    M.m33 := 0;

    if TType = 'matrix' then
    begin
      M := GetMatrix(Values);
    end
    else if TType = 'translate' then
    begin
      M := GetTranslate(Values);
    end
    else if TType = 'scale' then
    begin
      M := GetScale(Values);
    end
    else if TType = 'rotate' then
    begin
      M := GetRotation(Values);
    end
    else if TType = 'skewX' then
    begin
      M := GetSkewX(Values);
    end
    else if TType = 'skewY' then
    begin
      M := GetSkewY(Values);
    end;

    if M.m33 = 1 then
    begin
      if Result.m33 = 0 then
        Result := M
      else
        Result := M * Result;
    end;

    S := Trim(Copy(S, Stop + 1, Length(S)));
  end;
end;

end.

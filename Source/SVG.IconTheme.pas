unit SVG.IconTheme;

interface

uses
  System.UITypes;

type
  TIconTheme = class
  public
    /// Base color for icons (normal state)
    class function GetPrimaryColor: TAlphaColor; static;

    /// Secondary color (for duotone 2nd layer)
    class function GetSecondaryColor: TAlphaColor; static;

    /// Disabled icon color
    class function GetDisabledColor: TAlphaColor; static;

    /// Lighten color by 0..1 (0 = no change, 1 = white)
    class function Lighten(const AColor: TAlphaColor; Factor: Single): TAlphaColor; static;

    /// Darken color by 0..1 (0 = no change, 1 = black)
    class function Darken(const AColor: TAlphaColor; Factor: Single): TAlphaColor; static;
  end;

implementation

uses
  System.Math, System.SysUtils;

class function TIconTheme.GetPrimaryColor: TAlphaColor;
begin
  // Adjust to your app’s primary accent color
  Result := $FF1E88E5; // material-ish blue
end;

class function TIconTheme.GetSecondaryColor: TAlphaColor;
begin
  // Secondary layer for duotone
  Result := $FF90CAF9; // lighter blue
end;

class function TIconTheme.GetDisabledColor: TAlphaColor;
begin
  Result := $FFB0BEC5; // grey-ish
end;

class function TIconTheme.Lighten(const AColor: TAlphaColor; Factor: Single): TAlphaColor;
var
  A, R, G, B: Byte;
begin
  if Factor <= 0 then
    Exit(AColor);
  if Factor > 1 then
    Factor := 1;

  A := AColor shr 24;
  R := (AColor shr 16) and $FF;
  G := (AColor shr 8) and $FF;
  B := AColor and $FF;

  R := Min(255, R + Round((255 - R) * Factor));
  G := Min(255, G + Round((255 - G) * Factor));
  B := Min(255, B + Round((255 - B) * Factor));

  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

class function TIconTheme.Darken(const AColor: TAlphaColor; Factor: Single): TAlphaColor;
var
  A, R, G, B: Byte;
begin
  if Factor <= 0 then
    Exit(AColor);
  if Factor > 1 then
    Factor := 1;

  A := AColor shr 24;
  R := (AColor shr 16) and $FF;
  G := (AColor shr 8) and $FF;
  B := AColor and $FF;

  R := Max(0, R - Round(R * Factor));
  G := Max(0, G - Round(G * Factor));
  B := Max(0, B - Round(B * Factor));

  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

end.

unit SVG.SVGButton;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.Objects, FMX.Graphics, FMX.Layouts,
  SVG.IconManager, SVG.IconTheme;

type
  TIconAlignment = (iaLeft, iaRight, iaTop, iaBottom, iaCenter);

  TSVGButton = class(TButton)
  private
    FIconName: string;
    FIconSize: Integer;
    FIconAlignment: TIconAlignment;
    FIconPadding: TBounds;
    FIconLayout: TLayout;
    FBasePath: TPath;
    FOverlayPath: TPath;
    FTextObj: TText;
    FIconColor: TAlphaColor;
    FIconColorSet: Boolean;
    function GetIconColor: TAlphaColor;
    function GetIconColorDisabled: TAlphaColor;
    function GetIconColorDown: TAlphaColor;
    function GetIconColorHover: TAlphaColor;
    procedure IconPaddingChanged(Sender: TObject);
    procedure SetIconAlignment(const Value: TIconAlignment);
    procedure SetIconName(const Value: string);
    procedure SetIconPadding(const Value: TBounds);
    procedure SetIconSize(const Value: Integer);
    procedure UpdateIconColors;
    procedure UpdateIconGeometry;
    procedure UpdateIconLayout;
    procedure UpdateTextLayout;
  protected
    procedure ApplyStyle; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoRealign; override;
    procedure DoStyleChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFontColor(const AColor: TAlphaColor);
    procedure SetIconColor(const AColor: TAlphaColor);
  published
    property IconName: string read FIconName write SetIconName;
    property IconSize: Integer read FIconSize write SetIconSize default 16;
    property IconAlignment: TIconAlignment read FIconAlignment write SetIconAlignment default iaLeft;
    property IconPadding: TBounds read FIconPadding write SetIconPadding;
  end;

  TSVGSpeedButton = class(TSpeedButton)
  private
    FIconName: string;
    FIconSize: Integer;
    FIconAlignment: TIconAlignment;
    FIconPadding: TBounds;
    FIconLayout: TLayout;
    FBasePath: TPath;
    FOverlayPath: TPath;
    FTextObj: TText;
    FIconColor: TAlphaColor;
    FIconColorSet: Boolean;
    function GetIconColor: TAlphaColor;
    function GetIconColorDisabled: TAlphaColor;
    function GetIconColorDown: TAlphaColor;
    function GetIconColorHover: TAlphaColor;
    procedure IconPaddingChanged(Sender: TObject);
    procedure SetIconAlignment(const Value: TIconAlignment);
    procedure SetIconName(const Value: string);
    procedure SetIconPadding(const Value: TBounds);
    procedure SetIconSize(const Value: Integer);
    procedure UpdateIconColors;
    procedure UpdateIconGeometry;
    procedure UpdateIconLayout;
    procedure UpdateTextLayout;
  protected
    procedure ApplyStyle; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoRealign; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFontColor(const AColor: TAlphaColor);
    procedure SetIconColor(const AColor: TAlphaColor);
  published
    property IconName: string read FIconName write SetIconName;
    property IconSize: Integer read FIconSize write SetIconSize default 16;
    property IconAlignment: TIconAlignment read FIconAlignment write SetIconAlignment default iaCenter;
    property IconPadding: TBounds read FIconPadding write SetIconPadding;
  end;

procedure Register;

implementation

uses
  System.Math;

{ Helpers }

function FindTextControl(Styled: TStyledControl): TText;
var
  Obj: TFmxObject;
begin
  Result := nil;
  if not Assigned(Styled) then
    Exit;

  Obj := Styled.FindStyleResource('text');
  if (Obj <> nil) and (Obj is TText) then
    Exit(TText(Obj));

  Obj := Styled.FindStyleResource('label');
  if (Obj <> nil) and (Obj is TText) then
    Exit(TText(Obj));

  Obj := Styled.FindStyleResource('buttontext');
  if (Obj <> nil) and (Obj is TText) then
    Exit(TText(Obj));
end;

procedure PositionIconLayout(const BtnRect: TRectF; IconSize: Single; const Padding: TBounds; Align: TIconAlignment;
  IconLayout: TLayout);
var
  R: TRectF;
begin
  if not Assigned(IconLayout) then
    Exit;

  R := BtnRect;
  R.Left := R.Left + Padding.Left;
  R.Top := R.Top + Padding.Top;
  R.Right := R.Right - Padding.Right;
  R.Bottom := R.Bottom - Padding.Bottom;

  IconLayout.Width := IconSize;
  IconLayout.Height := IconSize;

  case Align of
    iaLeft:
      begin
        IconLayout.Position.X := R.Left;
        IconLayout.Position.Y := R.Top + (R.Height - IconSize) / 2;
      end;
    iaRight:
      begin
        IconLayout.Position.X := R.Right - IconSize;
        IconLayout.Position.Y := R.Top + (R.Height - IconSize) / 2;
      end;
    iaTop:
      begin
        IconLayout.Position.X := R.Left + (R.Width - IconSize) / 2;
        IconLayout.Position.Y := R.Top;
      end;
    iaBottom:
      begin
        IconLayout.Position.X := R.Left + (R.Width - IconSize) / 2;
        IconLayout.Position.Y := R.Bottom - IconSize;
      end;
  else // iaCenter
    begin
      IconLayout.Position.X := R.Left + (R.Width - IconSize) / 2;
      IconLayout.Position.Y := R.Top + (R.Height - IconSize) / 2;
    end;
  end;
end;

procedure AdjustTextMargins(Text: TText; const BtnRect: TRectF; IconSize: Single; const Padding: TBounds;
  Align: TIconAlignment);
var
  IconBoxW, IconBoxH: Single;
  Gap: Single;
begin
  if not Assigned(Text) then
    Exit;

  IconBoxW := IconSize + Padding.Left + Padding.Right;
  IconBoxH := IconSize + Padding.Top + Padding.Bottom;
  Gap := 4; // extra spacing between icon & text

  // reset
  Text.Margins.Left := 0;
  Text.Margins.Right := 0;
  Text.Margins.Top := 0;
  Text.Margins.Bottom := 0;

  case Align of
    iaLeft:
      begin
        // text region starts after icon box
        Text.Margins.Left := IconBoxW + Gap;
      end;
    iaRight:
      begin
        // text region ends before icon box
        Text.Margins.Right := IconBoxW + Gap;
      end;
    iaTop:
      begin
        // text goes below icon
        Text.Margins.Top := IconBoxH + Gap;
      end;
    iaBottom:
      begin
        // text above icon
        Text.Margins.Bottom := IconBoxH + Gap;
      end;
    iaCenter:
      begin
        if Text.Text.Trim <> '' then
        begin
          // classic icon+text: icon centered, text below
          Text.Margins.Top := IconBoxH + Gap;
        end;
      end;
  end;
end;

{ TSVGButton }

constructor TSVGButton.Create(AOwner: TComponent);
begin
  inherited;

  FIconSize := 16;
  FIconAlignment := iaLeft;

  FIconPadding := TBounds.Create(TRectF.Empty);
  FIconPadding.OnChange := IconPaddingChanged;

  // Icon layout container
  FIconLayout := TLayout.Create(Self);
  FIconLayout.Parent := Self;
  FIconLayout.Stored := False;
  FIconLayout.HitTest := False;
  FIconLayout.Align := TAlignLayout.None;

  // Base path
  FBasePath := TPath.Create(FIconLayout);
  FBasePath.Parent := FIconLayout;
  FBasePath.Stored := False;
  FBasePath.HitTest := False;
  FBasePath.Align := TAlignLayout.Contents;
  FBasePath.WrapMode := TPathWrapMode.Stretch;
  FBasePath.Stroke.Kind := TBrushKind.None;

  // Overlay path (duotone)
  FOverlayPath := TPath.Create(FIconLayout);
  FOverlayPath.Parent := FIconLayout;
  FOverlayPath.Stored := False;
  FOverlayPath.HitTest := False;
  FOverlayPath.Align := TAlignLayout.Contents;
  FOverlayPath.WrapMode := TPathWrapMode.Stretch;
  FOverlayPath.Stroke.Kind := TBrushKind.None;
  FOverlayPath.Visible := False;
end;

destructor TSVGButton.Destroy;
begin
  FIconPadding.Free;
  inherited;
end;

procedure TSVGButton.ApplyStyle;
begin
  inherited;

  FTextObj := FindTextControl(Self);
  if FTextObj <> nil then
  begin
    UpdateIconGeometry;
    UpdateIconColors;
    UpdateTextLayout;
  end;
end;

procedure TSVGButton.IconPaddingChanged(Sender: TObject);
begin
  UpdateIconLayout;
end;

procedure TSVGButton.DoMouseEnter;
begin
  inherited;
  if Enabled then
    FBasePath.Fill.Color := GetIconColorHover;
end;

procedure TSVGButton.DoMouseLeave;
begin
  inherited;
  UpdateIconColors;
end;

procedure TSVGButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Enabled then
    FBasePath.Fill.Color := GetIconColorDown;
end;

procedure TSVGButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  UpdateIconColors;
end;

procedure TSVGButton.Resize;
begin
  inherited;
  UpdateIconLayout;
end;

procedure TSVGButton.DoRealign;
begin
  inherited;
  UpdateIconLayout;
end;

procedure TSVGButton.DoStyleChanged;
begin
  inherited;

  UpdateIconColors;
end;

function TSVGButton.GetIconColor: TAlphaColor;
begin
  if FIconColorSet then
    Result := FIconColor
  else
    if Assigned(FTextObj) then
      Result := FTextObj.TextSettings.FontColor
    else
      Result := TextSettings.FontColor;
end;

function TSVGButton.GetIconColorDisabled: TAlphaColor;
begin
  Result := TIconTheme.GetDisabledColor;
end;

function TSVGButton.GetIconColorDown: TAlphaColor;
begin
  Result := TIconTheme.Darken(GetIconColor, 0.15);
end;

function TSVGButton.GetIconColorHover: TAlphaColor;
begin
  Result := TIconTheme.Lighten(GetIconColor, 0.15);
end;

procedure TSVGButton.SetFontColor(const AColor: TAlphaColor);
begin
  StyledSettings := StyledSettings - [TStyledSetting.FontColor];
  FontColor := AColor;
  UpdateIconColors;
end;

procedure TSVGButton.SetIconAlignment(const Value: TIconAlignment);
begin
  if FIconAlignment <> Value then
  begin
    FIconAlignment := Value;
    UpdateIconLayout;
  end;
end;

procedure TSVGButton.SetIconColor(const AColor: TAlphaColor);
begin
  FIconColor := AColor;
  FIconColorSet := True;
  UpdateIconColors;
  Repaint;
end;

procedure TSVGButton.SetIconName(const Value: string);
begin
  if FIconName <> Value then
  begin
    FIconName := Value;

    // If style not yet loaded, defer update
    if (FBasePath <> nil) and (FIconLayout <> nil) then
      UpdateIconGeometry
    else
      Repaint; // schedule update for later
  end;
end;

procedure TSVGButton.SetIconPadding(const Value: TBounds);
begin
  FIconPadding.Assign(Value);
end;

procedure TSVGButton.SetIconSize(const Value: Integer);
begin
  if FIconSize <> Value then
  begin
    FIconSize := Max(Value, 4);
    UpdateIconLayout;
  end;
end;

procedure TSVGButton.UpdateIconGeometry;
var
  L1, L2: string;
begin
  FBasePath.Data.Clear;
  FOverlayPath.Data.Clear;
  FOverlayPath.Visible := False;

  if (IconManager = nil) or FIconName.Trim.IsEmpty then
    Exit;

  if not IconManager.TryGetPathData(FIconName, L1, L2) then
    Exit;

  FBasePath.Data.Data := L1;

  if L2 <> '' then
  begin
    FOverlayPath.Visible := True;
    FOverlayPath.Data.Data := L2;
  end;

  UpdateIconLayout;
end;

procedure TSVGButton.UpdateIconLayout;
begin
  PositionIconLayout(LocalRect, FIconSize, FIconPadding, FIconAlignment, FIconLayout);
  UpdateTextLayout;
end;

procedure TSVGButton.UpdateTextLayout;
begin
  AdjustTextMargins(FTextObj, LocalRect, FIconSize, FIconPadding, FIconAlignment);
end;

procedure TSVGButton.UpdateIconColors;
begin
  if not Enabled then
  begin
    FBasePath.Fill.Color := GetIconColorDisabled;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := GetIconColorDisabled;
  end
  else
  begin
    FBasePath.Fill.Color := GetIconColor;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := GetIconColor;
  end;
end;

{ TSVGSpeedButton }

constructor TSVGSpeedButton.Create(AOwner: TComponent);
begin
  inherited;

  FIconSize := 16;
  FIconAlignment := iaLeft;

  FIconPadding := TBounds.Create(TRectF.Empty);
  FIconPadding.OnChange := IconPaddingChanged;

  FIconLayout := TLayout.Create(Self);
  FIconLayout.Parent := Self;
  FIconLayout.Stored := False;
  FIconLayout.HitTest := False;
  FIconLayout.Align := TAlignLayout.None;

  FBasePath := TPath.Create(FIconLayout);
  FBasePath.Parent := FIconLayout;
  FBasePath.Stored := False;
  FBasePath.HitTest := False;
  FBasePath.Align := TAlignLayout.Contents;
  FBasePath.WrapMode := TPathWrapMode.Stretch;
  FBasePath.Stroke.Kind := TBrushKind.None;

  FOverlayPath := TPath.Create(FIconLayout);
  FOverlayPath.Parent := FIconLayout;
  FOverlayPath.Stored := False;
  FOverlayPath.HitTest := False;
  FOverlayPath.Align := TAlignLayout.Contents;
  FOverlayPath.WrapMode := TPathWrapMode.Stretch;
  FOverlayPath.Stroke.Kind := TBrushKind.None;
  FOverlayPath.Visible := False;
end;

destructor TSVGSpeedButton.Destroy;
begin
  FIconPadding.Free;
  inherited;
end;

procedure TSVGSpeedButton.ApplyStyle;
begin
  inherited;

  FTextObj := FindTextControl(Self);
  if FTextObj <> nil then
  begin
    UpdateIconGeometry;
    UpdateIconColors;
    UpdateTextLayout;
  end;
end;

procedure TSVGSpeedButton.IconPaddingChanged(Sender: TObject);
begin
  UpdateIconLayout;
end;

procedure TSVGSpeedButton.DoMouseEnter;
begin
  inherited;
  if Enabled then
    FBasePath.Fill.Color := GetIconColorHover;
end;

procedure TSVGSpeedButton.DoMouseLeave;
begin
  inherited;
  UpdateIconColors;
end;

procedure TSVGSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Enabled then
    FBasePath.Fill.Color := GetIconColorDown;
end;

procedure TSVGSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  UpdateIconColors;
end;

procedure TSVGSpeedButton.Resize;
begin
  inherited;
  UpdateIconLayout;
end;

procedure TSVGSpeedButton.DoRealign;
begin
  inherited;
  UpdateIconLayout;
end;

function TSVGSpeedButton.GetIconColor: TAlphaColor;
begin
  if FIconColorSet then
    Result := FIconColor
  else
    if Assigned(FTextObj) then
      Result := FTextObj.TextSettings.FontColor
    else
      Result := TextSettings.FontColor;
end;

function TSVGSpeedButton.GetIconColorDisabled: TAlphaColor;
begin
  Result := TIconTheme.GetDisabledColor;
end;

function TSVGSpeedButton.GetIconColorDown: TAlphaColor;
begin
  Result := TIconTheme.Darken(GetIconColor, 0.15);
end;

function TSVGSpeedButton.GetIconColorHover: TAlphaColor;
begin
  Result := TIconTheme.Lighten(GetIconColor, 0.15);
end;

procedure TSVGSpeedButton.SetFontColor(const AColor: TAlphaColor);
begin
  StyledSettings := StyledSettings - [TStyledSetting.FontColor];
  FontColor := AColor;
  UpdateIconColors;
end;

procedure TSVGSpeedButton.SetIconAlignment(const Value: TIconAlignment);
begin
  if FIconAlignment <> Value then
  begin
    FIconAlignment := Value;
    UpdateIconLayout;
  end;
end;

procedure TSVGSpeedButton.SetIconColor(const AColor: TAlphaColor);
begin
  FIconColor := AColor;
  FIconColorSet := True;
  UpdateIconColors;
  Repaint;
end;

procedure TSVGSpeedButton.SetIconName(const Value: string);
begin
  if FIconName <> Value then
  begin
    FIconName := Value;

    // If style not yet loaded, defer update
    if (FBasePath <> nil) and (FIconLayout <> nil) then
      UpdateIconGeometry
    else
      Repaint; // schedule update for later
  end;
end;

procedure TSVGSpeedButton.SetIconPadding(const Value: TBounds);
begin
  FIconPadding.Assign(Value);
end;

procedure TSVGSpeedButton.SetIconSize(const Value: Integer);
begin
  if FIconSize <> Value then
  begin
    FIconSize := Max(Value, 4);
    UpdateIconLayout;
  end;
end;

procedure TSVGSpeedButton.UpdateIconGeometry;
var
  L1, L2: string;
begin
  FBasePath.Data.Clear;
  FOverlayPath.Data.Clear;
  FOverlayPath.Visible := False;

  if (IconManager = nil) or FIconName.Trim.IsEmpty then
    Exit;

  if not IconManager.TryGetPathData(FIconName, L1, L2) then
    Exit;

  FBasePath.Data.Data := L1;

  if L2 <> '' then
  begin
    FOverlayPath.Visible := True;
    FOverlayPath.Data.Data := L2;
  end;

  UpdateIconLayout;
  UpdateIconColors;
end;

procedure TSVGSpeedButton.UpdateIconLayout;
begin
  PositionIconLayout(LocalRect, FIconSize, FIconPadding, FIconAlignment, FIconLayout);
  UpdateTextLayout;
end;

procedure TSVGSpeedButton.UpdateTextLayout;
begin
  AdjustTextMargins(FTextObj, LocalRect, FIconSize, FIconPadding, FIconAlignment);
end;

procedure TSVGSpeedButton.UpdateIconColors;
begin
  if not Enabled then
  begin
    FBasePath.Fill.Color := GetIconColorDisabled;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := GetIconColorDisabled;
  end
  else
  begin
    FBasePath.Fill.Color := GetIconColor;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := GetIconColor;
  end;
end;

procedure Register;
begin
  RegisterComponents('GD', [TSVGButton, TSVGSpeedButton]);
end;

end.

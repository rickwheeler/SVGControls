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

    FIconColor: TAlphaColor;
    FIconColorHover: TAlphaColor;
    FIconColorDown: TAlphaColor;
    FIconColorDisabled: TAlphaColor;
    FIconSecondaryColor: TAlphaColor;

    FIconLayout: TLayout;
    FBasePath: TPath;
    FOverlayPath: TPath;

    FTextObj: TText;

    FHoverSetManually: Boolean;
    FDownSetManually: Boolean;
    FDisabledSetManually: Boolean;

    procedure IconPaddingChanged(Sender: TObject);

    procedure SetIconName(const Value: string);
    procedure SetIconSize(const Value: Integer);
    procedure SetIconAlignment(const Value: TIconAlignment);
    procedure SetIconPadding(const Value: TBounds);

    procedure SetIconColor(const Value: TAlphaColor);
    procedure SetIconColorDisabled(const Value: TAlphaColor);
    procedure SetIconColorDown(const Value: TAlphaColor);
    procedure SetIconColorHover(const Value: TAlphaColor);
    procedure SetIconSecondaryColor(const Value: TAlphaColor);

    procedure UpdateIconGeometry;
    procedure UpdateIconLayout;
    procedure UpdateTextLayout;
    procedure UpdateIconColors;
  protected
    procedure ApplyStyle; override;
    procedure Resize; override;
    procedure DoRealign; override;

    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property IconName: string read FIconName write SetIconName;
    property IconSize: Integer read FIconSize write SetIconSize default 20;
    property IconAlignment: TIconAlignment read FIconAlignment write SetIconAlignment default iaLeft;
    property IconPadding: TBounds read FIconPadding write SetIconPadding;

    property IconColor: TAlphaColor read FIconColor write SetIconColor;
    property IconColorHover: TAlphaColor read FIconColorHover write SetIconColorHover;
    property IconColorDown: TAlphaColor read FIconColorDown write SetIconColorDown;
    property IconColorDisabled: TAlphaColor read FIconColorDisabled write SetIconColorDisabled;

    property IconSecondaryColor: TAlphaColor read FIconSecondaryColor write SetIconSecondaryColor;
  end;

  TSVGSpeedButton = class(TSpeedButton)
  private
    FIconName: string;
    FIconSize: Integer;
    FIconAlignment: TIconAlignment;
    FIconPadding: TBounds;

    FIconColor: TAlphaColor;
    FIconColorHover: TAlphaColor;
    FIconColorDown: TAlphaColor;
    FIconColorDisabled: TAlphaColor;
    FIconSecondaryColor: TAlphaColor;

    FIconLayout: TLayout;
    FBasePath: TPath;
    FOverlayPath: TPath;

    FTextObj: TText;

    FHoverSetManually: Boolean;
    FDownSetManually: Boolean;
    FDisabledSetManually: Boolean;

    procedure IconPaddingChanged(Sender: TObject);

    procedure SetIconName(const Value: string);
    procedure SetIconSize(const Value: Integer);
    procedure SetIconAlignment(const Value: TIconAlignment);
    procedure SetIconPadding(const Value: TBounds);

    procedure SetIconColor(const Value: TAlphaColor);
    procedure SetIconColorDisabled(const Value: TAlphaColor);
    procedure SetIconColorDown(const Value: TAlphaColor);
    procedure SetIconColorHover(const Value: TAlphaColor);
    procedure SetIconSecondaryColor(const Value: TAlphaColor);

    procedure UpdateIconGeometry;
    procedure UpdateIconLayout;
    procedure UpdateTextLayout;
    procedure UpdateIconColors;
  protected
    procedure ApplyStyle; override;
    procedure Resize; override;
    procedure DoRealign; override;

    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property IconName: string read FIconName write SetIconName;
    property IconSize: Integer read FIconSize write SetIconSize default 20;
    property IconAlignment: TIconAlignment read FIconAlignment write SetIconAlignment default iaCenter;
    property IconPadding: TBounds read FIconPadding write SetIconPadding;

    property IconColor: TAlphaColor read FIconColor write SetIconColor;
    property IconColorHover: TAlphaColor read FIconColorHover write SetIconColorHover;
    property IconColorDown: TAlphaColor read FIconColorDown write SetIconColorDown;
    property IconColorDisabled: TAlphaColor read FIconColorDisabled write SetIconColorDisabled;

    property IconSecondaryColor: TAlphaColor read FIconSecondaryColor write SetIconSecondaryColor;
  end;

procedure Register;

implementation

uses
  System.Math;

{ ----------------------------- Helpers ------------------------------ }

//function ApplyOpacity(Color: TAlphaColor; Opacity: Single): TAlphaColor;
//var
//  C: TAlphaColorRec;
//begin
//  C := TAlphaColorRec(Color);
//  C.A := Round(C.A * Opacity);
//  Result := TAlphaColor(C);
//end;

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

{ ========================== TSVGButton ========================= }

constructor TSVGButton.Create(AOwner: TComponent);
begin
  inherited;

  FIconSize := 16;
  FIconAlignment := iaLeft;

  FIconColor := TIconTheme.GetPrimaryColor;
  FIconColorHover := TIconTheme.Lighten(FIconColor, 0.15);
  FIconColorDown := TIconTheme.Darken(FIconColor, 0.15);
  FIconColorDisabled := TIconTheme.GetDisabledColor;
  FIconSecondaryColor := TIconTheme.GetSecondaryColor;

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
  UpdateTextLayout;
  if FIconName <> '' then
    UpdateIconGeometry;
  UpdateIconColors;
end;

procedure TSVGButton.IconPaddingChanged(Sender: TObject);
begin
  UpdateIconLayout;
end;

procedure TSVGButton.DoMouseEnter;
begin
  inherited;
  if Enabled then
    FBasePath.Fill.Color := FIconColorHover;
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
    FBasePath.Fill.Color := FIconColorDown;
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

procedure TSVGButton.SetIconAlignment(const Value: TIconAlignment);
begin
  if FIconAlignment <> Value then
  begin
    FIconAlignment := Value;
    UpdateIconLayout;
  end;
end;

procedure TSVGButton.SetIconColor(const Value: TAlphaColor);
begin
  if FIconColor <> Value then
  begin
    FIconColor := Value;

    if not FHoverSetManually then
      FIconColorHover := TIconTheme.Lighten(Value, 0.15);

    if not FDownSetManually then
      FIconColorDown := TIconTheme.Darken(Value, 0.15);

    if not FDisabledSetManually then
      FIconColorDisabled := TIconTheme.GetDisabledColor;

    UpdateIconColors;
  end;
end;

procedure TSVGButton.SetIconColorDisabled(const Value: TAlphaColor);
begin
  if FIconColorDisabled <> Value then
  begin
    FIconColorDisabled := Value;
    FDisabledSetManually := True;
    UpdateIconColors;
  end;
end;

procedure TSVGButton.SetIconColorDown(const Value: TAlphaColor);
begin
  if FIconColorDown <> Value then
  begin
    FIconColorDown := Value;
    FDownSetManually := True;
    UpdateIconColors;
  end;
end;

procedure TSVGButton.SetIconColorHover(const Value: TAlphaColor);
begin
  if FIconColorHover <> Value then
  begin
    FIconColorHover := Value;
    FHoverSetManually := True;
    UpdateIconColors;
  end;
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

procedure TSVGButton.SetIconSecondaryColor(const Value: TAlphaColor);
begin
  if FIconSecondaryColor <> Value then
  begin
    FIconSecondaryColor := Value;
    UpdateIconColors;
  end;
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
  UpdateIconColors;
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
    FBasePath.Fill.Color := FIconColorDisabled;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := FIconColorDisabled;
  end
  else
  begin
    FBasePath.Fill.Color := FIconColor;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := FIconSecondaryColor;
  end;
end;

{ ====================== TSVGSpeedButton ====================== }

constructor TSVGSpeedButton.Create(AOwner: TComponent);
begin
  inherited;

  FIconSize := 16;
  FIconAlignment := iaLeft;

  FIconColor := TIconTheme.GetPrimaryColor;
  FIconColorHover := TIconTheme.Lighten(FIconColor, 0.15);
  FIconColorDown := TIconTheme.Darken(FIconColor, 0.15);
  FIconColorDisabled := TIconTheme.GetDisabledColor;
  FIconSecondaryColor := TIconTheme.GetSecondaryColor;

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
  UpdateTextLayout;
  if FIconName <> '' then
    UpdateIconGeometry;
  UpdateIconColors;
end;

procedure TSVGSpeedButton.IconPaddingChanged(Sender: TObject);
begin
  UpdateIconLayout;
end;

procedure TSVGSpeedButton.DoMouseEnter;
begin
  inherited;
  if Enabled then
    FBasePath.Fill.Color := FIconColorHover;
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
    FBasePath.Fill.Color := FIconColorDown;
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

procedure TSVGSpeedButton.SetIconAlignment(const Value: TIconAlignment);
begin
  if FIconAlignment <> Value then
  begin
    FIconAlignment := Value;
    UpdateIconLayout;
  end;
end;

procedure TSVGSpeedButton.SetIconColor(const Value: TAlphaColor);
begin
  if FIconColor <> Value then
  begin
    FIconColor := Value;

    if not FHoverSetManually then
      FIconColorHover := TIconTheme.Lighten(Value, 0.15);

    if not FDownSetManually then
      FIconColorDown := TIconTheme.Darken(Value, 0.15);

    if not FDisabledSetManually then
      FIconColorDisabled := TIconTheme.GetDisabledColor;

    UpdateIconColors;
  end;
end;

procedure TSVGSpeedButton.SetIconColorDisabled(const Value: TAlphaColor);
begin
  if FIconColorDisabled <> Value then
  begin
    FIconColorDisabled := Value;
    FDisabledSetManually := True;
    UpdateIconColors;
  end;
end;

procedure TSVGSpeedButton.SetIconColorDown(const Value: TAlphaColor);
begin
  if FIconColorDown <> Value then
  begin
    FIconColorDown := Value;
    FDownSetManually := True;
    UpdateIconColors;
  end;
end;

procedure TSVGSpeedButton.SetIconColorHover(const Value: TAlphaColor);
begin
  if FIconColorHover <> Value then
  begin
    FIconColorHover := Value;
    FHoverSetManually := True;
    UpdateIconColors;
  end;
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

procedure TSVGSpeedButton.SetIconSecondaryColor(const Value: TAlphaColor);
begin
  if FIconSecondaryColor <> Value then
  begin
    FIconSecondaryColor := Value;
    UpdateIconColors;
  end;
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
    FBasePath.Fill.Color := FIconColorDisabled;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := FIconColorDisabled;
  end
  else
  begin
    FBasePath.Fill.Color := FIconColor;
    if FOverlayPath.Visible then
      FOverlayPath.Fill.Color := FIconSecondaryColor;
  end;
end;

procedure Register;
begin
  RegisterComponents('GD', [TSVGButton, TSVGSpeedButton]);
end;

end.

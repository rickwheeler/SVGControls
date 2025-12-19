unit SVG.IconManager;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  FMX.Types, System.UITypes, FMX.Graphics, SVG.Utils;

type

  TIconManager = class
  private
    FIcons: TDictionary<string, TSVGIconData>;
    FSearchPaths: TList<string>;
    FDefaultSize: Integer;

    class var FIconManager: TIconManager;
    function FindSVGFile(const Name: string): string;
    procedure EnsureLoaded(const Name: string);
  public
    class constructor Create;
    class destructor Destroy;
    class function Instance: TIconManager;

    constructor Create;
    destructor Destroy; override;

    procedure AddSearchPath(const Path: string);
    procedure LoadSVGFile(const FileName: string);

    function IconExists(const Name: string): Boolean;

    /// Returns the icon data object (do NOT free it).
    function GetIconData(const Name: string): TSVGIconData;

    /// Convenience: returns just the path data strings (mono/duotone).
    function TryGetPathData(const Name: string; out Layer1, Layer2: string): Boolean;

    /// Render icon to a bitmap (e.g. for non-vector targets).
    function GetBitmap(const Name: string; Size: Integer; Color: TAlphaColor): TBitmap;

    property DefaultSize: Integer read FDefaultSize write FDefaultSize;
  end;

function IconManager: TIconManager;

implementation

uses
  System.IOUtils, FMX.Objects; // , FMX.Canvas;

function IconManager: TIconManager;
begin
  Result := TIconManager.Instance;
end;

{ TIconManager }

class constructor TIconManager.Create;
begin
  FIconManager := TIconManager.Create;
end;

class destructor TIconManager.Destroy;
begin
  FIconManager.Free;
end;

constructor TIconManager.Create;
begin
  inherited;
  FIcons := TDictionary<string, TSVGIconData>.Create;
  FSearchPaths := TList<string>.Create;
  FDefaultSize := 24;
end;

destructor TIconManager.Destroy;
var
  Icon: TSVGIconData;
begin
  for Icon in FIcons.Values do
    Icon.Free;
  FIcons.Free;
  FSearchPaths.Free;
  inherited;
end;

procedure TIconManager.AddSearchPath(const Path: string);
begin
  if (Path <> '') and DirectoryExists(Path) then
    if not FSearchPaths.Contains(Path) then
    begin
      FSearchPaths.Add(Path);

      var Dirs := System.IOUtils.TDirectory.GetDirectories(Path);
      if Length(Dirs) > 0 then
        for var Dir in Dirs do
          AddSearchPath(Dir);
    end;
end;

function TIconManager.FindSVGFile(const Name: string): string;
var
  Base, FN: string;
begin
  Result := '';
  for Base in FSearchPaths do
  begin
    FN := System.IOUtils.TPath.Combine(Base, Name + '.svg');
    if FileExists(FN) then
      Exit(FN);
  end;
end;

procedure TIconManager.LoadSVGFile(const FileName: string);
var
  Icon: TSVGIconData;
  Key: string;
begin
  Icon := TSVGUtils.LoadSVG(FileName);
  Key := Icon.Name.ToLower;

  if FIcons.ContainsKey(Key) then
  begin
    FIcons[Key].Free;
    FIcons.Remove(Key);
  end;

  FIcons.Add(Key, Icon);
end;

procedure TIconManager.EnsureLoaded(const Name: string);
var
  FN: string;
begin
  if FIcons.ContainsKey(Name) then
    Exit;

  FN := FindSVGFile(Name);
  if FN = '' then
    raise Exception.CreateFmt('Icon not found: %s', [Name]);

  LoadSVGFile(FN);
end;

function TIconManager.IconExists(const Name: string): Boolean;
var
  N: string;
begin
  N := Name.ToLower;
  Result := FIcons.ContainsKey(N) or (FindSVGFile(N) <> '');
end;

class function TIconManager.Instance: TIconManager;
begin
  Result := FIconManager;
end;

function TIconManager.GetIconData(const Name: string): TSVGIconData;
var
  N: string;
begin
  N := Name.ToLower;
  EnsureLoaded(N);

  if not FIcons.TryGetValue(N, Result) then
    raise Exception.CreateFmt('Icon not loaded: %s', [Name]);
end;

function TIconManager.TryGetPathData(const Name: string; out Layer1, Layer2: string): Boolean;
var
  Icon: TSVGIconData;
begin
  Layer1 := '';
  Layer2 := '';
  Result := False;

  if Name.Trim = '' then
    Exit;

  try
    Icon := GetIconData(Name);
  except
    Exit(False);
  end;

  Layer1 := Icon.Layer1;
  Layer2 := Icon.Layer2;
  Result := Layer1 <> '';
end;

function TIconManager.GetBitmap(const Name: string; Size: Integer; Color: TAlphaColor): TBitmap;
var
  L1, L2: string;
  P1, P2: FMX.Objects.TPath;
  Bmp: TBitmap;
  Cvs: TCanvas;
begin
  Result := nil;

  if not TryGetPathData(Name, L1, L2) then
    Exit;

  P1 := nil;
  P2 := nil;
  try
    if L1 <> '' then
      P1 := TSVGUtils.CreatePath(L1);
    if L2 <> '' then
      P2 := TSVGUtils.CreatePath(L2);

    Bmp := TBitmap.Create(Size, Size);
    Cvs := Bmp.Canvas;
    Cvs.BeginScene;
    try
      Bmp.Clear(0);

      if Assigned(P1) then
      begin
        P1.Fill.Kind := TBrushKind.Solid;
        P1.Fill.Color := Color;
        P1.Stroke.Kind := TBrushKind.None;
        P1.PaintTo(Cvs, RectF(0, 0, Size, Size));
      end;

      if Assigned(P2) then
      begin
        P2.Fill.Kind := TBrushKind.Solid;
        P2.Fill.Color := Color;
        P2.Stroke.Kind := TBrushKind.None;
        P2.PaintTo(Cvs, RectF(0, 0, Size, Size));
      end;
    finally
      Cvs.EndScene;
    end;

    Result := Bmp;
  finally
    P1.Free;
    P2.Free;
  end;
end;

end.

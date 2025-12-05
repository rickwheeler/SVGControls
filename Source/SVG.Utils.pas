unit SVG.Utils;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Objects, FMX.Graphics,
  Xml.XMLDoc, Xml.XMLIntf;

type
  TSVGIconData = class
  public
    Name: string;
    Layer1: string;
    Layer2: string;
    ViewBox: TRectF;
    constructor Create(const AName, AL1, AL2: string; const AViewBox: TRectF);
  end;

  TSVGUtils = class
  public
    /// Load a single SVG file and extract up to 2 <path d="..."> layers.
    /// Assumes icons are already normalized (e.g. 0..24 viewBox) by the designer.
    class function LoadSVG(const AFileName: string): TSVGIconData;

    /// Low-level helper used by LoadSVG.
    class procedure ExtractPaths(const Root: IXMLNode; out L1, L2: string);

    /// Optional helper: compute the bounds of the path data (if needed).
    class function ComputeBounds(const PathData: string): TRectF;

    /// Create a TPath from a given path data string (caller owns it).
    class function CreatePath(const PathData: string): FMX.Objects.TPath;
  end;

implementation

{ TSVGIconData }

constructor TSVGIconData.Create(const AName, AL1, AL2: string; const AViewBox: TRectF);
begin
  Name := AName;
  Layer1 := AL1;
  Layer2 := AL2;
  ViewBox := AViewBox;
end;

{ TSVGUtils }

class procedure TSVGUtils.ExtractPaths(const Root: IXMLNode; out L1, L2: string);
var
  i, j: Integer;
  Node, Child: IXMLNode;
begin
  L1 := '';
  L2 := '';

  if Root = nil then
    Exit;

  // Shallow scan: <path> and paths inside <g> groups.
  for i := 0 to Root.ChildNodes.Count - 1 do
  begin
    Node := Root.ChildNodes[i];

    if SameText(Node.NodeName, 'path') then
    begin
      if Node.HasAttribute('d') then
      begin
        if L1 = '' then
          L1 := Node.Attributes['d']
        else if L2 = '' then
          L2 := Node.Attributes['d'];
      end;
    end
    else if SameText(Node.NodeName, 'g') then
    begin
      for j := 0 to Node.ChildNodes.Count - 1 do
      begin
        Child := Node.ChildNodes[j];
        if SameText(Child.NodeName, 'path') and Child.HasAttribute('d') then
        begin
          if L1 = '' then
            L1 := Child.Attributes['d']
          else if L2 = '' then
            L2 := Child.Attributes['d'];
        end;
      end;
    end;
  end;
end;

class function TSVGUtils.LoadSVG(const AFileName: string): TSVGIconData;
var
  Xml: IXMLDocument;
  Root: IXMLNode;
  VBString: string;
  VBParts: TArray<string>;
  VB: TRectF;
  L1, L2: string;
  IconName: string;
begin
  Xml := TXMLDocument.Create(nil);
  Xml.LoadFromFile(AFileName);
  Xml.Active := True;

  Root := Xml.DocumentElement;
  if not Assigned(Root) then
    raise Exception.CreateFmt('Invalid SVG: %s (no root element)', [AFileName]);

  // Default viewBox
  VB := TRectF.Create(0, 0, 24, 24);

  if Root.HasAttribute('viewBox') then
  begin
    VBString := Root.Attributes['viewBox'];
    VBParts := VBString.Split([' ', ','], TStringSplitOptions.ExcludeEmpty);
    if Length(VBParts) >= 4 then
    begin
      // viewBox: minx miny width height
      var
      MinX := StrToFloatDef(VBParts[0], 0);
      var
      MinY := StrToFloatDef(VBParts[1], 0);
      var
      W := StrToFloatDef(VBParts[2], 24);
      var
      H := StrToFloatDef(VBParts[3], 24);
      VB := TRectF.Create(MinX, MinY, MinX + W, MinY + H);
    end;
  end;

  ExtractPaths(Root, L1, L2);

  if L1 = '' then
    raise Exception.CreateFmt('SVG %s contains no <path d="...">', [AFileName]);

  IconName := ChangeFileExt(ExtractFileName(AFileName), '');
  Result := TSVGIconData.Create(IconName, L1, L2, VB);
end;

class function TSVGUtils.CreatePath(const PathData: string): FMX.Objects.TPath;
var
  P: FMX.Objects.TPath;
  PD: TPathData;
begin
  P := FMX.Objects.TPath.Create(nil);
  PD := TPathData.Create;
  try
    PD.Data := PathData;
    P.Data := PD;
    PD := nil; // owned by P
    P.Stored := False;
    P.Locked := True;
    Result := P;
  except
    PD.Free;
    P.Free;
    raise;
  end;
end;

class function TSVGUtils.ComputeBounds(const PathData: string): TRectF;
var
  P: FMX.Objects.TPath;
begin
  if PathData.Trim = '' then
    Exit(TRectF.Empty);

  P := CreatePath(PathData);
  try
    if Assigned(P.Data) then
      Result := P.Data.GetBounds
    else
      Result := P.BoundsRect;
  finally
    P.Free;
  end;
end;

end.

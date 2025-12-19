unit GD.Styles.SvgIcons;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList;

type

  TSvgIcons = class(TDataModule)
    ImageList32: TImageList;
  private
  public
    function AddSVGToImageList(const AIconName: string): Integer;
  end;

var
  SvgIcons: TSvgIcons;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses

  System.IOUtils,

  SVG.IconManager,
  System.UITypes;

{ TSvgIcons }

function TSvgIcons.AddSVGToImageList(const AIconName: string): Integer;
begin
  var Bitmap := IconManager.GetBitmap(AIconName, 32, TAlphaColorRec.Red);

  Bitmap.SaveToFile(TPath.Combine('C:\temp\icons', AIconName + '.bmp'));
  var NewImage := ImageList32.Source.Add;
  NewImage.Name := AIconName;

  var BitmapItem := NewImage.MultiResBitmap.Add;
  BitmapItem.Bitmap.Assign(Bitmap);

  Result := ImageList32.Source.IndexOf(AIconName);
end;

end.

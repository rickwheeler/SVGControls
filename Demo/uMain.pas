unit uMain;

interface

uses
  SVG.SVGButton,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation;

type

  TForm2 = class(TForm)
    PathLabel1: TPathLabel;
    SVGButton1: TSVGButton;
    SVGSpeedButton1: TSVGSpeedButton;
    procedure FormCreate(Sender: TObject);
  private
    procedure CreateDemoControls;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  SVG.IconManager,
  System.IOUtils;

{$R *.fmx}

procedure TForm2.CreateDemoControls;
begin
  var BtnSave := TSVGButton.Create(Self);
  BtnSave.Parent := Self;
  BtnSave.Position.Point := PointF(20, 20);
  BtnSave.Width := 120;
  BtnSave.Height := 40;
  BtnSave.Text := 'Save';
  BtnSave.CanFocus := False;
  BtnSave.IconSize := 32;
  BtnSave.IconName := 'save';   // expects icons\save.svg
  BtnSave.IconPadding.Left := 4;
  BtnSave.IconAlignment := iaLeft;

  var BtnTrash := TSVGSpeedButton.Create(Self);
  BtnTrash.Parent := Self;
  BtnTrash.Position.Point := PointF(200, 20);
  BtnTrash.Width := 40;
  BtnTrash.Height := 40;
  BtnTrash.IconSize := 20;
  BtnTrash.IconName := 'trash-duotone'; // expects icons\trash.svg
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  // point IconManager to ./icons under the EXE
  var IconPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'icons');
  if DirectoryExists(IconPath) then
    IconManager.AddSearchPath(IconPath);

  CreateDemoControls;
end;

end.

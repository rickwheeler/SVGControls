program SVGControlsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form2},
  SVG.IconManager in '..\Source\SVG.IconManager.pas',
  SVG.Utils in '..\Source\SVG.Utils.pas',
  SVG.IconTheme in '..\Source\SVG.IconTheme.pas',
  SVG.SVGButton in '..\Source\SVG.SVGButton.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

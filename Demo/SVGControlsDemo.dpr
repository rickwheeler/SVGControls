program SVGControlsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  GD.Styles.StyleBooks in 'GD.Styles.StyleBooks.pas' {StyleBooks: TDataModule},
  GD.Styles.SvgIcons in 'GD.Styles.SvgIcons.pas' {SvgIcons: TDataModule},
  SVG.IconManager in '..\Source\SVG.IconManager.pas',
  SVG.IconTheme in '..\Source\SVG.IconTheme.pas',
  SVG.SVGButton in '..\Source\SVG.SVGButton.pas',
  SVG.Utils in '..\Source\SVG.Utils.pas',
  uMain in 'uMain.pas' {Form2},
  uViewAllForm in 'uViewAllForm.pas' {ViewAllForm},
  SVG.SVGPopupMenu in '..\Source\SVG.SVGPopupMenu.pas',
  System.IOUtils,
  System.SysUtils;

{$R *.res}

begin
  Application.Initialize;

  IconManager.AddSearchPath(TPath.Combine(ExtractFilePath(ParamStr(0)), 'icons'));

  Application.CreateForm(TStyleBooks, StyleBooksDataModule);
  Application.CreateForm(TSvgIcons, SvgIcons);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TViewAllForm, ViewAllForm);
  Application.Run;
end.

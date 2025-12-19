unit uViewAllForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts;

type
  TViewAllForm = class(TForm)
    ButtonsLayout: TFlowLayout;
    ToolbarLayout: TFlowLayout;
    LoadButtonsButton: TButton;
    LoadSpeedButtonsSpeedButton: TSpeedButton;
    procedure LoadButtonsButtonClick(Sender: TObject);
    procedure LoadSpeedButtonsSpeedButtonClick(Sender: TObject);
  private
  public
  end;

var
  ViewAllForm: TViewAllForm;

implementation

{$R *.fmx}

uses
  SVG.SVGButton,
  SVG.IconManager,
  System.IOUtils;


procedure TViewAllForm.LoadButtonsButtonClick(Sender: TObject);
  procedure AddDirectory(const ADirectory: string);
  begin
    var Files := TDirectory.GetFiles(ADirectory);
    for var FilePath in Files do
    begin
      var FileName := TPath.GetFileNameWithoutExtension(FilePath);
      var NewButton := TSVGButton.Create(Self);
      NewButton.Parent := ButtonsLayout;
      NewButton.Height := 80;
      NewButton.Width := 200;
      NewButton.IconAlignment := TIconAlignment.iaLeft;
      NewButton.IconName := FileName;
      NewButton.IconSize := 64;
      NewButton.IconPadding.Left := 8;
      NewButton.Text := FileName;
    end;

    var Directories := TDirectory.GetDirectories(ADirectory);
    if Length(Directories) > 0 then
      for var Directory in Directories do
        AddDirectory(Directory)
  end;
begin
  ButtonsLayout.BeginUpdate;
  ButtonsLayout.DeleteChildren;
  AddDirectory(TPath.Combine(ExtractFilePath(ParamStr(0)), 'icons'));
  ButtonsLayout.EndUpdate;
end;

procedure TViewAllForm.LoadSpeedButtonsSpeedButtonClick(Sender: TObject);
  procedure AddDirectory(const ADirectory: string);
  begin
    var Files := TDirectory.GetFiles(ADirectory);
    for var FilePath in Files do
    begin
      var FileName := TPath.GetFileNameWithoutExtension(FilePath);
      var NewButton := TSVGSpeedButton.Create(Self);
      NewButton.Parent := ButtonsLayout;
      NewButton.Height := 80;
      NewButton.Width := 80;
      NewButton.IconAlignment := TIconAlignment.iaCenter;
      NewButton.IconName := FileName;
      NewButton.IconSize := 64;
      NewButton.IconPadding.Left := 8;
      //NewButton.Text := FileName;
    end;

    var Directories := TDirectory.GetDirectories(ADirectory);
    if Length(Directories) > 0 then
      for var Directory in Directories do
        AddDirectory(Directory)
  end;
begin
  ButtonsLayout.BeginUpdate;
  ButtonsLayout.DeleteChildren;
  AddDirectory(TPath.Combine(ExtractFilePath(ParamStr(0)), 'icons'));
  ButtonsLayout.EndUpdate;
end;

end.

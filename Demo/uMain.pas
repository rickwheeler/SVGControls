unit uMain;

interface

uses
  SVG.SVGButton,
  SVG.SVGPopupMenu,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Menus, System.ImageList,
  FMX.ImgList;

type

  TForm2 = class(TForm)
    ViewAllIconsButton: TSVGButton;
    DarkButton: TSVGSpeedButton;
    LightButton: TSVGSpeedButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    Label1: TLabel;
    PopupMenu1: TPopupMenu;
    SVGSpeedButton1: TSVGSpeedButton;
    Button1: TButton;
    Button2: TButton;
    procedure ViewAllIconsButtonClick(Sender: TObject);
    procedure DarkButtonClick(Sender: TObject);
    procedure LightButtonClick(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Button1Click(Sender: TObject);
  private
    FBtnSave: TSVGButton;
    FBtnTrash: TSVGSpeedButton;
    FPopupMenu: TSVGPopupMenu;
    procedure CreateDemoControls;
    procedure CreatePopupMenu;
  public
    constructor Create(AOwner: TComponent); override;

  end;

var
  Form2: TForm2;

implementation

uses
  uViewAllForm,
  GD.Styles.StyleBooks,
  GD.Styles.SvgIcons,
  SVG.IconManager,
  FMX.Styles,
  System.IOUtils;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  FBtnTrash.Enabled := not FBtnTrash.Enabled;
end;

constructor TForm2.Create(AOwner: TComponent);
begin
  inherited;

  CreateDemoControls;
  CreatePopupMenu;
end;

procedure TForm2.CreateDemoControls;
begin
  FBtnSave := TSVGButton.Create(Self);
  FBtnSave.Parent := Self;
  FBtnSave.Position.Point := PointF(20, 20);
  FBtnSave.Width := 120;
  FBtnSave.Height := 40;
  FBtnSave.Text := 'Save';
  FBtnSave.CanFocus := False;
  FBtnSave.IconSize := 32;
  FBtnSave.IconName := 'panel';   // expects icons\save.svg
  FBtnSave.IconPadding.Left := 4;
  FBtnSave.IconAlignment := iaRight;
  FBtnSave.SetFontColor(TAlphaColorRec.Green);

  FBtnTrash := TSVGSpeedButton.Create(Self);
  FBtnTrash.Parent := Self;
  FBtnTrash.Position.Point := PointF(200, 20);
  FBtnTrash.Width := 100;
  FBtnTrash.Height := 40;
  FBtnTrash.IconSize := 20;
  FBtnTrash.IconName := 'trash'; // expects icons\trash.svg
  FBtnTrash.Text := 'trash';
//  FBtnTrash.IconColor(TAlphaColorRec.Darkgreen);
end;

procedure TForm2.CreatePopupMenu;
//var
//  II: Integer;

  procedure CreateMenuItem(const AIconName: string);
  begin
      var MenuItem := TMenuItem.Create(Self);
      MenuItem.Text := AIconName;
      MenuItem.Bitmap := IconManager.GetBitmap(AIconName, 32, TAlphaColorRec.Red);
//      SvgIcons.AddSVGToImageList(AIconName);
//      MenuItem.ImageIndex := II;
//      Inc(II);
      PopupMenu1.AddObject(MenuItem);
  end;

begin
//  SvgIcons.ImageList32.Source.Clear;
//
//
//  CreateMenuItem('panel');
//  CreateMenuItem('door-left-180');

  FPopupMenu := TSVGPopupMenu.Create(Self);

end;

procedure TForm2.DarkButtonClick(Sender: TObject);
begin
  TStyleManager.SetStyle(StyleBooksDataModule.DarkStyleBook.Style.Clone(StyleBooksDataModule));
end;

procedure TForm2.LightButtonClick(Sender: TObject);
begin
  TStyleManager.SetStyle(StyleBooksDataModule.LightStyleBook.Style.Clone(StyleBooksDataModule));
end;

procedure TForm2.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
  begin
    FPopupMenu.Clear;

    FPopupMenu.AddCommand(
      'Add Panel',
      'panel',
      nil
    );

    FPopupMenu.AddCommand(
      'Left Door',
      'door-left-180',
      nil
    );

    FPopupMenu.Popup(TPointF.Create(X, Y));
  end;
end;

procedure TForm2.ViewAllIconsButtonClick(Sender: TObject);
begin
  ViewAllForm.Show;
end;

end.

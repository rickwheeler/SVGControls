unit GD.Styles.StyleBooks;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls;

type
  TStyleBooks = class(TDataModule)
    LightStyleBook: TStyleBook;
    DarkStyleBook: TStyleBook;
  private
  public
  end;

var
  StyleBooksDataModule: TStyleBooks;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TStyleBooksDataModule }

end.

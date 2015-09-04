unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Menus, Dialogs,
  SynEdit, SynEditHighlighter, LCLType, Lazlogger,
  SynFacilCompletion;

type

  { TForm1 }
  TForm1 = class(TForm)
    ed1: TSynEdit;
    procedure ed1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ed1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
//crea espacio para almacenar token
var hlt1 : TSynFacilComplet;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //configure highlighters
  hlt1 := TSynFacilComplet.Create(self);  //my highlighter
  hlt1.LoadFromFile('ObjectPascal.xml');
  hlt1.SelectEditor(ed1);

  ed1.OnUTF8KeyPress:=@ed1UTF8KeyPress;
  ed1.Lines.LoadFromFile('ObjectPascal_sample.txt');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //Prepare the completion
//  InicAyudaContext(ed1,Self,'');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  hlt1.UnSelectEditor;
  hlt1.Free;
end;

procedure TForm1.ed1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt1.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.ed1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt1.UTF8KeyPress(Sender, UTF8Key);
end;

end.


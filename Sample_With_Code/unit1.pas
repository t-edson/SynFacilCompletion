unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Menus, Dialogs,
  SynEdit, SynEditHighlighter, LCLType, Lazlogger,
  SynFacilBasic, SynFacilCompletion;

type

  { TForm1 }

  TForm1 = class(TForm)
    ed1: TSynEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnSyntax: TMenuItem;
    OpenDialog1: TOpenDialog;
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
var hlt : TSynFacilComplet;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  opEve: TFaOpenEvent;
begin
  //configure highlighters
  hlt := TSynFacilComplet.Create(self);  //my highlighter
  //define syntax
  hlt.ClearSpecials;               //para empezar a definir tokens
  hlt.CreateAttributes;            //Limpia atributos
  hlt.ClearMethodTables;           //limpìa tabla de métodos
  hlt.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  hlt.DefTokContent('[0-9]', '[0..9xa-fXA-F]', hlt.tkNumber);
  hlt.AddIdentSpecList('begin end var const type', hlt.tkKeyword);
  hlt.AddIdentSpecList('case class if else exit unit', hlt.tkKeyword);
  hlt.AddIdentSpecList('for function procedure property', hlt.tkKeyword);
  hlt.DefTokDelim('''','''', hlt.tkString);
  hlt.DefTokDelim('//','', hlt.tkComment);
  hlt.DefTokDelim('{','}', hlt.tkComment, tdMulLin);
  hlt.Rebuild;  //reconstruye
  //define completion
  hlt.CompletionOn:=true;
  hlt.OpenOnKeyUp:=true;
  opEve := hlt.AddOpenEvent('Identifier', '', fil_LastTokPart);
  opEve.AddItems('begin end var const type', -1);
  opEve.AddItems('case class if else exit unit', -1);
  opEve.AddItems('for function procedure property', -1);

  hlt.SelectEditor(ed1);
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
  hlt.UnSelectEditor;
  hlt.Free;
end;

procedure TForm1.ed1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.ed1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt.UTF8KeyPress(Sender, UTF8Key);
end;

end.


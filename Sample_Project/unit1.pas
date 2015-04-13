unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Menus, Dialogs,
  SynEdit, SynEditHighlighter, LCLType, Lazlogger,
  SynFacilCompletion;

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
    procedure itClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
//crea espacio para almacenar token
var hlt1 : TSynFacilComplet;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  it: TMenuItem;
  fResult: LongInt;
  fFile: TSearchRec;
begin
  //configure highlighters
  hlt1 := TSynFacilComplet.Create(self);  //my highlighter
  hlt1.LoadFromFile('../languages/ObjectPascal.xml');
  hlt1.SelectEditor(ed1);

  ed1.OnUTF8KeyPress:=@ed1UTF8KeyPress;
  ed1.Lines.LoadFromFile('../languages/ObjectPascal_sample.txt');

  //load the syntax files
  fResult :=  FindFirst('../languages/*.xml', faAnyFile, fFile );
  while fResult = 0 do begin
    it := TMenuItem.Create(self);
    it.Caption:=fFile.Name;
    it.OnClick:=@itClick;
    mnSyntax.Add(it);
    fResult := FindNext(fFile);
  end;
  FindClose(fFile);
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

procedure TForm1.itClick(Sender: TObject);
var
  i: Integer;
begin
  hlt1.LoadFromFile('../languages/'+TMenuItem(Sender).Caption);
  ed1.Invalidate;
  //check the selected item
  for i:=0 to mnSyntax.Count-1 do mnSyntax.Items[i].Checked:=false;
  TMenuItem(Sender).Checked:=true;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  xml: String;
  i: Integer;
  it: TMenuItem;
begin
  OpenDialog1.Filter:='Samples|*.txt';
  OpenDialog1.InitialDir:='../languages';
  if not OpenDialog1.Execute then exit;    //canceled
  ed1.Lines.LoadFromFile(OpenDialog1.FileName);  //load
  self.Caption:=OpenDialog1.FileName;
  xml := hlt1.LoadSyntaxFromPath(OpenDialog1.FileName,'../languages/'); //select the XML
  //check the language menu
  for i:=0 to mnSyntax.Count-1 do begin
    it := mnSyntax.Items[i];
    it.Checked := Copy(Upcase(it.Caption+'.xml'),2,1000) = UpCase(xml);
  end;
end;

end.


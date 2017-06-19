unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Menus, Dialogs, SynEdit,
  SynEditHighlighter, SynHighlighterXML, LCLType, ExtCtrls, StdCtrls, Lazlogger,
  SynFacilCompletion;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GrabSint: TButton;
    ed: TSynEdit;
    edXML: TSynEdit;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Splitter1: TSplitter;
    SynXMLSyn1: TSynXMLSyn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GrabSintClick(Sender: TObject);
    procedure edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  hlt1.SelectEditor(ed);
  hlt1.IconList := ImageList1;
  ed.Lines.LoadFromFile('test.txt');
  ed.Options:=ed.Options-[eoTrimTrailingSpaces];
  edXML.Lines.LoadFromFile('test.xml');
  hlt1.LoadFromFile('test.xml');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //Prepare the completion
//  InicAyudaContext(ed,Self,'');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  hlt1.UnSelectEditor;
  hlt1.Free;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
  VK_TAB: begin
      if Shift = [ssCtrl] then begin
        if ed.Focused then begin
          edXML.SetFocus;
          Key := 0;
          exit;
        end;
        if edXML.Focused then begin
          ed.SetFocus;
          Key := 0;
          exit;
        end;
      end;
  end;
  end;
end;

procedure TForm1.edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt1.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.GrabSintClick(Sender: TObject);
begin
  edXML.Lines.SaveToFile('test.xml');
  hlt1.LoadFromFile('test.xml');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ed.SetFocus;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  edXML.SetFocus;
end;

procedure TForm1.edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt1.UTF8KeyPress(Sender, UTF8Key);
end;

end.


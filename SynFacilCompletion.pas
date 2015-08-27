{
SynFacilCompletion 1.1b
=======================
Por Tito Hinostroza 19/12/2014
* Se crean tipos nuevos para soportar patrones de apertura del completado.
* Se agrega soporte para la etiqueta <OpenOn ... >
* Desaparece la propiedad "before" y "posit" de TFaCompletItem, porque se está delegando
esta verificación a TFaOpenPattern.
* Se elimina el parámetro "AfterIdentif" de la sinatxis del XML.
* Se elimina el tipo TPosicCursor, y el código asociado.
* Se cambia completamente el código de OnExecute(), porque ha cambiado el método de
verificación para la apertura de la ventana de completado.
* Se cambia el nombre de algunos métodos.

Descripción
============
Unidad que expande al resaltador TSynFacilSyn, para que pueda soportar configuraciones
de autocompletado de texto.

Se usa de forma similar a SynFacilSyn. Se debe crear un resaltador, pero ahora de la
clase TSynFacilComplet:

uses ... , SynFacilCompletion;

procedure TForm1.FormShow(Sender: TObject);
begin
 //configure highlighter
  hlt := TSynFacilComplet.Create(self);  //my highlighter
  SynEdit1.Highlighter := hlt;  //optional if we are going to use SelectEditor()
  hlt.LoadFromFile('./languages/ObjectPascal.xml');  //load syntax
  hlt.SelectEditor(SynEdit1);  //assign to editor
end;

Luego se debe interceptar los evento KeyUp y UTF8KeyPress, del SynEdit:

procedure TForm1.SynEdit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.SynEdit1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt.UTF8KeyPress(Sender, UTF8Key);
end;

Y se debe terminar correctamente:

procedure TForm1.FormDestroy(Sender: TObject);
begin
  hlt.UnSelectEditor;   //release editor (only necessary if we are to call to SelectEditor(), again)
  hlt.Free;  //destroy the highlighter
end;

Cuando se desea desaparecer la ventana de ayuda contextual por algún evento, se debe
llamar a CloseCompletionWindow().

}
unit SynFacilCompletion;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Dialogs, XMLRead, DOM, LCLType, Graphics,
  SynEdit, SynEditHighlighter, SynEditTypes, SynEditKeyCmds, Lazlogger,
  SynFacilHighlighter, SynFacilBasic, SynCompletion;

type

  TFaCompletItem = record
    item   : string;       //etiqueta a mostrar en el menú
    content: string;       //contenido a reemplazar
    tokTyp : TSynHighlighterAttributes;  //token donde es válida la palabra
    block  : TFaSynBlock;  //bloque donde es válida la palabra
  end;
  TFaCompletItems = array of TFaCompletItem;


  //Tipo de Elemento del patrón
  TFaTPatternElementKind = (
    pak_none,      //tipo no definido
    pak_String,    //es literal cadena
    pak_Identif,   //es token identificador (tkKeyword, tkIndetifier, ...)
    pak_NoIdentif, //no es token identificador
    pak_TokTyp,    //es un tipo específico de token
    pak_NoTokTyp   //no es un tipo específico de token
  );
  //Elemento del patrón
  TFaPatternElement = record
    patKind: TFaTPatternElementKind;
    str    : string;          //valor, cuando es del tipo pak_String
    toktyp : TSynHighlighterAttributes;  //valor cuando es de tipo pak_TokTyp o pak_NoTokTyp
  end;

  //Patrón de coincidencia para abrir la lista de completado
  {Los índices representan posiciones realticas de tokens
    [0]  -> Token que está justo después del cursor (token actual)
    [-1] -> Token que está antes del token actual
    [-2] -> Token que está antes del token [-1]
    [1]  -> Token que está después del token actual)
  }
  TFaOpenPattern = record
    elem : array[-3..0] of TFaPatternElement;
    nBef : integer;   //número de elementos válidos haste el ítem 0 (puede ser 0,1,2 o 3)
    nAft : integer;   //número de elementos válidos depués del ítem 0 (puede ser 0 o 1)
    Items: array of TFaCompletItem; //lista de las palabras disponibles para el completado
  end;

type
  //clase personalziada para el completado
  { TSynCompletionF }
  TSynCompletionF = class(TSynCompletion)
    function OnSynCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X,
      Y: integer; IsSelected: boolean; Index: integer): boolean;

  private
    function PaintCompletionItem(const AKey: string; ACanvas: TCanvas; X, Y,
      MaxX: integer; ItemSelected: boolean; Index: integer;
      aCompletion: TSynCompletion): TPoint;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynFacilComplet }
  //clase principal
  TSynFacilComplet = class(TSynFacilSyn)
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure OnExecute(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  protected
    ed        : TSynEdit;        //referencia interna al editor
    MenuComplet: TSynCompletionF;//menú contextual
    AllItems  : TFaCompletItems; //lista de todas las palabras disponibles para el completado
    AvailItems: TStringList;  {Lista de palabras disponible para cargar en el menú de
                                auto-completado}
    tok0      : TFaTokInfo;      //token actual del completado
    Pos0      : TPoint;     //Posición incial del cursor donde se abrió la ventana de completado

    IdentAct0 : string;          //IDentificador actual desde el inicio hasta el cursor
    IdentAct  : string;          //Identificador actual completo
    IdentAnt  : string;          //Identificador anterior
    BloqueAct : TFaSynBlock;     //referecnia al bloque actual

    tokens    : TATokInfo;       //lista de tokens actuales
    iCurTok   : integer;         //índice al token actual
    inMidTok  : boolean;         //indica si el cursor está en medio de un token

    utKey         : TUTF8Char;     //tecla pulsada
    vKey          : word;          //código de tecla virtual
    vShift        : TShiftState;   //estado de shift
    SpecIdentifiers: TArrayTokSpec;
    SearchOnKeyUp : boolean;       //bandera de control
    function CheckForClose: boolean;
    procedure FillCompletMenuFilteredBy(str: string);
    procedure OpenCompletionWindow;
    procedure ProcCompletionLabel(nodo: TDOMNode);
    procedure ReadSpecialIdentif;
    procedure LookAroundCursor; virtual;
  private  //manejo de patrones de apertura
    OpenPatterns: array of TFaOpenPattern;  //lista de patrones de apertura
    procedure ClearOpenPatterns;
    function AddOpenPattern(AfterPattern, BeforePattern: string): integer;
    procedure AddToOpenPatternsL(np: integer; list: string; blk: TFaSynBlock);
  public
    CompletionOn: boolean;  //activa o desactiva el auto-completado
    SelectOnEnter: boolean;   //habilita la selección con enter
    CaseSensComp: boolean;     //Uso de caja, en autocompletado
    OpenOnKeyUp: boolean;   //habilita que se abra automáticamente al soltar una tecla
    procedure LoadFromFile(Arc: string); override;
    function LoadSyntaxFromPath(SourceFile: string; path: string;
      CaseSens: boolean=false): string;
    procedure SelectEditor(ed0: TSynEdit);  //inicia la ayuda contextual
    procedure UnSelectEditor;  //termina la ayuda contextual con el editor
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddCompItem(item, content: string; blk: TFaSynBlock);
    procedure AddCompItemL(list: string; blk: TFaSynBlock);
    procedure CloseCompletionWindow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  //utilidades
  function ReadExtenFromXML(XMLfile: string): string;
  function XMLFileHaveExten(XMLfile: string; exten: string;
                CaseSens: boolean = false): boolean;

implementation
uses SynEditMiscProcs;

const
//  ERR_ATTRIB_NO_EXIST = 'Atributo %s no existe. (etiqueta <COMPLETION ...>)';
//  ERR_INVAL_LAB_COMP = 'Etiqueta %s no válida para etiqueta <COMPLETION ...>';
//  ERROR_LOADING_ = 'Error loading: ';

  ERR_ATTRIB_NO_EXIST = 'Attribute %s doesn''t exist. (label <COMPLETION ...>)';
  ERR_INVAL_LAB_COMP = 'Invalid label %s for  <COMPLETION ...>';
  ERR_INVAL_BLK_NAME = 'Invalid block name.';
  ERROR_LOADING_ = 'Error loading: ';
  ERR_PAR_BEF_PATT = 'Error in parameter "BeforePattern"';
  ERR_PAR_AFT_PATT = 'Error in parameter "AfterPattern"';

function ReadExtenFromXML(XMLfile: string): string;
//Lee las extensiones que tiene definidas un archivo de sintaxis.
var doc: TXMLDocument;
  atri: TDOMNode;
  i: Integer;
begin
  try
    Result := '';   //por defecto
    ReadXMLFile(doc, XMLfile);  //carga archivo
    //busca el parámetro "ext"
    for i:= 0 to doc.DocumentElement.Attributes.Length-1 do begin
      atri := doc.DocumentElement.Attributes.Item[i];
      if UpCase(atri.NodeName) = 'EXT' then begin
        Result := trim(atri.NodeValue);  //valor sin espacios
      end;
    end;
    doc.Free;  //libera
  except
    on E: Exception do begin
      ShowMessage(ERROR_LOADING_ + XMLfile + #13#10 + e.Message);
      doc.Free;
    end;
  end;
end;
function XMLFileHaveExten(XMLfile: string; exten: string;
                CaseSens: boolean = false): boolean;
//Indica si un archivo XML de sintaxis, tiene definida la extensión que se indica
//La comparación se puede hacer con o sin diferecnia de caja
var
  lext: TStringList;
  s: String;
  tmp: String;
begin
  Result := false;  //por defecto
  lext:= TStringList.Create;  //crea lista
  tmp := ReadExtenFromXML(XMLfile);  //lee archivo
  lext.DelimitedText:=' ';
  lext.Text:=tmp;  //divide
  //busca de acuerdo a la caja
  if CaseSens then begin
    for s in lext do begin
      if s = exten then begin
        //encontró
        Result := true;
        lext.Free;
        exit;
      end;
    end;
  end else begin
    for s in lext do begin
      if Upcase(s) = Upcase(exten) then begin
        //encontró
        Result := true;
        lext.Free;
        exit;
      end;
    end;
  end;
  //No enecontró
  lext.Free;
end;

{ TSynCompletionF }
function TSynCompletionF.OnSynCompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; IsSelected: boolean; Index: integer): boolean;
var
  MaxX: Integer;
  hl: TSynCustomHighlighter;
begin
  //configura propiedades de texto
//  if (Editor<>nil) then begin
//    ACanvas.Font := Editor.Font
//  end;
  ACanvas.Font.Style:=[];
{  if not IsSelected then
    ACanvas.Font.Color := FActiveEditDefaultFGColor
  else
    ACanvas.Font.Color := FActiveEditSelectedFGColor;}
  MaxX:=TheForm.ClientWidth;
  hl := nil;
  if Editor <> nil then
    hl := Editor.Highlighter;
  PaintCompletionItem(AKey, ACanvas, X, Y, MaxX, IsSelected, Index, self);

  Result := false;
//  Result:=true;  //para indicar que lo intercepta
end;
function TSynCompletionF.PaintCompletionItem(const AKey: string;
  ACanvas: TCanvas; X, Y, MaxX: integer; ItemSelected: boolean; Index: integer;
  aCompletion: TSynCompletion): TPoint;

var
  BGRed: Integer;
  BGGreen: Integer;
  BGBlue: Integer;
  TokenStart: Integer;
  BackgroundColor: TColorRef;
  ForegroundColor: TColorRef;

  procedure SetFontColor(NewColor: TColor);
  var
    FGRed: Integer;
    FGGreen: Integer;
    FGBlue: Integer;
    RedDiff: integer;
    GreenDiff: integer;
    BlueDiff: integer;
  begin
    NewColor := TColor(ColorToRGB(NewColor));
    FGRed:=(NewColor shr 16) and $ff;
    FGGreen:=(NewColor shr 8) and $ff;
    FGBlue:=NewColor and $ff;
    RedDiff:=Abs(FGRed-BGRed);
    GreenDiff:=Abs(FGGreen-BGGreen);
    BlueDiff:=Abs(FGBlue -BGBlue);
    if RedDiff*RedDiff + GreenDiff*GreenDiff + BlueDiff*BlueDiff<30000 then
    begin
      NewColor:=InvertColor(NewColor);
      {IncreaseDiff(FGRed,BGRed);
      IncreaseDiff(FGGreen,BGGreen);
      IncreaseDiff(FGBlue,BGBlue);
      NewColor:=(FGRed shl 16) or (FGGreen shl 8) or FGBlue;}
    end;
    ACanvas.Font.Color:=NewColor;
  end;

  procedure WriteToken(var TokenStart, TokenEnd: integer);
  var
    CurToken: String;
  begin
    if TokenStart>=1 then begin
      CurToken:=copy(AKey,TokenStart,TokenEnd-TokenStart);
      ACanvas.TextOut(x+1, y, CurToken);
      x := x + ACanvas.TextWidth(CurToken);
      //debugln('Paint A Text="',CurToken,'" x=',dbgs(x),' y=',dbgs(y),' "',ACanvas.Font.Name,'" ',dbgs(ACanvas.Font.Height),' ',dbgs(ACanvas.TextWidth(CurToken)));
      TokenStart:=0;
    end;
  end;

var
  s: string;
//  IdentItem: TIdentifierListItem;
  AColor: TColor;
  IsReadOnly: boolean;
  ImageIndex: longint;
begin
  ForegroundColor := ColorToRGB(ACanvas.Font.Color);
  Result.X := 0;
  Result.Y := ACanvas.TextHeight('W');


  // draw
    BackgroundColor:=ColorToRGB(ACanvas.Brush.Color);
    BGRed:=(BackgroundColor shr 16) and $ff;
    BGGreen:=(BackgroundColor shr 8) and $ff;
    BGBlue:=BackgroundColor and $ff;
    ImageIndex:=-1;


        AColor:=clBlack;
        s:='keyword';


    SetFontColor(AColor);
    ACanvas.TextOut(x+1,y,s);
    inc(x,ACanvas.TextWidth('constructor '));
    if x>MaxX then exit;

    // paint the identifier
    SetFontColor(ForegroundColor);
    ACanvas.Font.Style:=ACanvas.Font.Style+[fsBold];
    s:='identificador';
    //DebugLn(['PaintCompletionItem ',x,',',y,' ',s]);
    ACanvas.TextOut(x+1,y,s);
    inc(x,ACanvas.TextWidth(s));
    if x>MaxX then exit;
    ACanvas.Font.Style:=ACanvas.Font.Style-[fsBold];

    ImageIndex := -1;

    // paint icon
    if ImageIndex>=0 then begin
      //IDEImages.Images_16, es de tipo TCustomImageList
//      IDEImages.Images_16.Draw(ACanvas,x+1,y+(Result.Y-16) div 2,ImageIndex);
      inc(x,18);
      if x>MaxX then exit;
    end;

end;
constructor TSynCompletionF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //intercepta este evento
//  OnPaintItem:=@OnSynCompletionPaintItem;
end;

{ TSynFacilComplet }
procedure TSynFacilComplet.ReadSpecialIdentif;
//Hace una exploración para leer todos los identificadores especiales en la tabla
//SpecIdentifiers[].
var
  met: TFaProcMetTable;
  p: TPtrATokEspec;
  c: Char;
  i: Integer;
  n: Integer;
begin
  setlength(SpecIdentifiers,0);
  if CaseSensitive then begin
    for c in ['A'..'Z','a'..'z'] do begin
      TableIdent(c, p, met);
      if p<> nil then begin
        for i:= 0 to high(p^) do begin
          n := high(SpecIdentifiers)+1;  //tamaño de matriz
          setlength(SpecIdentifiers,n+1);
          SpecIdentifiers[n] := p^[i];
        end;
      end;
    end;
  end else begin  //no es sensible a la caja
    for c in ['A'..'Z'] do begin
      TableIdent(c, p, met);
      if p<> nil then begin
        for i:= 0 to high(p^) do begin
          n := high(SpecIdentifiers)+1;  //tamaño de matriz
          setlength(SpecIdentifiers,n+1);
          SpecIdentifiers[n] := p^[i];
        end;
      end;
    end;
  end;
end;
function TSynFacilComplet.LoadSyntaxFromPath(SourceFile: string; path: string;
              CaseSens: boolean = false): string;
//Carga un archivo de sintaxis, buscando el resaltador apropiado en un directorio.
//Si encuentra el archivo de sintaxis apropiado, devuelve el nombre del archivo usado
//(sin incluir la ruta), de otra forma, devuelve una cadena vacía.
var
  ext: String;
  Hay: Boolean;
  SR : TSearchRec;
  rut: String;
begin
  Result := '';
  ext := ExtractFileExt(SourceFile);
  if ext<>'' then ext := copy(ext, 2, 10);  //quita el punto
  //explora los lenguajes para encontrar alguno que soporte la extensión dada
  Hay := FindFirst(path + '\*.xml',faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo, lee sus extensiones
     rut := path + '\' + SR.name;
     if XMLFileHaveExten(rut, ext, CaseSens) then  begin //encontró
       LoadFromFile(rut);  //carga sintaxis
       Result := SR.name;
       exit;
     end;
     //no encontró extensión, busca siguiente archivo
     Hay := FindNext(SR) = 0;
  end;
  //no encontró
end;
procedure TSynFacilComplet.AddCompItem(item, content: string; blk: TFaSynBlock);
//Agrega un ítem a la lista de completado. "blk" es el bloque donde será válida el ítem para
//el completado. Si es NIL, será válido en cualquier bloque.
var
  n: Integer;
  r: TFaCompletItem;
begin
  r.item:=item;  //etiqueta a mostrar
  r.content:=content;
  r.block := blk;;
  //agrega
  n := high(AllItems)+1;  //tamaño de matriz
  setlength(AllItems,n+1);
  AllItems[n] := r;
end;
procedure TSynFacilComplet.AddCompItemL(list: string; blk: TFaSynBlock);
//Agrega una lista de ítems, separados por espacios, a la lista de completado
var
  n: Integer;
  r: TFaCompletItem;
  lst: TStringList;
  i: Integer;
begin
  //divide
  lst := TStringList.Create;
  lst.Delimiter := ' ';
  lst.DelimitedText := list;

  //agrega
  n := high(AllItems)+1;  //tamaño de matriz
  setlength(AllItems,n+lst.Count);
  for i:= 0 to lst.Count-1 do begin
    r.item:=lst[i];
    r.content:=r.item;
    r.block := blk;
    AllItems[n+i] := r;
  end;
  lst.Destroy;
end;
procedure TSynFacilComplet.ProcCompletionLabel(nodo: TDOMNode);
//Procesa la etiqueta <Completion>, que es el bloque que define todo el sistema de
//completado de código.
var
  listIden: string;
  tCasSen: TFaXMLatrib;
  i,j   : Integer;
  nodo2: TDOMNode;
  tListAttr: TFaXMLatrib;
  tipTok: TSynHighlighterAttributes;
  hayList, hayOpen: Boolean;
  tOpenKUp: TFaXMLatrib;
  tSelOEnt: TFaXMLatrib;
  tBlock: TFaXMLatrib;
  blk : TFaSynBlock;
  success: boolean;
  tBefPatt, tAftPatt: TFaXMLatrib;
  np: Integer;
begin
  //carga los parámetros
  tCasSen :=ReadXMLParam(nodo, 'CaseSensitive');
  tOpenKUp:=ReadXMLParam(nodo, 'OpenOnKeyUp');
  tSelOEnt:=ReadXMLParam(nodo, 'SelectOnEnter');
  //carga atributos leidos
  if tCasSen.hay then  //si se especifica
    CaseSensComp := tCasSen.bol  //se lee
  else  //si no
    CaseSensComp := CaseSensitive;  //toma el del resaltador
  if tOpenKUp.hay then OpenOnKeyUp:=tOpenKUp.bol;
  if tSelOEnt.hay then SelectOnEnter:=tSelOEnt.bol;
  hayList := false;  //inicia bandera
  hayOpen := false;  //inicia bandera
  ////////// explora nodos hijos //////////
  for i := 0 to nodo.ChildNodes.Count-1 do begin
    nodo2 := nodo.ChildNodes[i];
    if UpCAse(nodo2.NodeName)='INCLUDE' then begin  //incluye lista de palabras por atributo
      //lee parámetros
      tListAttr := ReadXMLParam(nodo2,'Attribute');
      CheckXMLParams(nodo2, 'Attribute');  //puede generar excepción
      if tListAttr.hay then begin
        //se pide agregar la lista de identificadores de un atributo en especial
        if IsAttributeName(tListAttr.val)  then begin
          tipTok := GetAttribByName(tListAttr.val);   //tipo de atributo
          //busca los identificadores para agregarlos
          for j:= 0 to high(SpecIdentifiers) do begin
            if SpecIdentifiers[j].tTok = tipTok then begin
              AddCompItem(SpecIdentifiers[j].orig,SpecIdentifiers[j].orig, nil);
            end;
          end;
        end else begin  //atributo no existe
          raise ESynFacilSyn.Create(Format(ERR_ATTRIB_NO_EXIST,[nodo2.NodeValue]));
        end;
      end;
    end else if UpCAse(nodo2.NodeName)='OPENON' then begin  //evento de apertura
      //lee parámetros
      hayOpen :=true;   //marca para indicar que hay lista
      tBefPatt := ReadXMLParam(nodo2,'BeforePattern');
      tAftPatt := ReadXMLParam(nodo2,'AfterPattern');
      CheckXMLParams(nodo2, 'BeforePattern AfterPattern');  //puede generar excepción
      np := AddOpenPattern(tAftPatt.val, tBefPatt.val);
      //verifica contenido
      listIden := nodo2.TextContent;
      if listIden<>'' then begin
        //Se ha especificado lista de palabras. Los carga
        AddToOpenPatternsL(np, listIden, nil);
      end;
    end else if UpCAse(nodo2.NodeName)='LIST' then begin  //forma alternativa para lista de palabras
      //Esta forma de declaración permite definir un orden en la carga de listas
      hayList :=true;   //marca para indicar que hay lista
      //lee parámetros
      tBlock := ReadXMLParam(nodo2,'Block');
      CheckXMLParams(nodo2, 'Block');  //puede generar excepción
      if tBlock.hay then begin
        blk := SearchBlock(tBlock.val, success);
        if not success then begin
          raise ESynFacilSyn.Create(ERR_INVAL_BLK_NAME);
        end;
      end else begin
        blk := nil;
      end;
      //ve contenido
      listIden := nodo2.TextContent;
      if listIden<>'' then begin
         //Se ha especificado lista de palabras. Los carga en AllItems[]
         AddCompItemL(listIden, blk);
      end;
    end else if nodo2.NodeName='#text' then begin
      //éste nodo aparece siempre que haya espacios, saltos o tabulaciones
    end else if LowerCase(nodo2.NodeName) = '#comment' then begin
      //solo para evitar que de mensaje de error
    end else begin
      raise ESynFacilSyn.Create(Format(ERR_INVAL_LAB_COMP,[nodo2.NodeName]));
    end;
  end;
  //verifica si se especificaron listas
  if not hayList and not hayOpen then begin
    //No se definieron listas ni eventos.
    //Se verifica si hay lista en el cuerpo de <completion></completion>
    listIden := nodo.TextContent;
    if listIden<>'' then begin
       //Se ha especificado lista de palabras directamente. Los carga en AllItems[]
       AddCompItemL(listIden, nil);
    end;
  end;
end;
procedure TSynFacilComplet.LoadFromFile(Arc: string);
var
  doc: TXMLDocument;
  i: Integer;
  nodo: TDOMNode;
  nombre: WideString;
begin
  inherited LoadFromFile(Arc);  {Puede disparar excepción. El mesnajes de error generado
                                incluye el nombre del archivo}
  OpenOnKeyUp := true;   //por defecto
  ReadSpecialIdentif;    //carga los identificadores especiales
  setlength(AllItems,0);  //inicia lista
  ClearOpenPatterns;         //limpia patrones de apertura
  try
    ReadXMLFile(doc, Arc);  //carga archivo
    //procede a la carga de la etiqueta <COMPLETION>
    for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do begin
       // Lee un Nodo o Registro
       nodo := doc.DocumentElement.ChildNodes[i];
       nombre := UpCase(nodo.NodeName);
       if nombre = 'COMPLETION' then  begin
         //forma corta de <TOKEN ATTRIBUTE='KEYWORD'> lista </TOKEN>
         ProcCompletionLabel(nodo);  //Puede generar error
       end;
    end;
    doc.Free;  //libera
  except
    on e: Exception do begin
      //Completa el mensaje con nombre de archivo, porque esta parte del código
      //no lo incluye.
      e.Message:=ERROR_LOADING_ + Arc + #13#10 + e.Message;
      doc.Free;
      raise   //genera de nuevo
    end;
  end;
end;
procedure TSynFacilComplet.SelectEditor(ed0: TSynEdit);
//Inicia el motor de ayuda contextual, en el editor indicado
begin
  ed := ed0;    //guarda referencia
  if ed = nil then begin
    showmessage('ERROR: Se requiere un editor para el autocompletado.');
    ed := nil;   //para indicar que no es válido
    exit;
  end;
  //asigna por si acaso no se había hecho
  ed.Highlighter :=  self;
  MenuComplet:=TSynCompletionF.Create(ed.Owner);   //crea menú contextual en el formulario
  MenuComplet.Editor:=ed;     //asigna editor
  MenuComplet.Width:=200;     //ancho inicial
  MenuComplet.OnExecute:=@OnExecute;
  MenuComplet.OnCodeCompletion:=@OnCodeCompletion;
  //iintercepta eventos de teclado, para cambiar comportamiento
  MenuComplet.OnKeyDown:=@FormKeyDown;
  MenuComplet.OnUTF8KeyPress:=@FormUTF8KeyPress;  //eventos del teclado de la ventana de completado
end;
procedure TSynFacilComplet.UnSelectEditor;
//Método que quita la ayuda contextual al formulario indicado y al editor.
//Se debería llamar siempre si se ha llamado a SelectEditor().
begin
  if MenuComplet = nil then exit;  //nunca se creó
  MenuComplet.Destroy;
  MenuComplet := nil;  //lo marca como  liberado
end;

function TSynFacilComplet.CheckForClose: boolean;
//Verifica y cierra la ventana de completado si el cursor está fuera del token actual.
//SI se cierra devuelve TRUE.
begin
  Result := false;
  //verifica si sale de token actual
  if ed.CaretX <= Pos0.x then begin
    MenuComplet.Deactivate;
    Result := true;
  end;
end;
procedure TSynFacilComplet.LookAroundCursor;
{Analiza el estado del cursor en el editor. Se supone que se debe llamar, después de
 actualizar el editor. Actualiza: PosiCursor, IdentAct, IdentAct0, IdentAnt, BloqueAct }
var
  curTok: integer;
  CurX: Integer;
begin
  //valores por defecto
  IdentAct:='';
  IdentAct0:='';
  IdentAnt:='';
  BloqueAct := nil;
  //explora la línea con el resaltador
  self.ExploreLine(ed.CaretXY, tokens, curTok);
  if curTok=-1 then exit;   //no ubica al token actual
{  //Ubica el token de trabajo. Puede ser el actual o el anterior
  //ve si estamos al inicio de un token
  CurX := ed.LogicalCaretXY.x;  //usa posición física para comparar
  if tokens[curTok].posIni+1 = CurX then begin
    //Estamos justo al inicio del token (después de un token)
    iCurTok:=curTok-1;  //toma el anterior
    if iCurTok<0 then iCurTok := 0; //a menos que sea el primero
  end else begin
    //estamos en medio de un token (no necesariamente identificador)
    iCurTok:=curTok;  //toma el mismo
  end;
  tok0 := tokens[icurTok];    //lee token actual}
  iCurTok := curTok;
  tok0 := tokens[icurTok];    //lee token actual
  CurX := ed.LogicalCaretXY.x;  //usa posición física para comparar
  inMidTok := tokens[curTok].posIni+1 <> CurX;

  //captura "IdentAct0" y "BloqueAct"
  CurX := ed.LogicalCaretXY.x;  //usa posición física para comparar
  IdentAct0:= copy(tok0.txt,1,CurX-tok0.posIni-1);
  IdentAct:=tok0.txt;
  BloqueAct := tok0.curBlk;  //devuelve bloque
end;

procedure TSynFacilComplet.ClearOpenPatterns;
{Elimina los patrones de apertura}
begin
  setlength(OpenPatterns,0);
end;
function TSynFacilComplet.AddOpenPattern(AfterPattern, BeforePattern: string): integer;
{Permite agregar un patrón de apertura}
const
  wordChars = ['a'..'z','0'..'9','A'..'Z','_'];
  strDelim = ['''','"'];
  allIdentif = 'AllIdentifiers';
  function AddPattern(var opPat: TFaOpenPattern): integer;
  {Agrega un patrón, y devuelve el índice al patrón agregado}
  var
    n: Integer;
  begin
    n := high(OpenPatterns)+1;  //número de elementos
    setlength(OpenPatterns, n+1);
    OpenPatterns[n] := opPat;
    Result := n;
  end;
  function ExtractItem(var pat: string): string;
  {Extrae un elemento de una cadena de patrón. La cadena puede ser algo así como
   "Identifier,'.',AllIdentifiers" }

  var
    i: Integer;
    ci: Char;
  begin
    pat := trim(pat);     //quita espacios
    if pat='' then exit('');  //no hay más elementos
    if pat[1] in wordChars then begin
      //es una palabra
      //captura nombre de tipo de token o la cadena especial "AllIdentifiers"
      i := 1;
      while (i<=length(pat)) and (pat[i] in wordChars)  do begin
        inc(i);
      end;
      //if i>length(pat) then exit('#');   //hay error
    end else if pat[1] in strDelim then begin
      //es un literal cadena
      ci := pat[1];   //caracter inicial
      i := 2;
      while (i<=length(pat)) and (pat[i] <> ci) do begin
        inc(i);
      end;
      if i>length(pat) then exit('#');   //hay error
      inc(i);
    end else begin
      exit('#');   //hay error
    end;
    Result := copy(pat, 1,i-1);  //extrae cadena
    pat := copy(pat, i, 200); //recorta
    pat := trim(pat);     //quita espacios
    //quita posible coma final
    if (pat<>'') and (pat[i] = ',') then pat := copy(pat, 2, 200);
  end;
  procedure AddElement(strElem: string; var patEle: TFaPatternElement; var success: boolean);
  {Agrega un elemento a un registro TFaOpenPattern, de acuerdo a su contenido.}
  begin
    success := true;
    if strElem[1] in wordChars then begin
      //es una palabra
      if upcase(strElem) = upcase(allIdentif) then begin
         //es de tipo "Todos los identificadores"
         patEle.patKind := pak_Identif;
      end else if strElem[1] = '!' then begin
        //debe ser de tipo "No es ..."
        strElem := copy(strElem,2,length(strElem)); //quita caracter
        if upcase(strElem) = upcase(allIdentif) then begin
           //es de tipo "Todos los identificadores"
           patEle.patKind := pak_NoIdentif;
        end else begin
           //debe ser un nombre de tipo de token
           if IsAttributeName(strElem) then begin  //es
             patEle.patKind := pak_NoTokTyp;
             patEle.toktyp := GetAttribByName(strElem);   //tipo de atributo
           end else begin  //no es, debe haber algún error
             success := false;
             exit;
           end;
        end;
      end else begin
         //debe ser un nombre de tipo de token
         if IsAttributeName(strElem) then begin  //es
           patEle.patKind := pak_TokTyp;
           patEle.toktyp := GetAttribByName(strElem);   //tipo de atributo
         end else begin  //no es, debe haber algún error
           success := false;
           exit;
         end;
      end;
    end else if strElem[1] in strDelim then begin
      //es una cadena delimitada
      if strElem[length(strElem)] <> strElem[1] then begin  //verificación
        success := false;
        exit;
      end;
      patEle.patKind := pak_String;
      patEle.str:= copy(strElem, 2, length(strElem)-2);
    end else begin
      success := false;
    end;
  end;
  procedure shiftPatterns(var opPat: TFaOpenPattern);
  {Desplaza un elemento del patrón, poniendo el de menor índice en pak_none. No toca el de
  índice 0}
  begin
    opPat.elem[-1] := opPat.elem[-2];
    opPat.elem[-2] := opPat.elem[-3];
    opPat.elem[-3].patKind := pak_none;
    dec(opPat.nBef);  //actualiza elementos anteriores válidos
  end;
var
  opPat: TFaOpenPattern;
  elem : String;
  nelem: Integer;
  success: boolean;
  i: Integer;
begin
  ///////analiza AfterPattern
  AfterPattern := trim(AfterPattern);
  if AfterPattern='' then begin
    //no se especifica
    opPat.elem[-3].patKind := pak_none;
    opPat.elem[-2].patKind := pak_none;
    opPat.elem[-1].patKind := pak_none;
    opPat.nBef:=0;  //no hay elementos válidos
  end else begin
    //Caso común
     //extrae y guarda los elementos
    elem := ExtractItem(AfterPattern);
    nelem := 0;  //contedor
    while (elem<>'#') and (elem<>'') and (nelem<=3) do begin
      AddElement(elem, opPat.elem[nelem-3], success);
      if not success then begin
        raise ESynFacilSyn.Create(ERR_PAR_BEF_PATT);
      end;
      elem := ExtractItem(AfterPattern);
      inc(nelem);
    end;
    //verifica
    if elem = '#' then begin
      raise ESynFacilSyn.Create(ERR_PAR_BEF_PATT);
    end;
    if nelem>3 then begin
      raise ESynFacilSyn.Create(ERR_PAR_BEF_PATT);
    end;
    //alinea elementos encontrados
    opPat.nBef:=3;   //valor asumido, se ajustará al valor real
    for i:=1 to 3-nelem do begin
      shiftPatterns(opPat);
    end;
  end;
  ///////analiza BeforePattern
  BeforePattern := trim(BeforePattern);
  if BeforePattern='' then begin
    //no se especifica
    opPat.elem[0].patKind := pak_none;
    opPat.nAft:=0;  //no hay
  end else begin
    //hay un patrón después
    elem := ExtractItem(BeforePattern);
    if elem = '#' then begin
      raise ESynFacilSyn.Create(ERR_PAR_AFT_PATT);
    end;
    if BeforePattern<>'' then begin  //no deberái quedar nada
      raise ESynFacilSyn.Create(ERR_PAR_AFT_PATT);
    end;
    AddElement(elem, opPat.elem[0], success);
    if not success then begin
      raise ESynFacilSyn.Create(ERR_PAR_AFT_PATT);
    end;
    opPat.nAft:=1;  //hay uno
  end;
  setlength(opPat.Items, 0);  //inicia tabla de ítems
  Result := AddPattern(opPat);  //agrega
end;
procedure TSynFacilComplet.AddToOpenPatternsL(np: integer; list: string;
                                                 blk: TFaSynBlock);
{Agrega una lista de palabras al patrón indicado por el índice "n". }
var
  lst: TStringList;
  n: Integer;
  cItems: ^TFaCompletItems;
  r : TFaCompletItem;
  i: Integer;
begin
  lst := TStringList.Create;
  lst.Delimiter := ' ';
  lst.DelimitedText := list;

  //agrega
  cItems := @OpenPatterns[np].Items;  //referencia abreviada
  n := high(cItems^)+1;  //tamaño de matriz
  setlength(cItems^,n+lst.Count);
  for i:= 0 to lst.Count-1 do begin
    r.item:=lst[i];
    r.content:=r.item;
    r.block := blk;
//    r.posit := posit;
//    r.before:= before;
    cItems^[n+i] := r;
  end;
  lst.Destroy;
end;

procedure TSynFacilComplet.OnCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
//Se genera antes de hacer el reemplazo del texto. "Value", es la cadena que se usará
//para reemplazar la palabra en el editor.
begin
  //Se puede usar "MenuComplet.Position" para saber el elemento seleccionado de la lista
//  value := 'hola';
//  showmessage(value);
end;
procedure TSynFacilComplet.OnExecute(Sender: TObject);
//Este evento se genera antes de abrir el menú de completado.
//Llena la lista "AvailItems", con los ítems que correspondan de acuerdo a la posición
//actual del cursor y de la configuración del archivo XML.
//Luego llena el menú contextual con los ítems filtrados de acuerdo a la posición actual.
  function ItemInBlock(i: Integer): boolean;  inline;
  {Indica si el item de AllItems[], cumple la condición del bloque actual}
  begin
    Result := (AllItems[i].block = nil) or //es válido para todos los bloques
              (AllItems[i].block = BloqueAct);
  end;
  function MatchPatternElement(pe: TFaPatternElement; itok: integer): boolean;
  {Verifica el elemento de un patrón, coincide con un token de tokens[]}
  begin
    if itok<0 then exit(false); //no existe este token
    case pe.patKind of
    pak_none:           //*** No definido.
      exit(true);  //No debería llegar aquí.
    pak_String: begin   //*** Es una cadena
      if CaseSensComp then begin //comparación con caja
        if tokens[itok].txt = pe.str then exit(true)
        else exit(false);
      end else begin    //comparación sin caja
        if UpCase(tokens[itok].txt) = UpCase(pe.str) then exit(true)
        else exit(false);
      end;
    end;
    pak_Identif: begin  //*** Es identificador
      Result := tokens[itok].IsIDentif;
    end;
    pak_NoIdentif: begin
      Result := not tokens[itok].IsIDentif;
    end;
    pak_TokTyp: begin   //*** Es un tipo específico de token
      Result := pe.toktyp = tokens[itok].TokTyp;
    end;
    pak_NoTokTyp: begin   //*** Es un tipo específico de token
      Result := not (pe.toktyp = tokens[itok].TokTyp);
    end;
    end;
  end;
  function MatchPatternBefore(const opPat: TFaOpenPattern): boolean;
  {Verifica si el patrón indicado, cumple con las condiciones actuales (before)}
  begin
    case opPat.nBef of
    0: begin  //no hay elementos, siempre cumple
      exit(true);
    end;
    1: begin  //hay elem[-1]
        if inMidTok then begin
          Result := MatchPatternElement(opPat.elem[-1], iCurTok);
        end else begin  //está al inicio de un token
          Result := MatchPatternElement(opPat.elem[-1], iCurTok-1);
        end;
    end;
    2: begin  //hay elem[-2],elem[-1]
        if inMidTok then begin
          Result := MatchPatternElement(opPat.elem[-1], iCurTok) and
                    MatchPatternElement(opPat.elem[-2], iCurTok-1);
        end else begin  //está al inicio de un token
          Result := MatchPatternElement(opPat.elem[-1], iCurTok-1) and
                    MatchPatternElement(opPat.elem[-2], iCurTok-2);
        end;
    end;
    3: begin  //hay elem[-3],elem[-2],elem[-1]
        if inMidTok then begin
          Result := MatchPatternElement(opPat.elem[-1], iCurTok) and
                    MatchPatternElement(opPat.elem[-2], iCurTok-1) and
                    MatchPatternElement(opPat.elem[-3], iCurTok-2);
        end else begin  //está al inicio de un token
          Result := MatchPatternElement(opPat.elem[-1], iCurTok-1) and
                    MatchPatternElement(opPat.elem[-2], iCurTok-2) and
                    MatchPatternElement(opPat.elem[-3], iCurTok--3);
        end;
    end;
    end;
  end;
  function MatchPatternAfter(const opPat: TFaOpenPattern): boolean;
  {Verifica si el patrón indicado, cumple con las condiciones actuales (after)}
  begin
    case opPat.nAft of
    0: begin  //no hay elementos, siempre cumple
      exit(true);
    end;
    1: begin  //hay elem[0]
        //es independiente de "inMidTok"
        Result := MatchPatternElement(opPat.elem[0], iCurTok);
      end;
    end;
  end;
var
  i: Integer;
  opPat: TFaOpenPattern;
begin
  //limpìa listas
  AvailItems.Clear;
  MenuComplet.ItemList.Clear;  //inicia en cero,  por si no se llena.
  //Analiza entorno de cursor
  LookAroundCursor;  //actualiza IdentAct, IdentAnt, BloqueAct, PosiCursor
  //Verifica si se va a abrir la lista por tecla común o por un atajo
  if MenuComplet.CurrentString='<KeyUp>' then begin
//    debugln('Abierto por tecla común utKey='+utKey+',vKey='+IntToStr(vKey));
    if (vKey=VK_TAB) and (vShift=[]) then begin
      //esta tecla es válida
    end else begin
      if utKey='' then begin
        //La tecla pulsada no es un caracter imprimible
        exit;
      end;
      //Verifica si es tecla válida
      if not (utKey[1] in ['a'..'z','A'..'Z',' ',#9]) then begin
        exit;
      end;
    end;
  end;
  //Analiza entorno y carga AvailItems[], con la lista de completado que corresponda
  //Los items en AllItems[], se cargan siempre
 for i:=0 to high(AllItems) do begin
    if ItemInBlock(i) then begin //y en identificadores
      AvailItems.Add(AllItems[i].item);
    end;
  end;
  //Verifica patrones de apertura
  for opPat in OpenPatterns do begin
    if MatchPatternBefore(opPat) and MatchPatternAfter(opPat) then begin
      //se cumple el patrón en la posición actual del cursor
      for i:=0 to high(opPat.Items) do begin  //agrega sus ítems
        AvailItems.Add(opPat.Items[i].item);
      end;
    end;
  end;
  //Aplica Filtro
  FillCompletMenuFilteredBy(IdentAct0);

//Después de llenar la lista, se puede ver si tiene o no elementos
  if MenuComplet.ItemList.Count <> 0 then begin  //se abrirá
    if MenuComplet.ItemList.Count  = 1 then begin
      //se agrega un elemento más porque sino SynCompletion, hará el reemplazo automáticamente.
      MenuComplet.ItemList.Add('');
    end;
    //aprovechamos para guardar la posición de inicio del token identificador
    Pos0 := Point(tok0.posIni+1, ed.CaretY);   //guarda la posición de origen del token actual
//    debugln('Fij.Pos0.X='+IntToStr(Pos0.x));
  end;
end;
procedure TSynFacilComplet.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
procedure SeleccionarPalabra;
//Selecciona, la palabra actual de la lista de completado. Usamos nuestra propia
//rutina en vez de OnValidate(), para poder reemplazar identificadores de acuerdo
//a la definición de sintaxis, además de otros tipos de tokens.
var
  NewWord: String;
  Pos1: TPoint;
  Pos2: TPoint;
begin
  if CurrentLines = nil then exit;
  //Reemplaza actual
//  ShowMessage(tok0.txt);
  NewWord := MenuComplet.ItemList[MenuComplet.Position];
  //calcula intervalo del token
  Pos1 := Point(tok0.posIni+1,ed.CaretY);
  Pos2 := Point(tok0.posIni+tok0.length+1,ed.CaretY);
//  ShowMessage(IntToStr(tok0.posIni+1)+','+IntToStr(tok0.posIni+tok0.length+1)) ;
  ed.TextBetweenPoints[Pos1,Pos2] := NewWord;  //usa TextBetweenPoints(), para poder deshacer
  ed.LogicalCaretXY :=Point(tok0.posIni+length(NewWord)+1,ed.CaretY);  //mueve cursor
  CloseCompletionWindow;  //cierra
end;
begin
//debugln('Form.OnKeyDown:'+IdentAct0+':'+IntToStr(ed.CaretX));
  case Key of
    VK_RETURN: begin
        if Shift= [] then begin
           if SelectOnEnter then  //solo si está permitido reemplazar
             SeleccionarPalabra;
           Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
        end else begin
          Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
          CloseCompletionWindow;  //cierra
        end;
      end;
    VK_HOME: begin
        if Shift = [] then begin  //envía al editor
           ed.CommandProcessor(ecLineStart, #0, nil);
           MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelLineStart, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_END: begin
        if Shift = [] then begin  //envía al editor
           ed.CommandProcessor(ecLineEnd, #0, nil);
           MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelLineEnd, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_BACK: begin
        if Shift = [] then begin  //son Ctrl o Shift
           ed.CommandProcessor(ecDeleteLastChar, #0, nil);  //envía al editor
           if CheckForClose then begin Key:=VK_UNKNOWN;; exit end;
           LookAroundCursor;  //solo para actualizar el identificador actual
           FillCompletMenuFilteredBy(IdentAct0);
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_LEFT: begin
        if Shift = [] then begin  //envía al editor
          ed.CommandProcessor(ecLeft, #0, nil);
          if CheckForClose then begin Key:=VK_UNKNOWN;; exit end;
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelLeft, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssCtrl] then begin
          ed.CommandProcessor(ecWordLeft, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift,ssCtrl] then begin
          ed.CommandProcessor(ecSelWordLeft, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_RIGHT: begin
        if Shift = [] then begin  //envía al editor
          ed.CommandProcessor(ecRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssCtrl] then begin
          ed.CommandProcessor(ecWordRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift,ssCtrl] then begin
          ed.CommandProcessor(ecSelWordRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_TAB: begin
        if Shift = [] then begin
          SeleccionarPalabra;
          SearchOnKeyUp := false;  {para que no intente buscar luego en el evento KeyUp,
                   porque TAB está configurado como tecla válida para abrir la lista, y si
                   se abre, (y no se ha isertado el TAB), aparecerá de nuevo el mismo
                   identificador en la lista}
          Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
        end;
      end;
  end;
  //si no lo procesó aquí, lo procesará SynCompletion
end;
procedure TSynFacilComplet.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
//Este evento se dispara cuando ya está visible la ventana de autocompletado y por
//lo tanto no se generará el evento KeyUp
begin
  //Como este evento se genera apneas pulsar una tecla, primero pasamos la tecla al
  //editor para que lo procese y así tendremos el texto modificado, como si estuviéramos
  //después de un KeyUp().
  ed.CommandProcessor(ecChar, UTF8Key, nil);
  UTF8Key := '';  //limpiamos para que ya no lo procese SynCompletion
  //ahora ya tenemos al editor cambiado
//debugln('Form.OnKeyPress:'+IdentAct0+':'+IntToStr(ed.CaretX));
//Las posibles opciones ya se deben haber llenado. Aquí solo filtramos.
  LookAroundCursor;  //solo para actualizar el identificador actual
  if CheckForClose then exit;
  FillCompletMenuFilteredBy(IdentAct0);
end;
procedure TSynFacilComplet.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
{Debe recibir la tecla pulsada aquí, y guardarla para KeyUp, porque allí no se puede
reconocer caracteres ASCII. Se usa UTF para hacerlo más fléxible}
begin
  utKey:=UTF8Key;  //guarda tecla
end;
procedure TSynFacilComplet.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{Verifica la tecla pulsada, para determinar si abrir o no el menú de ayuda contextual
 Debe llamarse después de que el editor ha procesado el evento, para tener
 el estado final del editor
 Este evento solo se ejecutará una vez antes de abrir la ventana de autocompletado}
begin
  if not CompletionOn then exit;
  if not OpenOnKeyUp then exit;
  if not SearchOnKeyUp then begin
    //Se ha inhabilitado este evento para el completado
    SearchOnKeyUp := true;
    exit;
  end;
  //verificación principal
  if MenuComplet.IsActive then begin
    //verifica si debe desaparecer la ventana, por mover el cursor a una posición anterior
//    if CompareCarets(Pos0, CurrentEditor.CaretXY) < 0 then
//      Deactivate;
    exit;   //ya está mostrado
  end;
  if ed = NIL then exit;     //no hay editor
  if ed.SelectionMode <> smNormal then exit;  //para no interferir en modo columna
  //captura las teclas pulsadas y llama a OnExecute(), para ver si correspodne mostrar
  //la ventana de completado
  vKey := Key;   //guarda
  vShift := Shift;
  //Dispara evento OnExecute con el valor de "vKey", "vShift" y "utKey" actualizados
  OpenCompletionWindow;  //solo se mostrará si hay ítems
  vKey := 0;
  vShift := [];  //limpia
  utKey := '';  //limpia por si la siguiente tecla pulsada no dispara a UTF8KeyPress()
  SearchOnKeyUp := true;  //limpia bandera
End;
procedure TSynFacilComplet.FillCompletMenuFilteredBy(str: string);
//Llena el menú de completado a partir de "AvailItems", filtrando solo las
//palabras que coincidan con "str"
var
  l: Integer;
  i: Integer;
begin
  l := length(str);
//debugln('  FilCompl:'+str);
  //Genera la lista que coincide
  { TODO : Este proceso es lento si se actualizan muchas opciones en la lista }
  MenuComplet.ItemList.Clear;
  if str='*' then begin  //caso comodín
    for i:=0 to AvailItems.Count-1 do begin  //agrega todo
       MenuComplet.ItemList.Add(AvailItems[i]);
    end;
  end else if CaseSensComp then begin
    for i:=0 to AvailItems.Count-1 do begin
      if str = copy(AvailItems[i],1,l) then
         MenuComplet.ItemList.Add(AvailItems[i]);
    end;
  end else begin  //ignora la caja
    str := UpCase(str);
    for i:=0 to AvailItems.Count-1 do begin
      if str = upcase(copy(AvailItems[i],1,l)) then begin
        MenuComplet.ItemList.Add(AvailItems[i]);
      end;
    end;
  end;
  if MenuComplet.ItemList.Count = 0 then begin
    //cierra por no tener elementos
    MenuComplet.Deactivate;
  end else begin
    //hay elementos
    MenuComplet.Position:=0;  //selecciona el primero
  end;
  MenuComplet.TheForm.Invalidate;  //para que se actualice
end;
procedure TSynFacilComplet.OpenCompletionWindow;
//Abre la ayuda contextual, en la posición del cursor.
var p:TPoint;
begin
  //calcula posición donde aparecerá el menú de completado
  p := Point(ed.CaretXPix,ed.CaretYPix + ed.LineHeight);
  p.X:=Max(0,Min(p.X, ed.ClientWidth - MenuComplet.Width));
  p := ed.ClientToScreen(p);
  //Abre menú contextual, llamando primero a OnExecute(). Solo se mostrará si tiene elementos.
  MenuComplet.Execute('<KeyUp>', p.x, p.y);   //pasa una clave cualquiera para identificación posterior
End;
procedure TSynFacilComplet.CloseCompletionWindow;
//Cierra la ventana del menú contextual
begin
  MenuComplet.Deactivate;
end;
constructor TSynFacilComplet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClearOpenPatterns;
  CaseSensComp := false;  //por defecto
  CompletionOn := true;  //activo por defecto
  SelectOnEnter := true;
  AvailItems := TStringList.Create;  //crea lista
  vKey := 0;     //limpia
  vShift := [];  //limpia
  utKey := '';   //limpia
end;
destructor TSynFacilComplet.Destroy;
begin
  if MenuComplet<>nil then MenuComplet.Destroy;  //por si no lo liberaron
  AvailItems.Destroy;
  inherited Destroy;
end;

end.


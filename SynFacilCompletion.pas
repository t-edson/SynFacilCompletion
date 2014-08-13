{
SynFacilCompletion 0.1b
======================
Por Tito Hinostroza 06/08/2014.
Unidad que expande al resaltador TSynFacilSyn, para que pueda soportar configuraiones
de autocompletado de texto.
Requiere de la unidad SynCompetionQ, que es una versión modificada de SynCompletion

Se usa de froma similara a SynFacilSyn. Se debe crear un resaltador, pero ahora de la
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

Luego se debe interceptar el evento KeyUp, del SynEdit:

procedure TForm1.edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Key, Shift);
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
  Classes, SysUtils, Dialogs, XMLRead, DOM, Math, LCLType, SynEditHighlighter,
  SynEdit, SynEditTypes, Lazlogger, SynFacilHighlighter,
  {Debe utilizar su propia unidad SynCompletion, porque maneja de forma diferente los
  eventos del teclado, de la ventana de completado}
  SynCompletionQ;

type
  TCompletItem = record
    item   : string;      //etiqueta a mostrar en el menú
    content: string;      //contenido a reemplazar
    block  : TFaSynBlock; //bloque donde es válida la palabra
    before : string;      //palabra anterior
//    casesen: boolean;     //indica si es sensible a la caja
  end;

  TCompletItems = array of TCompletItem;
  //Posiciones de cursor en el editor
  TPosicCursor=( pcDesconocido,    //posición desconocida
                 pcFinalDeIdent,   //al final de identificador.
                 pcEnMedioIdent,   //en medio de identificador
                 pcDespuesIdent,   //despues de identificador (separado por espacio)
                 pcDespuesPunto);  //despues de punto

  { TSynFacilComplet }
  //clase principal
  TSynFacilComplet = class(TSynFacilSyn)
    procedure OnCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure OnExecute(Sender: TObject);
    procedure OnTextModified(Sender: TObject; KeyChar: TUTF8Char;
      Shift: TShiftState);
  private
    ed: TSynEdit;  //referencia interna al editor
    MenuComplet: TSynCompletion;   //menú contextual
    AvailItems : TStringList;  {Lista de palabras disponible para cargar en el menú de
                                auto-completado}
    PosiCursor: TPosicCursor;  //Posición del cursor
//    Pos0: TPoint;  //Posición incial del cursor donde se abrió la ventana de completado
{ TODO : Pos0, debe manejarse desde dentro de esta clase y no desde SynCompeltionQ }
    IdentAct0: string;         //IDentificador actual desde el inicio hasta el cursor
    IdentAct : string;         //Identificador actual completo
    IdentAnt : string;         //Identificador anterior
    BloqueAct: TFaSynBlock;    //referecnia al bloque actual
    CaseSensComp: boolean;     //Uso de caja, en autocompletado
    procedure AddCompItem(item, content: string; blk: TFaSynBlock);
    procedure AddCompItemL(list: string; blk: TFaSynBlock);
    function EsIdentif(const tok: TFaTokInfo): boolean;
    procedure FillCompletMenuFilteredBy(str: string);
    procedure OpenCompletionWindow;
    procedure ProcCompletionLabel(nodo: TDOMNode);
    procedure ReadSpecialIdentif;
  protected
    SpecIdentifiers: TArrayTokEspec;
    //lista para completado, leidas del archivo XML
    CompWords : TStringList;   //word list for completion
    AllItems  : TCompletItems; //lista de todas las palabras disponibles para el completado
    procedure MiraEntornoCursor; virtual;
    procedure FillCompletionMenu; virtual;
  public
    CompletionOn: boolean;  //activa o desactiva el auto-completado
    procedure LoadFromFile(Arc: string); override;
    function LoadSyntaxFromPath(SourceFile: string; path: string;
      CaseSens: boolean=false): string;
    procedure SelectEditor(ed0: TSynEdit);  //inicia la ayuda contextual
    procedure UnSelectEditor;  //termina la ayuda contextual con el editor
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure CloseCompletionWindow;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  //utilidades
  function ReadExtenFromXML(XMLfile: string): string;
  function XMLFileHaveExten(XMLfile: string; exten: string;
                CaseSens: boolean = false): boolean;

implementation

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
      ShowMessage('Error cargando: ' + XMLfile + #13#10 + e.Message);
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

{ TSynFacilComplet }
procedure TSynFacilComplet.ReadSpecialIdentif;
//Hace una exploración para leer todos los identificadores especiales en una tabla
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
//Agrega un ítem a la lsta de completado
var
  n: Integer;
  r: TCompletItem;
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
  r: TCompletItem;
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
//Procesa la etiqueta <Completion>Permite agregar una lista de identificadores especiales separados por espacios.
var
  listIden: string;
  tCasSen: TFaXMLatrib;
  i: Integer;
  nodo2: TDOMNode;
  tListAttr: TFaXMLatrib;
  tipTok: TSynHighlighterAttributes;
  j: Integer;
begin
  //carga los parámetros
  tCasSen :=LeeAtrib(nodo, 'CaseSensitive');

  //carga atributos leidos
  if tCasSen.hay then  //si se especifica
    CaseSensComp := tCasSen.bol  //se lee
  else  //si no
    CaseSensComp := CaseSensitive;  //toma el del resaltador

  listIden := nodo.TextContent;
  if listIden<>'' then begin
     //se ha especificado una lista de palabras directamente en el cuerpo de
     //<completion></completion>. Los carga en CompWords
     CompWords.Delimiter := ' ';
     CompWords.DelimitedText := listIden;
     AddCompItemL(listIden,nil);
//     showmessage(listIden+','+IntToStr(CompWords.Count));
  end;
  ////////// explora nodos hijos //////////
  for i := 0 to nodo.ChildNodes.Count-1 do begin
    nodo2 := nodo.ChildNodes[i];
    if UpCAse(nodo2.NodeName)='INCLUDE' then begin  //incluye lista de palabras
      tListAttr := LeeAtrib(nodo2,'Attribute');
      //tListBlk := LeeAtrib(nodo2,'Block');
      if ValidarAtribs(nodo2, 'Attribute') then exit;
      if tListAttr.hay then begin
        //se pide agregar la lista de identificadores de un atributo en especial
        if IsAttributeName(tListAttr.val)  then begin
          tipTok := GetAttribByName(tListAttr.val);   //tipo de atributo
          //busca los identificadores para agregarlos
          ReadSpecialIdentif; //lee todos
          for j:= 0 to high(SpecIdentifiers) do begin
            if SpecIdentifiers[j].tTok = tipTok then begin
              CompWords.Add(SpecIdentifiers[j].orig);
              AddCompItem(SpecIdentifiers[j].orig,SpecIdentifiers[j].orig, nil);
            end;
          end;
        end else begin  //atributo no existe
          Err := 'Atributo '+ nodo2.NodeValue+ ' no existe. (etiqueta <COMPLETION ...>)';
          exit;
        end;
      end;
    end else if UpCAse(nodo2.NodeName)='KEYWORD' then begin  //definición alternativa de delimitador
//      tTokPos := LeeAtrib(nodo2,'TokPos');
//      if ValidarAtribs(nodo2, 'TokPos') then exit;
//      //agrega la referecnia del bloque al nuevo token delimitador
//      AddFinBlockToTok(trim(nodo2.TextContent), tTokPos.n, blq);
    end else begin
      Err := 'Etiqueta "' + nodo2.NodeName +
             '" no válida para etiqueta <COMPLETION ...>';
      exit;
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
  inherited LoadFromFile(Arc);
  if Err<>'' then exit;
  CompWords.Clear;  //limpia la lista de palabras
  setlength(AllItems,0);  //inicia lista
  try
    ReadXMLFile(doc, Arc);  //carga archivo
    //procede a la carga de la etiqueta <COMPLETION>
    for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do begin
       // Lee un Nodo o Registro
       nodo := doc.DocumentElement.ChildNodes[i];
       nombre := UpCase(nodo.NodeName);
       if nombre = 'COMPLETION' then  begin
         //forma corta de <TOKEN ATTRIBUTE='KEYWORD'> lista </TOKEN>
         ProcCompletionLabel(nodo);  //Carga Keywords
       end;
       if Err <> '' then begin
          Err +=  ' <' + nombre + '> en: ' + Arc;  //completa mensaje
          break;
       end;
    end;
    doc.Free;  //libera
  except
    on E: Exception do begin
      ShowMessage('Error cargando: ' + Arc + #13#10 + e.Message);
      doc.Free;
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
  MenuComplet:=TSynCompletion.Create(ed.Owner);   //crea menú contextual en el formulario
  MenuComplet.Editor:=ed;     //asigna editor
  MenuComplet.Width:=200;     //ancho inicial
  MenuComplet.OnExecute:=@OnExecute;
  MenuComplet.OnTextModified:=@OnTextModified;  //eventos del teclado de la ventana de completado
  MenuComplet.OnCodeCompletion:=@OnCodeCompletion;
end;

procedure TSynFacilComplet.UnSelectEditor;
//Método que quita la ayuda contextual al formulario indicado y al editor.
//Se debería llamar siempre si se ha llamado a SelectEditor().
begin
  if MenuComplet = nil then exit;  //nunca se creó
  MenuComplet.Destroy;
  MenuComplet := nil;  //lo marca como  liberado
end;

function TSynFacilComplet.EsIdentif(const tok: TFaTokInfo): boolean;
//Permite saber los tipos de tokens que se consideran como identificador
begin
  if (tok.TokTyp = self.tkIdentif) or (tok.TokTyp = self.tkKeyword) then
    Result := true
  else
    Result := false;
end;

procedure TSynFacilComplet.MiraEntornoCursor;
{Analiza el estado del cursor en el editor. Se supone que se debe llamar, después de
 actualizar el editor. Actualiza: PosiCursor, IdentAct, IdentAct0, IdentAnt, BloqueAct }
var
  tokens: TATokInfo;
  curTok: integer;
  tok0: TFaTokInfo;              //token actual
  tok_1, tok_2, tok_3: TFaTokInfo; //tokens anteriores
  CurX: Integer;
begin
  //valores por defecto
  PosiCursor := pcDesconocido;  //inicialmente
  IdentAct:='';
  IdentAct0:='';
  IdentAnt:='';
  BloqueAct := nil;
  //explora la línea con el resaltador
  self.ExploreLine(ed.CaretXY, tokens, curTok);
  tok0 := tokens[curTok];    //lee token actual
  if curTok = 0 then begin   ////////////////es el primer token
    tok_1.txt := '';         //token anterior
    if EsIdentif(tok0) then begin
      //está en medio de un identificador, no queda otra.
      PosiCursor:=pcEnMedioIdent;  //en medio de identificador
      CurX := ed.CaretX;
      IdentAct0:= copy(tok0.txt,1,CurX-tok0.posIni-1);
      IdentAct:=tok0.txt;
      IdentAnt:='';
    end;
    BloqueAct := tok0.curBlk;  //devuelve bloque
  end else begin             ///////////////hay otros tokens anteriores
    //lee el token actual y los anteriores
    tok_1 := tokens[curTok-1];   //token anterior
    if curTok>1 then   //hay anterior
      tok_2 := tokens[curTok-2]
    else begin  //token anterior
      tok_2.TokTyp:=nil;
      tok_2.txt:='';
    end;
    if curTok>2 then   //hay anterior
      tok_3 := tokens[curTok-3]
    else begin  //token anterior
      tok_3.TokTyp:=nil;
      tok_3.txt:='';
    end;
    //analiza la ubicación
    if EsIdentif(tok0) then begin
      //está al inicio o en medio de un identificador, no queda otra.
      if tok0.posIni+1 = ed.CaretX then begin
        //estamos al inicio del identificador

      end else begin
        //estamos en medio de un identificador
        PosiCursor:=pcEnMedioIdent;  //en medio de identificador
        CurX := ed.CaretX;
        IdentAct0:= copy(tok0.txt,1,CurX-tok0.posIni-1);
         IdentAct:=tok0.txt;
        //busca identificador anterior
        if (tok_1.TokTyp = self.tkSpace) and EsIdentif(tok_2) then begin
          //hay identificador anterior
          IdentAnt:=tok_2.txt;
        end else
          IdentAnt:='';
      end;
    end else if EsIdentif(tok_1) then begin
      //El anterior es identificador, entonces el que sigue no lo es.
      if tok0.posIni+1 = ed.CaretX then begin
        //Estamos justo al final del identificador
        PosiCursor:=pcFinalDeIdent;  //después de identificador
        IdentAct:=tok_1.txt;
        IdentAct0:=tok_1.txt;
        //busca identificador anterior
        if (tok_2.TokTyp = self.tkSpace) and EsIdentif(tok_3) then begin
          //hay identificador anterior
          IdentAnt:=tok_3.txt;
        end else
          IdentAnt:='';
      end else begin
        //Estamos en medio del siguiente token

      end;
    end else if not EsIdentif(tok0) and (tok_1.TokTyp = self.tkSpace) and
                    EsIdentif(tok_2) then begin
      //no hay identificador actual pero hay un identificador antes seguido de espacio
      PosiCursor := pcDespuesIdent;
      IdentAct:='';
      IdentAct0:='';
      IdentAnt:=tok_2.txt;
    end;
    BloqueAct := tok0.curBlk;  //devuelve bloque
  end;
end;

procedure TSynFacilComplet.OnCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
//Se genera antes de hacer el reemplazo del texto. "Value", es la cadena que se usará
//para reemplazar la palabra en el editor.
begin
//  value := 'hola';
//  showmessage(value);
end;

procedure TSynFacilComplet.OnExecute(Sender: TObject);
//Este evento se genera antes de abrir el menú de completado
begin
  FillCompletionMenu;
end;
procedure TSynFacilComplet.OnTextModified(Sender: TObject;
  KeyChar: TUTF8Char; Shift: TShiftState);
//Este evento se dispara cuando ya está visible la ventana de autocompletado y por
//lo tanto no se generará el evento KeyUp
begin
  if IdentAct0= '' then exit;
//debugln('OnTextModif');
  MiraEntornoCursor;  //solo para actualizar el identificador actual
  //Las posibles opciones ya se deben haber llenado. Aquí solo filtramos.
  FillCompletMenuFilteredBy(IdentAct0);
//  OpenCompletionWindow;
end;
procedure TSynFacilComplet.KeyUp(Key: Word; Shift: TShiftState);
{Verifica la tecla pulsada, para determinar si abrir o no el menú de ayuda contextual
 Debe llamarse después de que el editor ha procesado el evento, para tener
 el estado final del editor
 Este evento solo se ejecutará una vez antes de abrir la ventana de autocompletado}
begin
  if not CompletionOn then exit;
//debugln('OnKeyUp');
//debugln(IdentAct0 + ',' + IdentAnt);
    //verificación principal
    if MenuComplet.IsActive then Exit;   //ya está mostrado
    if ed = NIL then exit;     //no hay editor
    if ed.SelectionMode <> smNormal then exit;  //para no interferir en modo columna
    //filtra por tecla válida
    if not ( (key in [VK_A .. VK_Z, VK_SPACE, VK_OEM_PERIOD]) and (Shift <= [ssShift]) ) then
       Exit;
    OpenCompletionWindow;  //solo se mostrará si hay ítems
End;

procedure TSynFacilComplet.FillCompletionMenu;
//Llena la lista "AvailItems", con los ítems que correspondan de acuerdo a la posición
//actual del cursor y de la configuración del archivo XML.
//Luego llena el menú contextual con los ítems filtrados de acuerdo a la posición actual.
var
  i: Integer;
begin
  AvailItems.Clear;
  MenuComplet.ItemList.Clear;  //inicia en cero,  por si no se llena.
  //Analiza entorno de cursor
  MiraEntornoCursor;  //actualiza IdentAct, IdentAnt, BloqueAct, PosiCursor
  //Verifica condiciones para mostrar ventana
  case PosiCursor of
  pcEnMedioIdent:  begin //en medio de un identificador
      for i:=0 to high(AllItems) do begin
        if (AllItems[i].block = nil) then  //es válido para todos los bloques
          AvailItems.Add(AllItems[i].item);
      end;
      FillCompletMenuFilteredBy(IdentAct0);
    end;
  pcFinalDeIdent: begin  //caso típico. no se identifica situación especifica
      for i:=0 to high(AllItems) do begin
        if (AllItems[i].block = nil) then  //es válido para todos los bloques
          AvailItems.Add(AllItems[i].item);
      end;
      FillCompletMenuFilteredBy(IdentAct0);
    end;
  pcDespuesIdent: begin   //después de identificador
{        If (IdentAnt = 'RETURN') Then begin
         LlenaAyudContextual_TIPO;
      end else If (IdentAnt = 'CONNECT') Then begin
         LlenaConAyudContextual;
      end}
    end;
  else      //'pcDesconocido' no se identifica el contexto del código
    begin  end;
  end;
end;
procedure TSynFacilComplet.FillCompletMenuFilteredBy(str: string);
//Llena el menú de completado a partir de "AvailItems", filtrando solo las
//palabras que coincidan con "str"
var
  l: Integer;
  i: Integer;
begin
  l := length(str);
  //Genera la lista que coincide
  { TODO : Este proceso es lento si se actualizan muchas opciones en la lista }
  MenuComplet.ItemList.Clear;
  if CaseSensComp then begin
    for i:=0 to AvailItems.Count-1 do begin
      if str = copy(AvailItems[i],1,l) then
         MenuComplet.ItemList.Add(AvailItems[i]);
    end;
  end else begin  //ignora la caja
    str := UpCase(str);
    for i:=0 to AvailItems.Count-1 do begin
      if str = upcase(copy(AvailItems[i],1,l)) then
         MenuComplet.ItemList.Add(AvailItems[i]);
    end;
  end;
  if MenuComplet.ItemList.Count = 0 then begin
    MenuComplet.Deactivate;
  end;
end;
procedure TSynFacilComplet.OpenCompletionWindow;
//Abre la ayuda contextual, en la posición del cursor.
var p:TPoint;
begin
  //calcula posición donde aparecerá el menú de completado
  p := Point(ed.CaretXPix,ed.CaretYPix + ed.LineHeight);
  p.X:=Max(0,Min(p.X, ed.ClientWidth - MenuComplet.Width));
  p := ed.ClientToScreen(p);
  //Abre menú contextual, llamando primero OnExecute(). Solo se mostrará si tiene elementos.
  MenuComplet.Execute('', p.x, p.y);
End;
procedure TSynFacilComplet.CloseCompletionWindow;
//Cierra la ventana del menú contextual
begin
  MenuComplet.Deactivate;
end;
constructor TSynFacilComplet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CaseSensComp := false;  //por defecto
  CompletionOn := true;  //activo por defecto
  CompWords := TStringList.Create;
  AvailItems := TStringList.Create;  //crea lista
end;
destructor TSynFacilComplet.Destroy;
begin
  if MenuComplet<>nil then MenuComplet.Destroy;  //por si no lo liberaron
  AvailItems.Destroy;
  CompWords.Destroy;
  inherited Destroy;
end;

end.


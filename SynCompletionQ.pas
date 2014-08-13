{SynCOmpletionQ
 ==============
 Por Tito Hinostroza
 Modificado 06/08/2014
 Unidad que implementa el autocompletado de texto en un editor SynEdit. Se ha creado a
 partir de TsynCompletion, modificándolo para permitir seguir escribiendo el texto, sin
 que la ayuda contextual sea un estorbo (se modifica TSynBaseCompletionForm.KeyDown() y
 TSynBaseCompletionForm.UTF8KeyPress() ).
 Además, para facilitar la implementación de la ayuda contextual, más inteligente, se
 entrega  información sobre la posición del cursor con respecto al texto actual (
 Variable Pos0).
 }
unit SynCompletionQ;

{$I c:\lazarus\components\synedit\SynEdit.inc}

{$DEFINE HintClickWorkaround} // Workaround for issue 21952

interface

uses
  LCLProc, LCLIntf, LCLType, LMessages, Classes, Graphics, Forms,
  Controls, StdCtrls, ExtCtrls, Menus, SysUtils, types,
  SynEditMiscProcs, SynEditKeyCmds, SynEdit, SynEditTypes, SynEditPlugins;

type

  TCodeCompletionEvent = procedure(var Value: string;
                                   SourceValue: string;
                                   var SourceStart, SourceEnd: TPoint;
                                   KeyChar: TUTF8Char;
                                   Shift: TShiftState) of object;
  TValidateEvent = procedure(Sender: TObject;
                             KeyChar: TUTF8Char;
                             Shift: TShiftState) of object;
  TSynBaseCompletionForm = class;

  { TSynBaseCompletionHint }

  TSynBaseCompletionHint = class(THintWindow)
  private
    FCompletionForm: TSynBaseCompletionForm;
    FDisplayRect: TRect;
    FIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    procedure Paint; override;
    property Index: Integer read FIndex write FIndex;
    property DisplayRect: TRect read FDisplayRect write FDisplayRect;
  end;


  TSynCompletionLongHintType = (sclpNone,
                                sclpExtendRightOnly,
                                sclpExtendHalfLeft,
                                sclpExtendUnlimitedLeft
                               );

  { TSynBaseCompletionFormSizeDrag }

  TSynBaseCompletionFormSizeDrag = class(TPanel)
  private
    FMouseDownPos, FMouseLastPos, FWinSize: TPoint;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  end;

  { TSynBaseCompletionForm }

  TSynBaseCompletionForm = class(TForm)
    procedure SDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SDKeyPress(Sender: TObject; var Key: char);
    procedure SDUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  protected
    FItemList: TStrings;
    FPosition: Integer;
    FNbLinesInWindow: Integer;
    FFontHeight: integer;
    FResizeLock: Integer;
    Scroll: TScrollBar;
    SizeDrag: TSynBaseCompletionFormSizeDrag;
    FOnKeyPress: TKeyPressEvent;
    FOnValidate: TValidateEvent;
    FOnTextModified: TValidateEvent;
    FClSelect: TColor;
    FBackgroundColor: TColor;
    FTextColor: TColor;
    FTextSelectedColor: TColor;
    FHint: TSynBaseCompletionHint;
    FHintTimer: TTimer;
    FLongLineHintTime: Integer;
    FLongLineHintType: TSynCompletionLongHintType;
    FMouseWheelAccumulator: Integer;
    procedure DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEditorKeyPress(Sender: TObject; var Key: char);
    procedure DoEditorUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure AppDeactivated(Sender: TObject); // Because Form.Deactivate isn't called
    procedure Deactivate; override;
    procedure SelectPrec;
    procedure SelectNext;
    procedure ScrollChange(Sender: TObject);
    procedure ScrollGetFocus(Sender: TObject);
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SetItemList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure StringListChange(Sender: TObject);
    procedure DoOnResize; override;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure FontChanged(Sender: TObject); override;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
  private
    FCurrentEditor: TCustomSynEdit; // Must only be set via TSynCompletion.SetEditor
    FDoubleClickSelects: Boolean;
    FDrawBorderWidth: Integer;
    FOnDragResized: TNotifyEvent;
    FShowSizeDrag: Boolean;
    FHintLock: Integer;
    procedure SetCurrentEditor(const AValue: TCustomSynEdit);
    procedure SetDrawBorderWidth(const AValue: Integer);
    procedure SetLongLineHintTime(const AValue: Integer);
    procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetShowSizeDrag(const AValue: Boolean);
  protected
    procedure RegisterHandlers(EditOnly: Boolean = False);
    procedure UnRegisterHandlers(EditOnly: Boolean = False);
    procedure SetVisible(Value: Boolean); override;
    property DrawBorderWidth: Integer read FDrawBorderWidth write SetDrawBorderWidth;
    procedure IncHintLock;
    procedure DecHintLock;
    procedure DoOnDragResize(Sender: TObject);
  public
    Pos0: TPoint;  //Posición incial del cursor donde se abrió la ventana de completado

    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    function Focused: Boolean; override;
    procedure ShowItemHint(AIndex: Integer);
    procedure OnHintTimer(Sender: TObject);
    // Must only be set via TSynCompletion.SetEditor
    property CurrentEditor: TCustomSynEdit read FCurrentEditor;
  published
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property ItemList: TStrings read FItemList write SetItemList;
    property Position: Integer read FPosition write SetPosition;
    property NbLinesInWindow: Integer read FNbLinesInWindow write SetNbLinesInWindow;
    property ClSelect: TColor read FClSelect write FClSelect;
    property FontHeight:integer read FFontHeight;
    property OnTextModified:TValidateEvent read FOnTextModified write FOnTextModified;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property TextColor: TColor read FTextColor write FTextColor;
    property TextSelectedColor: TColor read FTextSelectedColor write FTextSelectedColor;
    property LongLineHintTime: Integer read FLongLineHintTime write SetLongLineHintTime default 0;
    property LongLineHintType: TSynCompletionLongHintType read FLongLineHintType
             write FLongLineHintType default sclpExtendRightOnly;
    property DoubleClickSelects: Boolean read FDoubleClickSelects write FDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read FShowSizeDrag write SetShowSizeDrag default False;
  end;

  { TSynBaseCompletion }
  {Controla la aparición y desaparición de la ventana de opciones: TSynBaseCompletionForm}
  TSynBaseCompletion = class(TLazSynMultiEditPlugin)
  private
    Form: TSynBaseCompletionForm;
    FAddedPersistentCaret: boolean;
    FOnExecute: TNotifyEvent;
    FWidth: Integer;
    function GetClSelect: TColor;
    function GetDoubleClickSelects: Boolean;
    function GetLongLineHintTime: Integer;
    function GetLongLineHintType: TSynCompletionLongHintType;
    function GetOnKeyDown: TKeyEvent;
    function GetShowSizeDrag: Boolean;
    procedure SetClSelect(const Value: TColor);
    function GetItemList: TStrings;
    function GetNbLinesInWindow: Integer;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetDoubleClickSelects(const AValue: Boolean);
    procedure SetItemList(const Value: TStrings);
    procedure SetLongLineHintTime(const AValue: Integer);
    procedure SetLongLineHintType(const AValue: TSynCompletionLongHintType);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    procedure SetShowSizeDrag(const AValue: Boolean);
    procedure SetWidth(Value: Integer);
    function GetOnUTF8KeyPress: TUTF8KeyPressEvent;
    procedure SetOnUTF8KeyPress(const AValue: TUTF8KeyPressEvent);
    function GetFontHeight:integer;
    function GetOnTextModified:TValidateEvent;
    procedure SetOnTextModified(NewValue :TValidateEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer); overload;
    procedure Execute(s: string; TopLeft: TPoint); overload;
    procedure Execute(s: string; TokenRect: TRect); overload; // Excute below or above the token // may be extended to adjust left corner too
    procedure Deactivate;
    function IsActive: boolean;
    function TheForm: TSynBaseCompletionForm;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnUTF8KeyPress
                                                write SetOnUTF8KeyPress;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property FontHeight: integer read GetFontHeight;
    property ClSelect: TColor read GetClSelect write SetClSelect; deprecated; // use SelectedColor
    property NbLinesInWindow: Integer read GetNbLinesInWindow write SetNbLinesInWindow; deprecated;
  published
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property ItemList: TStrings read GetItemList write SetItemList;
    property Position: Integer read GetPosition write SetPosition;
    property LinesInWindow: Integer read GetNbLinesInWindow
                                      write SetNbLinesInWindow;
    property OnTextModified: TValidateEvent read GetOnTextModified write SetOnTextModified;
    property SelectedColor: TColor read GetClSelect write SetClSelect;
    property Width: Integer read FWidth write SetWidth;
    property LongLineHintTime: Integer read GetLongLineHintTime
             write SetLongLineHintTime default 0;
    property LongLineHintType: TSynCompletionLongHintType read GetLongLineHintType
             write SetLongLineHintType default sclpExtendRightOnly;
    property DoubleClickSelects: Boolean read GetDoubleClickSelects write SetDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read GetShowSizeDrag write SetShowSizeDrag default False;
  end;

  { TSynCompletion }
  {Esta clase es la que permite "integrar" al editor para procesar el atajo "Ctrl+Espacio" }
  TSynCompletion = class(TSynBaseCompletion)
  private
    FShortCut: TShortCut;
    FExecCommandID: TSynEditorCommand;
    FOnCodeCompletion: TCodeCompletionEvent;
    procedure Validate(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure UTF8KeyPress(Sender: TObject; var Key: TUTF8Char); // called by the form
  protected
    procedure OnFormPaint(Sender: TObject);
    procedure SetEditor(const Value: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure SetShortCut(Value: TShortCut);
    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
  public
    constructor Create(AOwner: TComponent); override;
    function EditorsCount: integer; deprecated; // use EditorCount
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property OnCodeCompletion: TCodeCompletionEvent
      read FOnCodeCompletion write FOnCodeCompletion;
    property ExecCommandID: TSynEditorCommand read FExecCommandID write FExecCommandID;
    property Editor;
  end;

procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);

const
  ecSynCompletionExecute     = ecPluginFirst +  0;
  ecSynAutoCompletionExecute = ecPluginFirst +  1;

  ecSynCompletionCount = 2;

implementation
var
  KeyOffset: integer;

{ TSynBaseCompletionFormSizeDrag }

constructor TSynBaseCompletionFormSizeDrag.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMouseDownPos.y := -1;
end;

procedure TSynBaseCompletionFormSizeDrag.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMouseDownPos.x := x + Left;
  FMouseDownPos.y := y + Top;
  FMouseLastPos.x := x + Left;
  FMouseLastPos.y := y + Top;
  FWinSize.x := TSynBaseCompletionForm(Owner).Width;
  FWinSize.y := TSynBaseCompletionForm(Owner).Height;
  TSynBaseCompletionForm(Owner).IncHintLock;
  MouseCapture := True;
end;

procedure TSynBaseCompletionFormSizeDrag.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: TSynBaseCompletionForm;
begin
  inherited MouseMove(Shift, X, Y);
  x := x + Left;
  y := y + Top;
  if (FMouseDownPos.y < 0) or
     ((FMouseLastPos.x = x) and (FMouseLastPos.y = y))
  then
    exit;
  FMouseLastPos.x := x;
  FMouseLastPos.y := y;

  F := TSynBaseCompletionForm(Owner);
  F.Width :=
    Max(FWinSize.x + x - FMouseDownPos.x, 100);
  F.NbLinesInWindow :=
    Max((FWinSize.y + y - FMouseDownPos.y) div F.FontHeight, 3);
end;

procedure TSynBaseCompletionFormSizeDrag.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseDownPos.y := -1;
  MouseCapture := False;
  TSynBaseCompletionForm(Owner).DecHintLock;

  if (FWinSize.x <> TSynBaseCompletionForm(Owner).Width) or
     (FWinSize.y <> TSynBaseCompletionForm(Owner).Height)
  then
    TSynBaseCompletionForm(Owner).DoOnDragResize(Owner);
end;

procedure TSynBaseCompletionFormSizeDrag.Paint;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clBtnShadow;
  Canvas.MoveTo(ClientRect.Right-2, ClientRect.Bottom-1);
  Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-2);
  Canvas.MoveTo(ClientRect.Right-5, ClientRect.Bottom-1);
  Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-5);
  Canvas.MoveTo(ClientRect.Right-8, ClientRect.Bottom-1);
  Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-8);
end;

{ TSynBaseCompletionForm }

constructor TSynBaseCompletionForm.Create(AOwner: Tcomponent);
begin
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FResizeLock := 1; // prevent DoResize (on Handle Creation) do reset LinesInWindow
  FDoubleClickSelects := True;
  FHintLock := 0;
  BeginFormUpdate;
  KeyPreview:= True;
  // we have no resource => must be constructed using CreateNew
  inherited CreateNew(AOwner, 1);
  FItemList := TStringList.Create;
  BorderStyle := bsNone;
  FormStyle := fsSystemStayOnTop;
  Scroll := TScrollBar.Create(self);
  Scroll.Kind := sbVertical;
  {$IFNDEF SYN_LAZARUS}
//  Scroll.ParentCtl3D := False;
  {$ENDIF}
  Scroll.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollChange;
  Scroll.Parent := Self;
  Scroll.OnEnter := {$IFDEF FPC}@{$ENDIF}ScrollGetFocus;
  Scroll.OnScroll := {$IFDEF FPC}@{$ENDIF}ScrollScroll;
  Scroll.TabStop := False;
  Scroll.Visible := True;
  //Scroll.Align:=alRight;

  SizeDrag := TSynBaseCompletionFormSizeDrag.Create(Self);
  SizeDrag.Parent := Self;
  SizeDrag.BevelInner := bvNone;
  SizeDrag.BevelOuter := bvNone;
  SizeDrag.Caption := '';
  SizeDrag.AutoSize := False;
  SizeDrag.BorderStyle := bsNone;
  SizeDrag.Anchors := [akBottom, akRight, akLeft];
  SizeDrag.AnchorSideLeft.Side := asrTop;
  SizeDrag.AnchorSideLeft.Control := Scroll;
  SizeDrag.AnchorSideRight.Side := asrBottom;
  SizeDrag.AnchorSideRight.Control := Self;
  SizeDrag.AnchorSideBottom.Side := asrBottom;
  SizeDrag.AnchorSideBottom.Control := Self;
  SizeDrag.Height := Max(7, abs(Font.Height) * 2 div 3);
  SizeDrag.Cursor := crSizeNWSE;
  SizeDrag.Visible := False;

  SizeDrag.OnKeyPress:=@SDKeyPress;
  SizeDrag.OnKeyDown:=@SDKeyDown;
  SizeDrag.OnUTF8KeyPress:=@SDUtf8KeyPress;

  Scroll.Anchors:=[akTop,akRight, akBottom];
  Scroll.AnchorSide[akTop].Side := asrTop;
  Scroll.AnchorSide[akTop].Control := self;
  Scroll.AnchorSide[akRight].Side := asrBottom;
  Scroll.AnchorSide[akRight].Control := Self;
  Scroll.AnchorSide[akBottom].Side := asrTop;
  Scroll.AnchorSide[akBottom].Control := SizeDrag;

  DrawBorderWidth := 1;
  FTextColor:=clBlack;
  FTextSelectedColor:=clWhite;
  Caption:='Completion';
  Color:=clNone;
  FBackgroundColor:=clWhite;
  FHint := TSynBaseCompletionHint.Create(Self);
  FHint.FormStyle := fsSystemStayOnTop;
  FHintTimer := TTimer.Create(nil);
  FHintTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnHintTimer;
  FHintTimer.Interval := 0;
  FLongLineHintTime := 0;
  FLongLineHintType := sclpExtendRightOnly;
  Visible := false;
  ClSelect := clHighlight;
  TStringList(FItemList).OnChange := {$IFDEF FPC}@{$ENDIF}StringListChange;
  FNbLinesInWindow := 6;
  FontChanged(Font);
  ShowHint := False;
  EndFormUpdate;
  FResizeLock := 0;
end;

destructor TSynBaseCompletionForm.Destroy;
begin
  UnRegisterHandlers;
  FreeAndNil(Scroll);
  FreeAndNil(SizeDrag);
  FItemList.Free;
  FHintTimer.Free;
  FHint.Free;
  inherited destroy;
end;

procedure TSynBaseCompletionForm.Deactivate;
begin
  // completion box lost focus
  // this can happen when a hint window is clicked => ToDo
  Visible := False;
  FHintTimer.Enabled := False;
  FHint.Visible := False;
  if (FCurrentEditor<>nil) and (TCustomSynEdit(fCurrentEditor).HandleAllocated)
  then
    SetCaretRespondToFocus(TCustomSynEdit(FCurrentEditor).Handle,true);
end;

procedure TSynBaseCompletionForm.ShowItemHint(AIndex: Integer);
var
  R: TRect;
  P: TPoint;
  M: TMonitor;
  MinLeft: Integer;
begin
  FHintTimer.Enabled := False;
  if Visible and (AIndex >= 0) and (AIndex < ItemList.Count) and
     (FLongLineHintType <> sclpNone) and
     (FHintLock = 0)
  then begin
    // CalcHintRect uses the current index
    FHint.Index := AIndex;
    // calculate the size
    R := FHint.CalcHintRect(Monitor.Width, ItemList[AIndex], nil);

    if (R.Right <= Scroll.Left) then begin
      FHint.Hide;
      Exit;
    end;

    // calculate the position
    M := Monitor;
    P := ClientToScreen(Point(0, (AIndex - Scroll.Position) * FFontHeight));
    case FLongLineHintType of
      sclpExtendHalfLeft:      MinLeft := Max(M.Left,  P.X - ClientWidth div 2); // ClientWidth may be too much, if part of the ClientWidth extends to another screen.
      sclpExtendUnlimitedLeft: MinLeft := M.Left;
      else                     MinLeft := P.X;
    end;
    P.X := Max(MinLeft,
               Min(P.X,                              // Start at drop-down Left boundary
                   M.Left + M.Width - R.Right - 1    // Or push left, if hitting right Monitor border
                  )
              );
    P.Y := Max(M.Top, Min(P.Y, M.Top + M.Height - R.Bottom - 1));
    // actually Width and Height
    R.Right := Min(r.Right, M.Left + M.Width - 1 - P.X);
    R.Bottom := Min(r.Bottom, M.Top + M.Height - 1 - P.Y);

    FHint.DisplayRect := Bounds(P.X, P.Y, R.Right, R.Bottom);

    if (not FHint.IsVisible) and (FLongLineHintTime > 0) then
      FHintTimer.Enabled := True
    else
      OnHintTimer(nil);
  end
  else begin
    FHint.Hide;
  end;
end;

procedure TSynBaseCompletionForm.OnHintTimer(Sender: TObject);
begin
  FHintTimer.Enabled := False;
  FHint.ActivateHint(FHint.DisplayRect, ItemList[FHint.Index]);
  FHint.Invalidate;
end;


procedure TSynBaseCompletionForm.KeyDown(var Key: Word; Shift: TShiftState);
{Procesa el evento KeyDown de la lista. Algunas teclas se procesan como
 comandos para la lista, otras se pasan al editor.}
var
  i: integer;
  Handled: Boolean;
begin
  //debugln('TSynBaseCompletionForm.KeyDown A Key=',dbgs(Key));
  inherited KeyDown(Key,Shift);
  if Key=VK_UNKNOWN then exit;        //validación.
  if CurrentEditor = nil then exit;   //protección.
  Handled:=true;
  case Key of
// added the VK_XXX codes to make it more readable / maintainable
    VK_RETURN:
      if Shift = [] then begin  //sin control ni shift
        if Assigned(OnValidate) then OnValidate(Self, '', Shift);
      end else begin  //envía al editor
        CurrentEditor.CommandProcessor(ecLineEnd, #0, nil);
        Deactivate;  //desactiva
      end;
    VK_ESCAPE:
      Deactivate;  //desactiva
    // I do not think there is a worst way to do this, but laziness rules :-)
    VK_PRIOR:
      for i := 1 to NbLinesInWindow do
        SelectPrec;
    VK_NEXT:
      for i := 1 to NbLinesInWindow do
        SelectNext;
    VK_END:
      if Shift = [] then begin  //envía al editor
          CurrentEditor.CommandProcessor(ecLineEnd, #0, nil);
          Deactivate;  //desactiva
      end;
    VK_HOME:
      if Shift = [] then begin  //envía al editor
          CurrentEditor.CommandProcessor(ecLineStart, #0, nil);
      end;
    VK_UP:
      if ssCtrl in Shift then
        Position := 0
      else
        SelectPrec;
    VK_DOWN:
      if ssCtrl in Shift then
        Position := ItemList.count - 1
      else
        SelectNext;
    VK_BACK:
      if Shift = [] then begin  //envía al editor
          CurrentEditor.CommandProcessor(ecDeleteLastChar, #0, nil);
          // Genera evento
          if Assigned(FOnTextModified) then begin
            FOnTextModified(nil, '', Shift);
          end;
      end;
    VK_DELETE:
      if Shift = [] then begin  //envía al editor
          CurrentEditor.CommandProcessor(ecDeleteChar, #0, nil);
          // Genera evento
          if Assigned(FOnTextModified) then begin
            FOnTextModified(nil, '', Shift);
          end;
      end;
    VK_TAB:
      begin{ TODO : Da error cuando se hace TAB y no hay ningún item seleccionado (que de verdad pasa) }
        OnValidate(Self, '', Shift);  //selecciona
      end;
    VK_LEFT:
      if Shift = [] then begin  //envía al editor
        CurrentEditor.CommandProcessor(ecLeft, #0, nil);
      end else if Shift = [ssShift] then begin
        CurrentEditor.CommandProcessor(ecSelLeft, #0, nil);
        Deactivate;  //desactiva
      end else if Shift = [ssCtrl] then begin
        CurrentEditor.CommandProcessor(ecWordLeft, #0, nil);
        Deactivate;  //desactiva
      end else if Shift = [ssShift,ssCtrl] then begin
        CurrentEditor.CommandProcessor(ecSelWordLeft, #0, nil);
        Deactivate;  //desactiva
      end;
    VK_Right:
      if Shift = [] then begin  //envía al editor
        CurrentEditor.CommandProcessor(ecRight, #0, nil);
        Deactivate;  //desactiva
      end else if Shift = [ssShift] then begin
        CurrentEditor.CommandProcessor(ecSelRight, #0, nil);
        Deactivate;  //desactiva
      end else if Shift = [ssCtrl] then begin
        CurrentEditor.CommandProcessor(ecWordRight, #0, nil);
        Deactivate;  //desactiva
      end else if Shift = [ssShift,ssCtrl] then begin
        CurrentEditor.CommandProcessor(ecSelWordRight, #0, nil);
        Deactivate;  //desactiva
      end;
  else
    Handled:=false;  //marca que no se ha procesado
  end;
  if Handled then Key:=VK_UNKNOWN;   //se procesó tecla
  Invalidate;
  //verifica si debe desaparecer la ventana, por mover el cursor a una posición anterior
  if CompareCarets(Pos0, CurrentEditor.CaretXY) < 0 then Deactivate;
end;

procedure TSynBaseCompletionForm.UTF8KeyPress(var UTF8Key: TUTF8Char);
{Recibe la pulsación de las teclas cuando la lista se encuentra visible}
begin
  if Length(UTF8Key) = 0 then exit;  //validación
  if UTF8Key=#8 then
  begin
    // backspace
  end else
  begin
    if UTF8Key[1] in [#10,#13,' ' .. #127] then begin
      //caracteres que pasan al editor
      if UTF8Key[1] in [' ','+','-','*','/','.',',',#10,#13] then  //estos caracteres cierran la  búsqueda
         Deactivate;
      //envía al editor
      if Assigned(OnUTF8KeyPress) then OnUTF8KeyPress(Self, UTF8Key);
      UTF8Key := '';
      // Genera evento
      if Assigned(FOnTextModified) then begin
        FOnTextModified(nil, UTF8Key, []);
      end;
    end
    else begin
      // non identifier character

      // if it is special key then eat it
      if (Length(UTF8Key) = 1) and (UTF8Key[1] < #32) then
      begin

      end
      else
      if Assigned(OnValidate) then OnValidate(Self, UTF8Key, []);
      UTF8Key := '';
    end;
  end;
end;

procedure TSynBaseCompletionForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  y := (y - 1) div FFontHeight;
  Position := Scroll.Position + y;
  if DoubleClickSelects and (ssDouble in Shift) and (Position = OldPosition) and
     Assigned(OnValidate)
  then
    OnValidate(Self, '', Shift);
end;

procedure TSynBaseCompletionForm.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if ((Scroll.Visible) and (x > Scroll.Left)) or
     (y  < DrawBorderWidth) or (y >= ClientHeight - DrawBorderWidth)
  then
    exit;
  Y := (Y - DrawBorderWidth) div FFontHeight;
  ShowItemHint(Scroll.Position + Y);
end;

procedure TSynBaseCompletionForm.Paint;
var
  i: integer;

  function Min(a, b: integer): integer;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;

begin
//Writeln('[TSynBaseCompletionForm.Paint]');

  // update scroll bar
  Scroll.Enabled := ItemList.Count > NbLinesInWindow;
  Scroll.Visible := (ItemList.Count > NbLinesInWindow) or ShowSizeDrag;

  if Scroll.Visible and Scroll.Enabled then
  begin
    Scroll.Max := ItemList.Count - 1;
    Scroll.LargeChange := NbLinesInWindow;
    Scroll.PageSize := NbLinesInWindow;
  end
  else
  begin
    Scroll.PageSize := 1;
    Scroll.Max := 0;
  end;

  //DebugLn(['TSynBaseCompletionForm.Paint NbLinesInWindow=',NbLinesInWindow,' ItemList.Count=',ItemList.Count]);
  for i := 0 to min(NbLinesInWindow - 1, ItemList.Count - Scroll.Position - 1) do
  begin
    if i + Scroll.Position = Position then
    begin
      Canvas.Brush.Color := clSelect;
      Canvas.Pen.Color := clSelect;
      Canvas.Rectangle(DrawBorderWidth, DrawBorderWidth+(FFontHeight * i),
                      Width-2*DrawBorderWidth, (FFontHeight * (i + 1))+1);
      Canvas.Pen.Color := clBlack;
      Canvas.Font.Color := TextSelectedColor;
      Hint := ItemList[Position];
    end
    else
    begin
      Canvas.Brush.Color := BackgroundColor;
      Canvas.Font.Color := TextColor;
      Canvas.FillRect(Rect(DrawBorderWidth, DrawBorderWidth+(FFontHeight * i),
                           Width-2*DrawBorderWidth, (FFontHeight * (i + 1))+1));
    end;

    //DebugLn(['TSynBaseCompletionForm.Paint ',i,' ',ItemList[Scroll.Position + i]]);
    Canvas.TextOut(DrawBorderWidth+2, DrawBorderWidth+FFontHeight * i, ItemList[Scroll.Position + i]);
  end;
  // paint the rest of the background
  if NbLinesInWindow > ItemList.Count - Scroll.Position then
  begin
    Canvas.brush.color := color;
    i:=(FFontHeight * ItemList.Count)+1;
    Canvas.FillRect(Rect(0, i, Width, Height));
  end;
  // draw a rectangle around the window
  if DrawBorderWidth > 0 then begin
    Canvas.Pen.Color := TextColor;
    Canvas.Pen.Width := DrawBorderWidth;
    Canvas.Moveto(0, 0);
    Canvas.LineTo(Width - 1, 0);
    Canvas.LineTo(Width - 1, Height - 1);
    Canvas.LineTo(0, Height - 1);
    Canvas.LineTo(0, 0);
  end;
end;

function TSynBaseCompletionForm.Focused: Boolean;
begin
  Result:=(inherited Focused) or SizeDrag.Focused;
end;

procedure TSynBaseCompletionForm.AppDeactivated(Sender: TObject);
begin
  Deactivate;
end;

procedure TSynBaseCompletionForm.ScrollChange(Sender: TObject);
begin
  if Position < Scroll.Position then
    Position := Scroll.Position
  else
  if Position > Scroll.Position + NbLinesInWindow - 1 then
    Position := Scroll.Position + NbLinesInWindow - 1;
  Invalidate;
end;

procedure TSynBaseCompletionForm.ScrollGetFocus(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynBaseCompletionForm.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if ScrollPos > (Scroll.Max - Scroll.PageSize) + 1 then
    ScrollPos := Scroll.Max - Scroll.PageSize + 1;
  FHint.Hide;
  ShowItemHint(Position);
end;

procedure TSynBaseCompletionForm.SelectNext;
begin
  if Position < ItemList.Count - 1 then
    Position := Position + 1;
end;

procedure TSynBaseCompletionForm.SelectPrec;
begin
  if Position > 0 then
    Position := Position - 1;
end;

procedure TSynBaseCompletionForm.DoEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  KeyDown(Key, Shift);
  Key := 0;
end;

procedure TSynBaseCompletionForm.DoEditorKeyPress(Sender: TObject; var Key: char);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  KeyPress(Key);
  Key := #0;
end;

procedure TSynBaseCompletionForm.DoEditorUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  UTF8KeyPress(UTF8Key);
  UTF8Key := '';
end;

procedure TSynBaseCompletionForm.SDKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(key,shift);
end;

procedure TSynBaseCompletionForm.SDKeyPress(Sender: TObject; var Key: char);
begin
  KeyPress(key);
end;

procedure TSynBaseCompletionForm.SDUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

procedure TSynBaseCompletionForm.DoOnResize;
begin
  inherited DoOnResize;
  if ([csLoading,csDestroying]*ComponentState<>[]) or (Scroll=nil) then exit;
  if (fFontHeight > 0) and (FResizeLock = 0) then
  begin
    FNbLinesInWindow := (Height-2*DrawBorderWidth+(fFontHeight-1)) div fFontHeight;
    Invalidate;
  end;
end;

procedure TSynBaseCompletionForm.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Color := AValue;
    FHint.Color := AValue;
  end;
end;

procedure TSynBaseCompletionForm.FontChanged(Sender: TObject);
var
  TextMetric: TTextMetric;
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    inherited;
    FillChar(TextMetric{%H-},SizeOf(TextMetric),0);
    GetTextMetrics(Canvas.Handle, TextMetric);
    FFontHeight := TextMetric.tmHeight+2;
    SetNblinesInWindow(FNbLinesInWindow);
    SizeDrag.Height := Max(7, FFontHeight * 2 div 3);
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynBaseCompletionForm.WMMouseWheel(var Msg: TLMMouseEvent);
const
  WHEEL_DELTA = 120;
var
  WheelClicks: Integer;
begin
  Inc(FMouseWheelAccumulator, Msg.WheelDelta);
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator - WheelClicks * WHEEL_DELTA;
  WheelClicks := WheelClicks * Mouse.WheelScrollLines;
  Scroll.Position := Max(0, Min(FItemList.Count - NbLinesInWindow, Scroll.Position - WheelClicks));
end;

procedure TSynBaseCompletionForm.SetLongLineHintTime(const AValue: Integer);
begin
  if FLongLineHintTime = AValue then exit;
  FLongLineHintTime := AValue;
  FHintTimer.Interval := AValue;
end;

procedure TSynBaseCompletionForm.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin

end;

procedure TSynBaseCompletionForm.SetShowSizeDrag(const AValue: Boolean);
begin
  if FShowSizeDrag = AValue then exit;
  FShowSizeDrag := AValue;
  SizeDrag.Visible := AValue;
end;

procedure TSynBaseCompletionForm.RegisterHandlers(EditOnly: Boolean);
begin
  if FCurrentEditor <> nil then begin
    FCurrentEditor.RegisterStatusChangedHandler
    ({$IFDEF FPC}@{$ENDIF}EditorStatusChanged, [scTopLine]);
    // Catch Editor events. Some Widgetset may report keys to the editor,
    // if the user types faster, then the app can open the form
    FCurrentEditor.RegisterBeforeKeyDownHandler(@DoEditorKeyDown);
    FCurrentEditor.RegisterBeforeKeyPressHandler(@DoEditorKeyPress);
    FCurrentEditor.RegisterBeforeUtf8KeyPressHandler(@DoEditorUtf8KeyPress);
  end;
  if not EditOnly then
    Application.AddOnDeactivateHandler({$IFDEF FPC}@{$ENDIF}AppDeactivated);
end;

procedure TSynBaseCompletionForm.UnRegisterHandlers(EditOnly: Boolean);
begin
  if FCurrentEditor <> nil then begin
    FCurrentEditor.UnRegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}EditorStatusChanged);
    FCurrentEditor.UnregisterBeforeKeyDownHandler(@DoEditorKeyDown);
    FCurrentEditor.UnregisterBeforeKeyPressHandler(@DoEditorKeyPress);
    FCurrentEditor.UnregisterBeforeUtf8KeyPressHandler(@DoEditorUtf8KeyPress);
  end;
  if not EditOnly then
    Application.RemoveOnDeactivateHandler({$IFDEF FPC}@{$ENDIF}AppDeactivated);
end;

procedure TSynBaseCompletionForm.SetCurrentEditor(const AValue: TCustomSynEdit);
begin
  if FCurrentEditor = AValue then exit;
  UnRegisterHandlers(True);
  FCurrentEditor := AValue;
  if Visible then
    RegisterHandlers(True);
end;

procedure TSynBaseCompletionForm.SetDrawBorderWidth(const AValue: Integer);
begin
  if FDrawBorderWidth = AValue then exit;
  FDrawBorderWidth := AValue;
  NbLinesInWindow := NbLinesInWindow;
  Scroll.BorderSpacing.Top := FDrawBorderWidth;
  Scroll.BorderSpacing.Right := FDrawBorderWidth;
  if SizeDrag.Visible then
    Scroll.BorderSpacing.Bottom := 0
  else
    Scroll.BorderSpacing.Bottom := FDrawBorderWidth;
  SizeDrag.BorderSpacing.Right := FDrawBorderWidth;
  SizeDrag.BorderSpacing.Bottom := FDrawBorderWidth;
end;

procedure TSynBaseCompletionForm.SetVisible(Value: Boolean);
begin
  if Visible = Value then exit;;

  if Value then
    RegisterHandlers
  else
    UnRegisterHandlers;

  inherited SetVisible(Value);
end;

procedure TSynBaseCompletionForm.IncHintLock;
begin
  inc(FHintLock);
  FHint.Hide
end;

procedure TSynBaseCompletionForm.DecHintLock;
begin
  dec(FHintLock);
  if FHintLock = 0 then
    ShowItemHint(Position);
end;

procedure TSynBaseCompletionForm.DoOnDragResize(Sender: TObject);
begin
  if assigned(FOnDragResized) then
    FOnDragResized(Sender);
end;

procedure TSynBaseCompletionForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  if Position>=FItemList.Count then Position:=-1;
  Invalidate;
end;

procedure TSynBaseCompletionForm.SetNbLinesInWindow(
  const Value: Integer);
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    FNbLinesInWindow := Value;
    Height := fFontHeight * NbLinesInWindow + 2*DrawBorderWidth;
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynBaseCompletionForm.SetPosition(const Value: Integer);
begin
  if Value < ItemList.Count then begin
    if FPosition <> Value then begin
      FPosition := Value;
      if Position < Scroll.Position then
        Scroll.Position := Position
      else if Scroll.Position < Position - NbLinesInWindow + 1 then
        Scroll.Position := Position - NbLinesInWindow + 1;
      Invalidate;
    end;
  end;
  if Showing then
    ShowItemHint(Position);
end;

procedure TSynBaseCompletionForm.StringListChange(Sender: TObject);
begin
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  Position := Position;
end;

{ TSynBaseCompletion }

constructor TSynBaseCompletion.Create(AOwner: TComponent);
begin
  FWidth := 262;
  inherited Create(AOwner);
  Form := TSynBaseCompletionForm.Create(nil); // Do not create with owner, or the designer will make it visible
  Form.Width := FWidth;
end;

destructor TSynBaseCompletion.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Form);
end;

procedure TSynBaseCompletion.Execute(s: string; x, y: integer);
//Rutina básica para mostrar la ventana principal del completado.
var
  CurSynEdit: TCustomSynEdit;
begin
  //Todo: This is dangerous, if other plugins also change/changed the flag.
  FAddedPersistentCaret := False;

  if Assigned(OnExecute) then
    OnExecute(Self);

  if (ItemList.Count=0) then exit;

  if (Form.CurrentEditor is TCustomSynEdit) then begin
    CurSynEdit:=TCustomSynEdit(Form.CurrentEditor);
    FAddedPersistentCaret := not(eoPersistentCaret in CurSynEdit.Options);
    if FAddedPersistentCaret then
      CurSynEdit.Options:=CurSynEdit.Options+[eoPersistentCaret];
    //gurda la posición del cursor
    Form.Pos0 := CurSynEdit.CaretXY;
  end;
  Form.SetBounds(x,y,Form.Width,Form.Height);
  Form.Show;
  Form.Position := 0;
end;

procedure TSynBaseCompletion.Execute(s: string; TopLeft: TPoint);
begin
  Execute(s, TopLeft.x, TopLeft.y);
end;

procedure TSynBaseCompletion.Execute(s: string; TokenRect: TRect);
var
  SpaceBelow, SpaceAbove: Integer;
  Mon: TMonitor;
begin
  Mon := Screen.MonitorFromPoint(TokenRect.TopLeft);
  if Mon <> nil then
    TokenRect.Left := Min(TokenRect.Left, Mon.Left + Mon.Width - Form.Width);

  SpaceBelow := Mon.Height - TokenRect.Bottom;
  SpaceAbove := TokenRect.Top - Mon.Top;
  if Form.Height < SpaceBelow then
    Execute(s, TokenRect.Left, TokenRect.Bottom)
  else
  if Form.Height < SpaceAbove then
    Execute(s, TokenRect.Left, TokenRect.Top - Form.Height)
  else
  begin
    if SpaceBelow > SpaceAbove then begin
      Form.NbLinesInWindow := Max(SpaceBelow div Form.FontHeight, 3); // temporary height
    Execute(s, TokenRect.Left, TokenRect.Bottom);
    end else begin
      Form.NbLinesInWindow := Max(SpaceAbove div Form.FontHeight, 3); // temporary height
      Execute(s, TokenRect.Left, TokenRect.Top - Form.Height);
    end;;
  end;
end;

procedure TSynBaseCompletion.Deactivate;
var
  CurSynEdit: TCustomSynEdit;
begin
  if FAddedPersistentCaret and (Form<>nil) and (Form.CurrentEditor is TCustomSynEdit) then begin
    CurSynEdit:=TCustomSynEdit(Form.CurrentEditor);
    CurSynEdit.Options:=CurSynEdit.Options-[eoPersistentCaret];
  end;
  if Assigned(Form) then Form.Deactivate;
end;

function TSynBaseCompletion.GetOnUTF8KeyPress: TUTF8KeyPressEvent;
begin
  Result:=Form.OnUTF8KeyPress;
end;
procedure TSynBaseCompletion.SetOnUTF8KeyPress( const AValue: TUTF8KeyPressEvent);
begin
  Form.OnUTF8KeyPress:=AValue;
end;
function TSynBaseCompletion.GetFontHeight:integer;
begin
  Result:=Form.FontHeight;
end;
function TSynBaseCompletion.GetOnTextModified:TValidateEvent;
begin
  Result:=Form.OnTextModified;
end;
procedure TSynBaseCompletion.SetOnTextModified(NewValue :TValidateEvent);
begin
  Form.OnTextModified:=NewValue;
end;
function TSynBaseCompletion.GetItemList: TStrings;
begin
  result := Form.ItemList;
end;
function TSynBaseCompletion.GetNbLinesInWindow: Integer;
begin
  Result := Form.NbLinesInWindow;
end;
function TSynBaseCompletion.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;
function TSynBaseCompletion.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;
function TSynBaseCompletion.GetPosition: Integer;
begin
  Result := Form.Position;
end;
procedure TSynBaseCompletion.SetDoubleClickSelects(const AValue: Boolean);
begin
  Form.DoubleClickSelects := AValue;
end;
procedure TSynBaseCompletion.SetItemList(const Value: TStrings);
begin
  form.ItemList := Value;
end;
procedure TSynBaseCompletion.SetLongLineHintTime(const AValue: Integer);
begin
  Form.LongLineHintTime := AValue;
end;
procedure TSynBaseCompletion.SetLongLineHintType(const AValue: TSynCompletionLongHintType);
begin
  Form.LongLineHintType := AValue;
end;
procedure TSynBaseCompletion.SetNbLinesInWindow(const Value: Integer);
begin
  form.NbLinesInWindow := Value;
end;
procedure TSynBaseCompletion.SetOnKeyDown(const AValue: TKeyEvent);
begin
  Form.OnKeyDown:=AValue;
end;
procedure TSynBaseCompletion.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  form.OnKeyPress := Value;
end;
procedure TSynBaseCompletion.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;
procedure TSynBaseCompletion.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;
function TSynBaseCompletion.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;
function TSynBaseCompletion.GetDoubleClickSelects: Boolean;
begin
  Result := Form.DoubleClickSelects;
end;
function TSynBaseCompletion.GetLongLineHintTime: Integer;
begin
  Result := Form.LongLineHintTime;
end;
function TSynBaseCompletion.GetLongLineHintType: TSynCompletionLongHintType;
begin
  Result := Form.LongLineHintType;
end;
function TSynBaseCompletion.GetOnKeyDown: TKeyEvent;
begin
  Result:=Form.OnKeyDown;
end;
function TSynBaseCompletion.GetShowSizeDrag: Boolean;
begin
  Result := Form.ShowSizeDrag;
end;
procedure TSynBaseCompletion.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;
procedure TSynBaseCompletion.SetShowSizeDrag(const AValue: Boolean);
begin
  Form.ShowSizeDrag := AValue;
end;
procedure TSynBaseCompletion.SetWidth(Value: Integer);
begin
  FWidth := Value;
  Form.Width := FWidth;
  Form.SetNbLinesInWindow(Form.FNbLinesInWindow);
end;
function TSynBaseCompletion.IsActive: boolean;
begin
  Result:=(Form<>nil) and (Form.Visible);
end;
function TSynBaseCompletion.TheForm: TSynBaseCompletionForm;
begin
  Result:=Form;
end;
procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);
var
  i: integer;
  OldFontColor: TColor;
  OldFontStyle: TFontStyles;
begin
  OldFontColor:=c.Font.Color;
  OldFontStyle:=c.Font.Style;
  c.Font.Style:=[];
  c.Font.Color:=clBlack;
  try
    i := 1;
    while i <= Length(s) do
      case s[i] of
        #1: begin
            C.Font.Color := (Ord(s[i + 3]) shl 8 + Ord(s[i + 2])) shl 8 + Ord(s[i + 1]);
            inc(i, 4);
          end;
        #2: begin
            C.Font.Color := (Ord(s[i + 3]) shl 8 + Ord(s[i + 2])) shl 8 + Ord(s[i + 1]);
            inc(i, 4);
          end;
        #3: begin
            case s[i + 1] of
              'B': c.Font.Style := c.Font.Style + [fsBold];
              'b': c.Font.Style := c.Font.Style - [fsBold];
              'U': c.Font.Style := c.Font.Style + [fsUnderline];
              'u': c.Font.Style := c.Font.Style - [fsUnderline];
              'I': c.Font.Style := c.Font.Style + [fsItalic];
              'i': c.Font.Style := c.Font.Style - [fsItalic];
            end;
            inc(i, 2);
          end;
      else
        C.TextOut(x, y, s[i]);
        x := x + c.TextWidth(s[i]);
        inc(i);
      end;
  except
  end;
  c.Font.Color:=OldFontColor;
  c.Font.Style:=OldFontStyle;
end;

{ TSynCompletion }

procedure TSynCompletion.OnFormPaint(Sender: TObject);
begin

end;

procedure TSynCompletion.Validate(Sender: TObject; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  F: TSynBaseCompletionForm;
  Value, CurLine: string;
  NewBlockBegin, NewBlockEnd: TPoint;
  LogCaret: TPoint;
begin
  //debugln('TSynCompletion.Validate ',dbgsName(Sender),' ',dbgs(Shift),' Position=',dbgs(Position));
  F := Sender as TSynBaseCompletionForm;
  // Note: Form.Visible can be false, for example when completion only contains one item
  if F.CurrentEditor is TCustomSynEdit then
    with TCustomSynEdit(F.CurrentEditor) do begin
      BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSynCompletion.Validate'){$ENDIF};
      BeginUpdate;
      try
        LogCaret := LogicalCaretXY;
        NewBlockBegin:=LogCaret;
        CurLine:=Lines[NewBlockBegin.Y - 1];
        while (NewBlockBegin.X>1) and (NewBlockBegin.X-1<=length(CurLine))
        and (CurLine[NewBlockBegin.X-1] in ['a'..'z','A'..'Z','0'..'9','_','$']) do
          dec(NewBlockBegin.X);
        //BlockBegin:=NewBlockBegin;
        if ssShift in Shift then begin
          // replace only prefix
          NewBlockEnd := LogCaret;
        end else begin
          // replace the whole word
          NewBlockEnd := LogCaret;
          CurLine:=Lines[NewBlockEnd.Y - 1];
          while (NewBlockEnd.X<=length(CurLine))
          and (CurLine[NewBlockEnd.X] in ['a'..'z','A'..'Z','0'..'9','_']) do
            inc(NewBlockEnd.X);
        end;
        //debugln('TSynCompletion.Validate B Position=',dbgs(Position));
        if Position>=0 then begin
          if Assigned(FOnCodeCompletion) then
          begin
            Value := ItemList[Position];
            FOnCodeCompletion(Value, TextBetweenPoints[NewBlockBegin, NewBlockEnd],
                              NewBlockBegin, NewBlockEnd, KeyChar, Shift);
            if (CompareCarets(NewBlockBegin, NewBlockEnd) <> 0) or (Value <> '') then
            begin
              TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := Value;
              TCustomSynEdit(F.CurrentEditor).SetFocus;
            end;
          end else begin
            TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := ItemList[Position];
            TCustomSynEdit(F.CurrentEditor).SetFocus;
          end;
        end;
      finally
        EndUpdate;
        EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSynCompletion.Validate'){$ENDIF};
      end;
    end;
end;

procedure TSynCompletion.UTF8KeyPress(Sender: TObject; var Key: TUTF8Char);
var
  F: TSynBaseCompletionForm;
begin
  //debugln('TSynCompletion.UTF8KeyPress Key="',DbgStr(Key),'"');
  F := Sender as TSynBaseCompletionForm;
  if F.CurrentEditor <> nil then begin
    with F.CurrentEditor as TCustomSynEdit do begin
      CommandProcessor(ecChar, Key, nil);
    end;
  end;
end;

constructor TSynCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnUTF8KeyPress := @UTF8KeyPress;
  Form.OnValidate := {$IFDEF FPC}@{$ENDIF}Validate;
  Form.OnPaint:=@OnFormPaint;
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
  FExecCommandID := KeyOffset + ecSynCompletionExecute;
end;

procedure TSynCompletion.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletion.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
  var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean;
  var ComboKeyStrokes: TSynEditKeyStrokes);
var
  i: integer;
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  if (Code = VK_UNKNOWN) or Handled or FinishComboOnly or (FExecCommandID = ecNone) then exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);
    if (SState = ShortCutShift) and (Code = ShortCutKey) then begin
      Command := FExecCommandID;
      Handled := True;
    end;
  end;

end;

procedure TSynCompletion.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
{Este es el punto de ejecución por el atajo de teclado del editor <Ctrl> + <Espacio> }
var
  p: TPoint;
  i: integer;
begin
  if Handled or (Command <> FExecCommandID) then
    exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    with sender as TCustomSynEdit do begin
      if not ReadOnly then begin
        p := ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight + 1));
        Editor := Sender as TCustomSynEdit; // Will set Form.SetCurrentEditor
        Execute('', p.x, p.y);
        Handled := True;
      end;
    end;
  end;

end;

procedure TSynCompletion.DoEditorAdded(AValue: TCustomSynEdit);
//Registra esta clase para que sea usada por el editor.
begin
  inherited DoEditorAdded(AValue);

  AValue.RegisterCommandHandler(@ProcessSynCommand, nil);
  AValue.RegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynCompletion.DoEditorRemoving(AValue: TCustomSynEdit);
//De-registra esta clase.
begin
  inherited DoEditorRemoving(AValue);
  if Form.CurrentEditor = AValue then
    Form.SetCurrentEditor(nil);

  AValue.UnregisterCommandHandler(@ProcessSynCommand);
  AValue.UnRegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynCompletion.SetEditor(const Value: TCustomSynEdit);
begin
  inherited SetEditor(Value);
  Form.SetCurrentEditor(Value);
end;

function TSynCompletion.EditorsCount: integer;
begin
  result := EditorCount;
end;

{ TSynBaseCompletionHint }

procedure TSynBaseCompletionHint.Paint;
var
  R: TRect;
begin
  if FCompletionForm.Position = FIndex then
    Canvas.Brush.Color := FCompletionForm.ClSelect
  else
    Canvas.Brush.Color := Color;

  Canvas.Pen.Width := 1;
  R := ClientRect;
  Canvas.FillRect(R);
  DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT);

  Canvas.Font.Color := FCompletionForm.TextColor;

  Canvas.TextOut(2, 2, Caption);
end;

constructor TSynBaseCompletionHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Brush.Style := bsSolid;
  FCompletionForm := AOwner as TSynBaseCompletionForm;
  Color := FCompletionForm.BackgroundColor;
  AutoHide := False;
  Visible := False;
end;

function TSynBaseCompletionHint.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
begin
  Result := Rect(0, 0, Canvas.TextWidth(AHint) + 4, FCompletionForm.FontHeight);
end;

initialization
  KeyOffset := AllocatePluginKeyRange(ecSynCompletionCount, True);

end.


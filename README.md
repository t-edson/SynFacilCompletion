SynFacilCompletion
===================

Scriptable Highlighter with code-completion for the SynEdit Component of Lazarus. 

This highlighter is based on the highlighter https://github.com/t-edson/SynFacilSyn , and include all his options, and others special for defining the code-completion feature.

For to understand, how this library work, it's necessary to know first, how the SynFacilSyn library works.
 
In the XML file, it's added the label <COMPLETION> for defining a list of words for using when writing code on the editor.

Using in a program.

There are 3 units for to include  when using this library:

* SynCompletionQ.pas
* SynFacilHighlighter.pas  (version 0.9.1 or higher)
* SynFacilCompletion.pas

Only the unit 'SynFacilCompletion' is necessary to include in the section USES of the programs. The other two units, are used by 'SynCompletionFacil'.

For using the highlighter, we must create an TSynFacilCompletion object: 

```
uses ... , SynFacilCompletion;

procedure TForm1.FormShow(Sender: TObject);
begin
 //configure highlighter
  hlt := TSynFacilComplet.Create(self);  //my highlighter
  SynEdit1.Highlighter := hlt;  //optional if we are going to use SelectEditor()
  hlt.LoadFromFile('ObjectPascal.xml');  //load syntax
  hlt.SelectEditor(SynEdit1);  //assign to editor
end;
```

The completion menu will open, when pressing Ctrl+Space, but if we want to auto-open the xomplwtion mennu, when pressing a key, we must intercept the KeyUp() event, of the SynEdit:

```
procedure TForm1.edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Key, Shift);
end;
```

And the we must finalize correctly:

```
procedure TForm1.FormDestroy(Sender: TObject);
begin
  hlt.UnSelectEditor;   //release editor (only necessary if we are to call to SelectEditor(), again)
  hlt.Free;  //destroy the highlighter
end;
```

For to close the window of the completion menu, we must to call to the method:  CloseCompletionWindow().

In the XML file, we must include the list of word for to use in the completion menu. This list must be included in the label <COMPLETION>. 

The next XML file, define three words for the compeltionmenu:

```
<?xml version="1.0"?>
<Language name="Pascal" ext="pas">
  <completion>
    var
    procedure
    function 
  </completion>
</Language>
```

It's possible to use too, the list of Keywords like words for the compeltion menu:

```
<?xml version="1.0"?>
<Language name="Pascal" ext="pas">
  <completion>
    var
    procedure
    function 
  </completion>
</Language>
```

This library is still in a early state, and can change a lot in the future.


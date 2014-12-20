SynFacilCompletion 0.7
======================

Scriptable Highlighter with code-completion for the SynEdit Component of Lazarus. 

![SynFacilCompletion](http://blog.pucp.edu.pe/media/4946/20140920-synfacilcomplet1.png "Título de la imagen")

This highlighter is based on the highlighter https://github.com/t-edson/SynFacilSyn , and includes all his options, and others special for defining the code-completion feature.

This library works in the same way that the SynFacilSyn library works. It uses a XML file for to define the syntax, and additionally, the words for the completion code.
 
In the XML file, it's added the label <COMPLETION> for defining a list of words for using when writing code on the editor.

## Using in a program.

There are 2 units for to include  when using this library:

* SynFacilHighlighter.pas  (version 0.9.4 or higher)
* SynFacilCompletion.pas

Only the unit 'SynFacilCompletion' is necessary to include in the section USES of the programs. 'SynFacilHighlighter' is used by 'SynCompletionFacil' and must be accessible.

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

The completion menu will open, when pressing Ctrl+Space, but if we want to auto-open the completion mennu, when pressing a key, we must intercept the KeyUp() event, of the SynEdit:

```
procedure TForm1.edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.ed1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt.UTF8KeyPress(Sender, UTF8Key);
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

The next XML file, define three words for the completion menu:

```
<?xml version="1.0"?>
<Language name="Pascal" ext="pas">
  <completion>
    var
    procedure
    function 
  </completion>
  
  ...
  
</Language>
```

It's possible to use too, the list of Keywords (or another group) like words for the completion menu:

```
<?xml version="1.0"?>
<Language name="Pascal" ext="pas">
  <Completion> 
    <Include Attribute="Keyword"></Include>
  </Completion>
  <Identifiers>
    <Keyword>
    var
    procedure
    function 
    </Keyword>
  </Identifiers>
</Language>
```

In this way, we avoid to repeat all the list of keywords again.

This library is still in a early state of development, and could change a lot in the future.

For more information, check the documentation.


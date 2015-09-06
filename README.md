SynFacilCompletion 1.12
=======================

Scriptable Highlighter with code-completion for the SynEdit Component of Lazarus. 

![SynFacilCompletion](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2014/09/synfacilcomplet1.png "Título de la imagen")

This highlighter is based on the highlighter https://github.com/t-edson/SynFacilSyn , and includes all his options, and others special for defining the code-completion feature.

This library works in the same way that the SynFacilSyn library works. It uses a XML file to define the syntax, and additionally, the words for the completion code.
 
In the XML file, it's added the label "COMPLETION" for defining a list of words for using when writing code on the editor.

## Using in a program.

There are 3 units to include when using this library:

* SynFacilBasic.pas  (version 1.1 or higher)
* SynFacilHighlighter.pas  (version 1.1 or higher)
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

The completion menu will open, when pressing Ctrl+Space, but if we want to auto-open the completion menu, when pressing a key, we must intercept the KeyUp() event, of the SynEdit:

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

In the XML file, we must include the list of word to use in the completion menu. This list must be included in the label COMPLETION. 

## Defining the list of words for completion

The list of words for compeltion, is defined in the same XML where the syntax is defined.

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
This is the simpler way for to define a list of words for the completion menu. It will be shown, when a key is pressed, if this option is enabled.

There is some parameters to include on the tag <Completion>. They are:

* CaseSensitive -> True or false. Define if the completion function use or not the case of the text to work.
* OpenOnKeyUp -> Enabled if the completion menu is shown when a Key is pressed (released). It's enabled by default.
* SelectOnEnter -> Let to select a word of the completion menu, using the <enter> key.


## Using all the Keywords for completion

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

## Using list of words for completion

The words for completion, can be grouped on list using the tag <LIST>, like in the following definition:

```
<Language>
  <Completion OpenOnKeyUp="false">
  <List Name="MyList"> foo bar foobar
  </List>

  <List Name="MyList2"> foo2 bar2 foobar2
  </List>

  </Completion>
  ...
  
</Language>
```

Having lists in this way, make easy to classify the total words for completion, and can be reused when using advanced definitions.


For more information, check the documentation.


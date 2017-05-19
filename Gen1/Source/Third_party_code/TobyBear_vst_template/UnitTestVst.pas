unit UnitTestVst;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, G2_classes, StdCtrls, G2_Graph, G2_File, G2_types, ExtCtrls,
  uEditor, uPlugin;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    //procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    { Public declarations }
   //FG2    : TG2;
   Editor : TPluginEditorWindow;
   Plugin : APlugin;
   FSelectedSlot : integer;
 end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button2Click(Sender: TObject);
begin
  Editor.Show;
  Plugin.editorNeedsUpdate := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Plugin.setParameter(0, Random);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Plugin := APlugin.Create(nil);
  Editor := TPluginEditorWindow.Create(self);
  Editor.Effect := Plugin;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  Editor.Free;
  Plugin.Free;
end;


end.

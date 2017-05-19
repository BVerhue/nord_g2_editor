unit UnitG2Server;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, StdCtrls, g2_graph, g2_file,
  g2_usb, g2_classes, g2_types, ExtCtrls, g2_mess;

type
  TfrmG2Server = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    G2: TG2;
    Button1: TButton;
    ActionManager1: TActionManager;
    aQuit: TAction;
    cbUSBActive: TCheckBox;
    cbLog: TCheckBox;
    Timer1: TTimer;
    Label1: TLabel;
    Timer2: TTimer;
    Memo2: TMemo;
    Splitter2: TSplitter;
    Label2: TLabel;
    procedure G2USBActiveChange(Sender: TObject; Active: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure G2DeleteClient(Sender: TObject; ClientIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GlobalExceptionHandler( Sender : TObject; E : Exception);
  end;

var
  frmG2Server: TfrmG2Server;

implementation

{$R *.dfm}

procedure TfrmG2Server.aQuitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmG2Server.FormCreate(Sender: TObject);
begin
  Application.OnException := GlobalExceptionHandler;
end;

procedure TfrmG2Server.FormDestroy(Sender: TObject);
begin
  G2.USBActive := False;
end;

procedure TfrmG2Server.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TfrmG2Server.G2DeleteClient(Sender: TObject; ClientIndex: Integer);
begin
  Listbox1.Items.Delete(ClientIndex);
end;

procedure TfrmG2Server.G2USBActiveChange(Sender: TObject; Active: Boolean);
begin
  cbUSBActive.Checked := Active;
end;

procedure TfrmG2Server.GlobalExceptionHandler(Sender: TObject; E: Exception);
begin
  Memo1.Lines.Add( 'ERROR: ' + E.Message);

  {
   you could also call the default
   exception handler:

     Application.ShowException( E );
  }
end;

procedure TfrmG2Server.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  G2.USBActive := True;
end;

end.

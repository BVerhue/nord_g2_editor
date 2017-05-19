unit UnitG2USBTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, g2_types, g2_file,
  g2_mess, g2_usb, LibUSBWinDyn;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure G2LogLine(Sender: TObject; const LogLine: string; LogCmd: Integer);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     FG2 : TG2USB;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var G2DeviceList : TList;
    i : integer;
begin
  if Button1.Caption = 'Connect G2' then begin

    G2DeviceList := TList.Create;
    try
      GetUSBDeviceList( G2DeviceList);

      if G2DeviceList.Count > 0 then begin
        for i := 0 to G2DeviceList.Count - 1 do begin
          Memo1.Lines.Add('Found G2 USB Device : ' + IntToStr(integer(G2DeviceList[i])));
        end;
        Memo1.Lines.Add('Using device : ' + IntToStr(integer(G2DeviceList[0])));
        FG2.G2USBDevice := pusb_device(G2DeviceList[0]);
        FG2.USBActive := True;
        Button1.Caption := 'Disconnect G2';
      end else begin
        MessageBox(0, 'No g2 device found', '', MB_OK)
      end;
    finally
      G2DeviceList.Free;
    end;

  end else begin
    if FG2.USBActive then begin
      FG2.USBActive := False;
    end;
    Button1.Caption := 'Connect G2';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt := '.txt';
  if SaveDialog1.Execute then
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
var version : pusb_version;
begin
  FG2 := TG2USB.Create( nil);
  FG2.IsServer := True;
  FG2.ClientType := ctEditor;
  FG2.Port := DEFAULT_PORT;
  FG2.Host := '127.0.0.1';
  FG2.LogLevel := 1;
  FG2.OnLogLine := G2LogLine;

  version := usb_get_version;

  if version <> nil then begin
    Memo1.lines.Add( 'LibUSB-win32 DLL version : ' + IntToStr(version^.dllmajor) + '.' + IntToStr(version.dllminor) + '.' + IntToStr(version.dllmicro) + '.' + IntToStr(version.dllnano));
  end else
    Memo1.Lines.Add('LibUSB-win32 get version returns nil');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FG2.USBActive then
    FG2.USBActive := False;
  FG2.Free;
end;

procedure TForm1.G2LogLine(Sender: TObject; const LogLine: string;
  LogCmd: Integer);
begin
  Memo1.Lines.Add(LogLine);
end;

end.

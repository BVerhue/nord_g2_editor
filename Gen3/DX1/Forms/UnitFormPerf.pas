unit UnitFormPerf;

//  ////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2011 Bruno Verhue
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Generics.Collections,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
{$IFDEF ANDROID}
  System.Messaging,
  FMX.Platform,
  FMX.Platform.Android,
  //Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  AndroidApi.Helpers,
{$ENDIF}
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2Controls,
  BVE.NMG2ControlsGL,
  BVE.NMG2TexturesGL,
  UnitPerfSlot,
  UnitKeyboard;

type
  TForm1 = class(TForm)
    TimerConnect: TTimer;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    TimerCreate: TTimer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TimerConnectTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bClearLogClick(Sender: TObject);
    procedure TimerCreateTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConMan: IG2ConnectionManager;
    FLog: IG2Log;

    FInfo: TLabel;

    FPerf: TPerfGL;

    FPerfSlotA,
    FPerfSlotB,
    FPerfSlotC,
    FPerfSlotD: TPerfSlotGL;

    FKeyboard: TKeyboardPanelGL;

    FPathImg: string;

    procedure OnLog(Sender: TObject; const LogLine: string; LogCmd: integer);
{$IFDEF ANDROID}
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function HandleIntentAction(const Data: JIntent): Boolean;
{$ENDIF}
  public
    procedure LoadFileStream(aFilename: string);
    procedure ShowInfo(Sender: TObject; aImageFileName: string);
    procedure HideInfo;
  end;

var
  Form1: TForm1;
  FTextureList: TTextureListGL;

implementation
uses
  System.Zip,
  BVE.NMG2ControlsG2,
  BVE.NMG2ComManager,
  BVE.NMG2ComUSB,
  UnitSelectModule,
  UnitFormBanks,
  UnitFormAddModule;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
{$IFDEF ANDROID}
var
  AppEventService: IFMXApplicationEventService;
{$ENDIF}
begin
{$IFDEF ANDROID}
  // http://sourceforge.net/p/radstudiodemos/code/HEAD/tree/branches/RADStudio_Seattle/Object%20Pascal/Mobile%20Snippets/AndroidIntents/ReceiveIntent/Unit1.pas#l85
  // http://stackoverflow.com/questions/1733195/android-intent-filter-for-a-particular-file-extension

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, AppEventService) then
    AppEventService.SetApplicationEventHandler(HandleAppEvent);

  // Register the type of intent action that we want to be able to receive.
  // Note: A corresponding <action> tag must also exist in the <intent-filter> section of AndroidManifest.template.xml.

  MainActivity.registerIntentAction(TJIntent.JavaClass.ACTION_VIEW);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, HandleActivityMessage);
{$ENDIF}

{$IFDEF MSWINDOWS}
  FPathImg := '..\..\..\Img\';
{$ELSE}
  FPathImg := System.IOUtils.TPath.GetDocumentsPath + '/';
{$ENDIF}

  FConMan := TG2ConnectionManager.Create;
  FLog := TG2Log.Create;

  //FInfo := TImage.Create(self);
  FInfo := TLabel.Create(Self);
  FInfo.Parent := Self;
  FInfo.Align := TAlignLayout.None;
  //FInfo.WrapMode := TImageWrapMode.Center;
  FInfo.Font.Family := FONT_FAMILY;
  FInfo.Font.Size := 24;
  FInfo.Font.Style := [TFontStyle.fsBold];

  Memo1.Visible := False;

  ShowInfo(Self, 'LoadingGraphics.png');
  TimerCreate.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
end;

{$IFDEF ANDROID}
function TForm1.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
var
  StartupIntent: JIntent;
begin
  Result := False;
  case AAppEvent of
    TApplicationEvent.BecameActive:
      begin
        StartupIntent := MainActivity.getIntent;
        if StartupIntent <> nil then
           HandleIntentAction(StartupIntent);
      end;
  end;
end;

procedure TForm1.HandleActivityMessage(const Sender: TObject; const M: TMessage);
begin
  if M is TMessageReceivedNotification then
    HandleIntentAction(TMessageReceivedNotification(M).Value);
end;

function TForm1.HandleIntentAction(const Data: JIntent): Boolean;
var
  Uri: Jnet_Uri;
  FileName: String;
begin
  Result := False;
  if Data <> nil then
  begin
    Uri := Data.getData;
    if assigned(Uri) then
    begin
      FileName := JStringToString(Uri.getPath);
      // use uriStr as needed...
      frmBanks.LoadFileStream(FileName);
    end;
  end;
end;
{$ENDIF}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FConMan.DisconnectAll;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  FInfo.Width := Width;
  FInfo.Height := Height;
  FInfo.BringToFront;
end;

procedure TForm1.HideInfo;
begin
  FInfo.Visible := False;
  Invalidate;
end;

procedure TForm1.TimerConnectTimer(Sender: TObject);
var
  Connection: IG2Connection;
begin
  TimerConnect.Enabled := False;

  ShowInfo(Self, 'Initializing.png');
  try

    FConMan.DeviceDiscovery;

    if FConMan.ConnectionList.Count = 0 then
    begin
      Connection := TG2USBConnection.Create(FConMan);
      FConMan.ConnectionList.Add(Connection);
    end
    else
      Connection := FConMan.ConnectionList[0];
    FConMan.SelectedConnectionIndex := 0;

    Connection.Log := FLog;
    FLog.Enabled := False;
    FLog.OnLogLine := OnLog;

    Connection.Connect;

    FPerfSlotA.ConnectionIndex := 0;
    FPerfSlotA.SlotIndex := 0;

    FPerfSlotB.ConnectionIndex := 0;
    FPerfSlotB.SlotIndex := 1;

    FPerfSlotC.ConnectionIndex := 0;
    FPerfSlotC.SlotIndex := 2;

    FPerfSlotD.ConnectionIndex := 0;
    FPerfSlotD.SlotIndex := 3;

    FPerf.ConnectionIndex := 0;

    frmBanks.ConnectionIndex := 0;
  finally
    HideInfo;
  end;
end;

procedure TForm1.TimerCreateTimer(Sender: TObject);
begin
  TimerCreate.Enabled := False;

  TZipFile.ExtractZipFile(FPathImg + 'G2tex.zip', FTextureList.CacheDir);
  //CreateTextures(FTextureList, FPathImg + 'G2tex.zip');

  //FTextureList.MaxSize := 100000000;
  //CreateTextures(FTextureList, '', nil);
  //CreateModulePreviews(FTextureList, nil);
  //FTextureList.SaveTextures(FPathImg);

  FTextureList.MaxSize := 3000000;

  ShowInfo(Self, 'RenderingGraphics.png');

  FPerfSlotD := TPerfSlotGL.Create(self);
  FPerfSlotD.Parent := Self;
  FPerfSlotD.Width := Width;
  FPerfSlotD.Height := 80;
  FPerfSlotD.Align := TAlignLayout.Top;
  FPerfSlotD.TextureList := FTextureList;
  FPerfSlotD.LayoutControls;
  FPerfSlotD.ConMan := FConMan;

  FPerfSlotC := TPerfSlotGL.Create(self);
  FPerfSlotC.Parent := Self;
  FPerfSlotC.Width := Width;
  FPerfSlotC.Height := 80;
  FPerfSlotC.Align := TAlignLayout.Top;
  FPerfSlotC.TextureList := FTextureList;
  FPerfSlotC.LayoutControls;
  FPerfSlotC.ConMan := FConMan;

  FPerfSlotB := TPerfSlotGL.Create(self);
  FPerfSlotB.Parent := Self;
  FPerfSlotB.Width := Width;
  FPerfSlotB.Height := 80;
  FPerfSlotB.Align := TAlignLayout.Top;
  FPerfSlotB.TextureList := FTextureList;
  FPerfSlotB.LayoutControls;
  FPerfSlotB.ConMan := FConMan;

  FPerfSlotA := TPerfSlotGL.Create(self);
  FPerfSlotA.Parent := Self;
  FPerfSlotA.Width := Width;
  FPerfSlotA.Height := 80;
  FPerfSlotA.Align := TAlignLayout.Top;
  FPerfSlotA.TextureList := FTextureList;
  FPerfSlotA.LayoutControls;
  FPerfSlotA.ConMan := FConMan;

  FPerf := TPerfGL.Create(Self);
  FPerf.Parent := Self;
  FPerf.Width := Width;
  FPerf.Height := 80;
  FPerf.Align := TAlignLayout.Top;
  FPerf.TextureList := FTextureList;
  FPerf.LayoutControls;
  FPerf.ConMan := FConMan;

  FKeyboard := TKeyboardPanelGL.Create(Self);
  FKeyboard.Parent := Self;
  FKeyboard.Height := 150;
  FKeyboard.Align := TAlignLayout.Bottom;
  FKeyboard.TextureList := FTextureList;
  FKeyboard.LayoutRect := FKeyboard.CalcLayoutRect;
  FKeyboard.ConMan := FConMan;

  Application.CreateForm(TfrmBanks, frmBanks);
  frmBanks.TextureList := FTextureList;
  frmBanks.ConMan := FConMan;

  Application.CreateForm(TfrmAddModule, frmAddModule);
  frmAddModule.TextureList := FTextureList;
  frmAddModule.ConMan := FConMan;

  TimerConnect.Enabled := True;
end;

procedure TForm1.bClearLogClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.LoadFileStream(aFilename: string);
begin
  FConMan.SelectedConnectionIndex := 0;
  FConMan.LoadFromFile(aFilename);
  Invalidate;
end;

procedure TForm1.OnLog(Sender: TObject; const LogLine: string; LogCmd: integer);
begin
  Memo1.Lines.Add(LogLine);
end;

procedure TForm1.ShowInfo(Sender: TObject; aImageFileName: string);
begin
  //FInfo.Bitmap.LoadFromFile(FPathImg + aImageFileName);
  FInfo.Text := aImageFileName;
  FInfo.BringToFront;
  FInfo.Repaint;
  Invalidate;
end;

initialization
  FTextureList := TTextureListGL.Create(nil);

finalization
  FTextureList.Free;

end.

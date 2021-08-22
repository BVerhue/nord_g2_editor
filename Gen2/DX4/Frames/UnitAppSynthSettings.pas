unit UnitAppSynthSettings;
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types,
{$IFDEF VER260}
  FMX.Graphics,
{$ENDIF}
{$IF NOT Defined(ANDROID)}
  UnitUtils,
{$ENDIF}
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, BVE.NMG2ControlsFMX, BVE.NMG2GraphFMX, FMX.Controls.Presentation;
type
  TframeAppSynthSettings = class(TFrame)
    bfUSBConnection: TG2BtnFlat;
    G2Label1: TG2Label;
    lbCaption: TG2Label;
    eHost: TEdit;
    ePort: TEdit;
    G2Label2: TG2Label;
    G2Label3: TG2Label;
    btEnableLog: TG2BtnText;
    btSaveLog: TG2BtnText;
    procedure bfUSBConnectionChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure eHostExit(Sender: TObject);
    procedure ePortExit(Sender: TObject);
    procedure btEnableLogChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSaveLogChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FSynth : TG2GraphFMX;
    FLogLines : TStringList;
    FUSBConnection : boolean;
    FSynthName : string;
    FID : string;
    FHost : string;
    FPort : integer;
    FOnChange : TNotifyEvent;
    procedure SetSynth(const Value: TG2GraphFMX);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    procedure SetSynthName(const Value: string);
    procedure SetUSBConnection(const Value: boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateControls;
    procedure InitDefaults;
    property ID : string read FID write FID;
    property Synth : TG2GraphFMX read FSynth write SetSynth;
    property SynthName : string read FSynthName write SetSynthName;
    property USBConnection : boolean read FUSBConnection write SetUSBConnection;
    property Host : string read FHost write SetHost;
    property Port : integer read FPort write SetPort;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;
implementation
{$R *.fmx}
{ TframeAppSynthSettings }
procedure TframeAppSynthSettings.bfUSBConnectionChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FUSBConnection := bfUSBConnection.Value = 1;
  if assigned(FOnChange) then
    FOnChange(self);
end;
procedure TframeAppSynthSettings.btEnableLogChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 1 then
    if assigned(FSynth) then begin
      FSynth.LogLevel := btEnableLog.Value;
    end;
end;
procedure TframeAppSynthSettings.btSaveLogChangeValue(Sender: TObject;
  const aValue: Integer);
var LogFileName : string;
begin
{$IF NOT Defined(ANDROID)}
  if aValue = 1 then begin
    if assigned(FSynth) and assigned(FSynth.LogLines) then begin
{$IF Defined(MSWINDOWS)}
      LogFileName := 'g2_editor_fmx_log_' + FloatToStr(Now) + '.txt';
{$ELSE}
      LogFileName := GetHomePath + PathDelim + 'g2_editor_fmx_log_' + FloatToStr(Now) + '.txt';
{$ENDIF}
      FSynth.SaveLog(LogFileName);
      FSynth.ClearLog;
      TUtils.Open(LogFileName);
    end;
  end;
{$ENDIF}
end;
constructor TframeAppSynthSettings.Create(AOwner: TComponent);
begin
  inherited;
  FLogLines := TStringList.Create;
end;

destructor TframeAppSynthSettings.Destroy;
begin
  FLogLines.Free;
  inherited;
end;

procedure TframeAppSynthSettings.eHostExit(Sender: TObject);
begin
  FHost := eHost.Text;
  if assigned(FOnChange) then
    FOnChange(self);
end;
procedure TframeAppSynthSettings.ePortExit(Sender: TObject);
var Value, Code : integer;
begin
  if ePort.Text <> '' then begin
    val(ePort.Text, Value, Code);
    if Code <> 0 then
      raise Exception.Create('Must enter a number.');
    FPort := Value;
    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;
procedure TframeAppSynthSettings.InitDefaults;
begin
  FUSBConnection := True;
  FHost := '127.0.0.1';
  FPort :=  2501;
end;
procedure TframeAppSynthSettings.SetHost(const Value: string);
begin
  if FHost <> Value then begin
    FHost := Value;
    UpdateControls;
  end;
end;
procedure TframeAppSynthSettings.SetPort(const Value: integer);
begin
  if FPort <> Value then begin
    FPort := Value;
    UpdateControls;
  end;
end;
procedure TframeAppSynthSettings.SetSynth(const Value: TG2GraphFMX);
begin
  if Value <> FSynth then begin
    FSynth := Value;
    FSynthName := FSynth.SynthName;
{$IF NOT Defined(ANDROID)}
    FPort := FSynth.Port;
    FHost := FSynth.Host;
    FUSBConnection := FSynth.IsServer;
{$ENDIF}
    UpdateControls;
  end;
end;
procedure TframeAppSynthSettings.SetSynthName(const Value: string);
begin
  if Value <> FSynthName then begin
    FSynthName := Value;
    UpdateControls;
  end;
end;
procedure TframeAppSynthSettings.SetUSBConnection(const Value: boolean);
begin
  if FUSBConnection <> Value then begin
    FUSBConnection := Value;
    UpdateControls;
  end;
end;
procedure TframeAppSynthSettings.UpdateControls;
begin
  if assigned(FSynth) then
    lbCaption.LabelText := FSynth.SynthName +  ' connections'
  else
    lbCaption.LabelText := 'Unknown' +  ' connections';

  if FUSBConnection then
    bfUSBConnection.Value := 1
  else
    bfUSBConnection.Value := 0;
  eHost.Text := FHost;
  ePort.Text := IntToStr(FPort);
end;
end.

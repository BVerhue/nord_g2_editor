unit UnitEditorCentral;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TabControl, FMX.Layouts,
{$IF Defined(VER260) or Defined(VER270)}
  FMX.Graphics,
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  LibUSBWinDyn,
{$ENDIF}
{$IF Defined(MACOS)}
  libusb_dyn,
{$ENDIF}
  BVE.NMG2Types, BVE.NMG2ControlsFMX, BVE.NMG2GraphFMX,
  BVE.NMG2ParamDef, BVE.NMG2ModuleDef, BVE.NMG2USB,
  UnitAppSettings, UnitSynthStrip, UnitSynth, UnitPatch, UnitAddModule;

type
  TAppShowMessageEvent = procedure(Sender: TObject; aMessage : string) of object;
  TAppCloseMessageEvent = procedure(Sender: TObject) of object;


  TframeEditorCentral = class(TFrame)
    tcSynths: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    sbSynths: THorzScrollBox;
    FrameSynthStrip1: TFrameSynthStrip;
    FrameSynthStrip2: TFrameSynthStrip;
    FrameSynthStrip3: TFrameSynthStrip;
    frameSynth1: TframeSynth;
    frameSynth2: TframeSynth;
    frameSynth3: TframeSynth;
    procedure FrameSynthStrip1Click(Sender: TObject);
    procedure FrameSynthStrip2Click(Sender: TObject);
    procedure tcSynthsChange(Sender: TObject);
    procedure FrameSynthStrip3Click(Sender: TObject);
  private
    // The patch, appsettings and add module frame are dynamically created and
    // will move to the selected synth
    [Weak] FframePatch : TframePatch;
    [Weak] FframeAddModule : TframeAddModule;
    [Weak] FframeAppSettings : TframeAppSettings;

    [Weak] FStateStyleList : TG2StateStyleList;

    FG2List : TObjectList<TG2GraphFMX>;

    FModuleImageList : TObjectDictionary<integer, TBitmap>;

    FInitialized : boolean;

    FOnShowAppMessage : TAppShowMessageEvent;
    FOnCloseAppMessage : TAppCloseMessageEvent;

    function GetSelectedSynth: integer;
    procedure SetSelectedSynth(const Value: integer);
    procedure SetOnCloseAppMessage(const Value: TAppCloseMessageEvent);
    procedure SetOnShowAppMessage(const Value: TAppShowMessageEvent);
    procedure SelectSynth( aIndex : integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DeviceDiscovery;

    procedure AddSynth( aIndex : integer; aSynth : TG2GraphFMX);
    procedure Init;

    procedure ProcessKeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);

    property G2List : TObjectList<TG2GraphFMX> read FG2List write FG2List;
    property SelectedSynth : integer read GetSelectedSynth write SetSelectedSynth;

    property OnShowAppMessage : TAppShowMessageEvent read FOnShowAppMessage write SetOnShowAppMessage;
    property OnCloseAppMessage : TAppCloseMessageEvent read FOnCloseAppMessage write SetOnCloseAppMessage;
  end;

implementation

{$R *.fmx}

{ TframeSynths }

constructor TframeEditorCentral.Create(AOwner: TComponent);
begin
  inherited;

  FG2List := TObjectList<TG2GraphFMX>.Create(True);

  FInitialized := False;

  frameSynthStrip1.Visible := False;
  frameSynthStrip2.Visible := False;
  frameSynthStrip3.Visible := False;

  FModuleImageList := TObjectDictionary<integer, TBitmap>.Create([doOwnsValues], 170);

  // Shared frames in SynthFrames, dynamically created
  FframePatch := TframePatch.Create(self);
  FframePatch.Align := TAlignLayout.alClient;

  FframeAddModule := TframeAddModule.Create(self);
  FframeAddModule.Align := TAlignLayout.alClient;

  FframeAppSettings := TframeAppSettings.Create(self);
  FframeAppSettings.Align := TAlignLayout.alCenter;

  FframePatch.frameAppSettings := FframeAppSettings;

  frameSynth1.SetframeAppSettings( FframeAppSettings);
  frameSynth1.SetframePatch( FframePatch);
  frameSynth1.SetframeAddModule( FframeAddModule);

  frameSynth2.SetframeAppSettings( FframeAppSettings);
  frameSynth2.SetframePatch( FframePatch);
  frameSynth2.SetframeAddModule( FframeAddModule);

  frameSynth3.SetframeAppSettings( FframeAppSettings);
  frameSynth3.SetframePatch( FframePatch);
  frameSynth3.SetframeAddModule( FframeAddModule);

  frameSynth1.FrameSynthStrip := frameSynthStrip1;
  frameSynth2.FrameSynthStrip := frameSynthStrip2;
  frameSynth3.FrameSynthStrip := frameSynthStrip3;

  SelectSynth(0);
end;

destructor TframeEditorCentral.Destroy;
var i : integer;
begin
  for i := 0 to FG2List.Count - 1 do begin
    FG2List[i].USBActive := False;
  end;
  FG2List.Free;

  FModuleImageList.Free;
  inherited;
end;

procedure TframeEditorCentral.DeviceDiscovery;
var i : integer;
    G2DeviceList : TList;
    G2 : TG2GraphFMX;
begin
  G2DeviceList := TList.Create;
  try
    GetUSBDeviceList( G2DeviceList);
    if G2DeviceList.Count > 0 then begin
      for i := 0 to min(G2DeviceList.Count,3) - 1 do begin
        G2 := TG2GraphFMX.Create(self);
        G2.ClientType := ctEditor;
        G2.Host := '127.0.0.1';
        G2.Port := 2501 + FG2List.Count;
        G2.IsServer := True;
        //G2.LogLevel := 1;
        FG2List.Add( G2);
{$IFDEF MSWINDOWS}
        G2.G2USBDevice := pusb_device(G2DeviceList[i]);
{$ELSE}
        G2.G2USBDevice := Plibusb_device(G2DeviceList[i]));
{$ENDIF}
        AddSynth( i, G2);
      end;
    end else begin
      G2 := TG2GraphFMX.Create(self);
      G2.ClientType := ctEditor;
      G2.Host := '127.0.0.1';
      G2.Port := 2501;
      G2.IsServer := True;
      FG2List.Add( G2);
      G2.G2USBDevice := nil;
      AddSynth(0, G2);
      ShowMessage('No g2 device found')
    end;
  finally
    G2DeviceList.Free;
  end;
end;

procedure TframeEditorCentral.AddSynth( aIndex : integer; aSynth: TG2GraphFMX);
begin
  case aIndex of
  0 : begin
        frameSynthStrip1.Synth := aSynth;
        frameSynth1.Synth := aSynth;
        frameSynthStrip1.Visible := True;
        SelectSynth(0);
      end;
  1 : begin
        frameSynthStrip2.Synth := aSynth;
        frameSynth2.Synth := aSynth;
        frameSynthStrip2.Visible := True;
        SelectSynth(1);
      end;
  2 : begin
        frameSynthStrip3.Synth := aSynth;
        frameSynth3.Synth := aSynth;
        frameSynthStrip3.Visible := True;
        SelectSynth(2);
      end;
  end;
end;

procedure TframeEditorCentral.Init;
begin
  if assigned(FOnShowAppMessage) then
    FOnShowAppMessage(Self, 'Loading app settings...');

  if assigned(FOnShowAppMessage) then
     FOnShowAppMessage(Self, 'Creating module images...');

{$IF not Defined(Android)}
  CreateModuleImages(FModuleImageList);
{$ENDIF}

  FframeAppSettings.AddSynth( 'Synth1', frameSynth1.Synth);
  FframeAppSettings.AddSynth( 'Synth2', frameSynth2.Synth);
  FframeAppSettings.AddSynth( 'Synth3', frameSynth3.Synth);

  FframeAppSettings.LoadSettings;

  frameSynth1.Init;
  frameSynth2.Init;
  frameSynth3.Init;

  FframeAddModule.ModuleImageList := FModuleImageList;
  FframeAddModule.UpdateControls;

  if assigned(FOnShowAppMessage) then
    FOnShowAppMessage(Self, 'Scanning patch folder...');

  FframePatch.UpdateFiles;

  FInitialized := True;
  SelectedSynth := 0;
end;


procedure TframeEditorCentral.FrameSynthStrip1Click(Sender: TObject);
begin
  tcSynths.TabIndex := 0;
end;

procedure TframeEditorCentral.FrameSynthStrip2Click(Sender: TObject);
begin
  tcSynths.TabIndex := 1;
end;

procedure TframeEditorCentral.FrameSynthStrip3Click(Sender: TObject);
begin
  tcSynths.TabIndex := 3;
end;

function TframeEditorCentral.GetSelectedSynth: integer;
begin
  Result := tcSynths.TabIndex;
end;

procedure TframeEditorCentral.ProcessKeyDown(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case tcSynths.TabIndex of
  0 : frameSynth1.ProcessKeyDown( Key, KeyChar, Shift);
  1 : frameSynth2.ProcessKeyDown( Key, KeyChar, Shift);
  2 : frameSynth3.ProcessKeyDown( Key, KeyChar, Shift);
  end;
end;

procedure TframeEditorCentral.ProcessKeyUp(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case tcSynths.TabIndex of
  0 : frameSynth1.ProcessKeyUp( Key, KeyChar, Shift);
  1 : frameSynth2.ProcessKeyUp( Key, KeyChar, Shift);
  3 : frameSynth3.ProcessKeyUp( Key, KeyChar, Shift);
  end;
end;

procedure TframeEditorCentral.SetOnCloseAppMessage(
  const Value: TAppCloseMessageEvent);
begin
  FOnCloseAppMessage := Value;
  frameSynth1.OnCloseAppMessage := Value;
  frameSynth2.OnCloseAppMessage := Value;
  frameSynth3.OnCloseAppMessage := Value;
end;

procedure TframeEditorCentral.SetOnShowAppMessage(
  const Value: TAppShowMessageEvent);
begin
  FOnShowAppMessage := Value;
  frameSynth1.OnShowAppMessage := Value;
  frameSynth2.OnShowAppMessage := Value;
  frameSynth3.OnShowAppMessage := Value;
end;

procedure TframeEditorCentral.SetSelectedSynth(const Value: integer);
begin
  if Value <> tcSynths.TabIndex then
    tcSynths.TabIndex := Value;
end;

procedure TframeEditorCentral.SetStateStyles( aStateStyleList: TG2StateStyleList);
begin
  FStateStyleList := aStateStyleList;

  frameSynth1.SetStateStyles( aStateStyleList);
  frameSynth2.SetStateStyles( aStateStyleList);
  frameSynth3.SetStateStyles( aStateStyleList);

  frameSynthStrip1.SetStateStyles( aStateStyleList);
  frameSynthStrip2.SetStateStyles( aStateStyleList);
  frameSynthStrip3.SetStateStyles( aStateStyleList);


  FframePatch.SetStateStyles( FStateStyleList);
  FframeAddModule.SetStateStyles( FstateStyleList);
  FframeAppSettings.SetStateStyles( FStateStyleList);
end;

procedure TframeEditorCentral.tcSynthsChange(Sender: TObject);
begin
  if not FInitialized then
    exit;

  SelectSynth(tcSynths.TabIndex);
end;

procedure TframeEditorCentral.SelectSynth(aIndex: integer);
begin
  case aIndex of
  0 : begin
        FframePatch.Parent := frameSynth1.tcPatchFunctions.Tabs[3];
        FframePatch.Synth := frameSynth1.Synth;

        FframeAddModule.Parent := frameSynth1.tcPatchFunctions.Tabs[0];
        FframeAddModule.frameSlot := frameSynth1.SelectedFrameSlot;

        FFrameAppSettings.Parent := frameSynth1.tcFrame.Tabs[8];

        frameSynthStrip1.Selected := True;
        frameSynthStrip2.Selected := False;
        frameSynthStrip3.Selected := False;
      end;
  1 : begin
        FframePatch.Parent := frameSynth2.tcPatchFunctions.Tabs[3];
        FframePatch.Synth := frameSynth2.Synth;

        FframeAddModule.Parent := frameSynth2.tcPatchFunctions.Tabs[0];
        FframeAddModule.frameSlot := frameSynth2.SelectedFrameSlot;

        FFrameAppSettings.Parent := frameSynth2.tcFrame.Tabs[8];

        frameSynthStrip1.Selected := False;
        frameSynthStrip2.Selected := True;
        frameSynthStrip3.Selected := False;
      end;
  2 : begin
        FframePatch.Parent := frameSynth3.tcPatchFunctions.Tabs[3];
        FframePatch.Synth := frameSynth3.Synth;

        FframeAddModule.Parent := frameSynth3.tcPatchFunctions.Tabs[0];
        FframeAddModule.frameSlot := frameSynth3.SelectedFrameSlot;

        FFrameAppSettings.Parent := frameSynth3.tcFrame.Tabs[8];

        frameSynthStrip1.Selected := False;
        frameSynthStrip2.Selected := False;
        frameSynthStrip3.Selected := True;
      end;
  end;
end;

end.

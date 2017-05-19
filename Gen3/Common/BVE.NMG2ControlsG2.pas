unit BVE.NMG2ControlsG2;

// ////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011 Bruno Verhue
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// ////////////////////////////////////////////////////////////////////////////

// Added G2 connectivity to the set of controls

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.UIConsts,
  System.Generics.Collections,
  System.Math.Vectors,
  System.RTLConsts,
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Materials,
  BVE.NMG2Types,
  BVE.NMG2GraphTypes,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2ControlsGL,
  BVE.NMG2Controls,
  BVE.NMG2TexturesGL;

type
  TCreateG2ControlEvent = procedure(Sender: TObject; aModule: IG2Module;
    aControl: TControlGL) of Object;

  TG2FormGL = class(TForm, IG2Observer)
  private
    [Weak]
    FWConMan: IG2ConnectionManager;
    FConnectionIndex: integer;
    FTextureList: TTextureListGL;
  protected
    function GetConnection: IG2Connection;
    procedure SetConnectionManager(const Value: IG2ConnectionManager); virtual;
    procedure SetConnectionIndex(const Value: integer); virtual;
    procedure SetTextureList(const Value: TTextureListGL); virtual;
    procedure RemoveAsObserver; virtual;
    procedure RegisterAsObserver; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); virtual;

    property ConMan: IG2ConnectionManager read FWConMan
      write SetConnectionManager;
    property ConnectionIndex: integer read FConnectionIndex
      write SetConnectionIndex;
    property Connection: IG2Connection read GetConnection;

    property TextureList: TTextureListGL read FTextureList write SetTextureList;
  end;

  TG2PanelGL = class(TBufferedViewPortGL, IG2Observer)
  private
    [Weak]
    FWConMan: IG2ConnectionManager;
    FConnectionIndex: integer;
    FTextureList: TTextureListGL;
    function GetPerf: IG2Perf;

    function GetSynth: IG2Synth;
  protected
    function GetConnection: IG2Connection;
    procedure SetConnectionManager(const Value: IG2ConnectionManager); virtual;
    procedure SetConnectionIndex(const Value: integer); virtual;
    procedure SetTextureList(const Value: TTextureListGL); virtual;
    procedure RemoveAsObserver; virtual;
    procedure RegisterAsObserver; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LayoutControls; virtual;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); virtual;

    property ConMan: IG2ConnectionManager read FWConMan
      write SetConnectionManager;
    property ConnectionIndex: integer read FConnectionIndex
      write SetConnectionIndex;
    property Connection: IG2Connection read GetConnection;

    property TextureList: TTextureListGL read FTextureList write SetTextureList;
    property Synth: IG2Synth read GetSynth;
    property Perf: IG2Perf read GetPerf;
  end;

  TG2SlotFormGL = class(TG2FormGL)
  private
    FSlotIndex: integer;
  protected
    function GetSlot: IG2Slot;
    procedure SetSlotIndex(const Value: integer); virtual;

    procedure RemoveAsObserver; override;
    procedure RegisterAsObserver; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SlotIndex: integer read FSlotIndex write SetSlotIndex;
    property Slot: IG2Slot read GetSlot;
  end;

  TG2SlotPanelGL = class(TG2PanelGL)
  private
    FSlotIndex: integer;
  protected
    function GetSlot: IG2Slot;
    procedure SetSlotIndex(const Value: integer); virtual;

    procedure RemoveAsObserver; override;
    procedure RegisterAsObserver; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SlotIndex: integer read FSlotIndex write SetSlotIndex;
    property Slot: IG2Slot read GetSlot;
  end;

  TG2ModuleGL = class(TTexMatrixControlGL, IG2Observer)
  private
    [Weak]
    FWModule: IG2Module;
    FModuleLabel: TMatrixTextObject;
    FLines: TObjectList<TColorObject>;
    FLabels: TObjectList<TTexObject>;
    FSelected: boolean;
    FOnSelect: TNotifyEvent;
    procedure SetSelected(const Value: boolean);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
  protected
    procedure SetModule(const Value: IG2Module);
    procedure SetTextureList(const Value: TTextureListGL); override;
    procedure UpdateCellIndex; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateModuleControls(aConnectParams, aConnectConnectors: boolean;
      aKnobControl: TKnobControl; ProcCreateControl: TCreateG2ControlEvent);

    procedure InvalidateConnectors;

    procedure Init;

    //procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
    //  X, Y: Single); override;

    procedure RenderControl(aContext: TContext3D); override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Caption: string read GetCaption write SetCaption;
    // property Color: TAlphaColor read GetColor write SetColor;
    property Selected: boolean read FSelected write SetSelected;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Module: IG2Module read FWModule write SetModule;
  end;

  TG2ConnectorGL = class(TTexControlGL, IG2Observer)
  private
    [Weak]
    FWConnector: IG2Connector;
    procedure SetConnector(const Value: IG2Connector);
    function GetPoint: TPointF;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    procedure InvalidateCables;

    property Connector: IG2Connector read FWConnector write SetConnector;
    property Point: TPointF read GetPoint;
  end;

  TG2LedGL = class(TTexControlGL, IG2Observer)
  private
    FCodeRef: byte;
    [Weak]
    FWLed: IG2Led;
    procedure SetLed(const Value: IG2Led);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Led: IG2Led read FWLed write SetLed;
    property CodeRef: byte read FCodeRef write FCodeRef;
  end;

  TG2MiniVUGL = class(TTexControlGL, IG2Observer)
  private
    [Weak]
    FWLed: IG2Led;
    procedure SetLed(const Value: IG2Led);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Led: IG2Led read FWLed write SetLed;
  end;

  TG2TextFieldGL = class(TTextFieldGL, IG2Observer)
  private
    FWFunc: IG2Function;
    procedure SetFunc(const Value: IG2Function);
    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property Func: IG2Function read FWFunc write SetFunc;
  end;

  TG2GraphGL = class(TGraphGL, IG2Observer)
  private
    FWFunc: IG2Function;
    procedure SetFunc(const Value: IG2Function);
    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property Func: IG2Function read FWFunc write SetFunc;
  end;

  TG2TextEditGL = class(TTextEditGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
    procedure SetParam(const aValue: IG2Param);
    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);
  protected
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2KnobGL = class(TKnobGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
    procedure SetParam(const Value: IG2Param);

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);
  protected
    procedure SetValue(const aValue: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseLeave; override;

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2BtnTextGL = class(TBtnTextGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
    procedure SetParam(const aValue: IG2Param);
  protected
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2BtnFlatGL = class(TBtnFlatGL, IG2Observer)
  public
    [Weak]
    FWParam: IG2Param;
    procedure SetParam(const aValue: IG2Param);
  protected
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2BtnLevelShiftGL = class(TG2BtnFlatGL)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TG2BtnIncDecGL = class(TBtnIncDecGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
    procedure SetParam(const aValue: IG2Param);
  protected
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2BtnRadioGL = class(TBtnRadioGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
  protected
    procedure SetParam(const aValue: IG2Param);
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2BtnRadioEditGL = class(TBtnRadioEditGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
    procedure SetParam(const aValue: IG2Param);
  protected
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Param: IG2Param read FWParam write SetParam;
  end;

  TG2SelectorGL = class;

  TG2SelectorListGL = class(TTexMatrixControlGL)
  private
    FSelector: TG2SelectorGL;
  protected
    procedure UpdateCellIndex; override;
  public
    constructor Create(aSelector: TG2SelectorGL); reintroduce;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;

    procedure RenderControl(aContext: TContext3D); override;
    property Selector: TG2SelectorGL read FSelector;
  end;

  TG2SelectorGL = class(TTexControlGL, IG2Observer)
  private
    [Weak]
    FWParam: IG2Param;
    FValue: integer;
    FMaxValue: integer;
    FBtn: TTexControlGL;
    FList: TG2SelectorListGL;
    FDropDown: TNotifyEvent;
    FCloseUp: TNotifyEvent;
    procedure SetValue(const aValue: integer);
  protected
    procedure SetMaxValue(const aValue: integer);
    procedure SetParam(const aValue: IG2Param);
    procedure SetTextureList(const aValue: TTextureListGL); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloseUp;
    procedure DropDown;

    procedure Init;

    procedure RenderControl(aContext: TContext3D); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Param: IG2Param read FWParam write SetParam;
    property Value: integer read FValue write SetValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Btn: TTexControlGL read FBtn;
    property List: TG2SelectorListGL read FList;
    property OnDropDown: TNotifyEvent read FDropDown write FDropDown;
    property OnCloseUp: TNotifyEvent read FCloseUp write FCloseUp;
  end;

  TG2CableGL = class(TCableGL, IG2Observer)
  private
    [Weak]
    FWCable: IG2Cable;
    FConnector1, FConnector2: TG2ConnectorGL;
    procedure SetCable(const Value: IG2Cable);
    procedure SetConnector1(const Value: TG2ConnectorGL);
    procedure SetConnector2(const Value: TG2ConnectorGL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Cable: IG2Cable read FWCable write SetCable;
    property Connector1: TG2ConnectorGL read FConnector1 write SetConnector1;
    property Connector2: TG2ConnectorGL read FConnector2 write SetConnector2;
  end;

function ConvertToAlpha(aColor: integer): TAlphaColor;
function ModuleRect(const aCol, aRow, aHeight: integer): TRectF;
procedure CreateTextures(aTextureList: TTextureListGL; const aFileName: string;
  aLog: TStrings);
procedure CreateModulePreviews(aTextureList: TTextureListGL; aLog: TStrings);

implementation

uses
  System.Zip,
  BVE.NMG2Data,
  BVE.NMG2Module,
  BVE.NMG2Cable,
  BVE.NMG2Param;

function ConvertToAlpha(aColor: integer): TAlphaColor;
begin
  Result := $FF000000 + (Cardinal(aColor) and $000000FF) shl 16 +
    (Cardinal(aColor) and $0000FF00) + (Cardinal(aColor) and $00FF0000) shr 16;
end;

function ModuleRect(const aCol, aRow, aHeight: integer): TRectF;
begin
  Result.Left := aCol * GRID_UNITS_X;
  Result.Right := aCol * GRID_UNITS_X + UNITS_COL * SCALE_X;
  Result.Top := aRow * GRID_UNITS_Y;
  Result.Bottom := aRow * GRID_UNITS_Y + aHeight * UNITS_ROW * SCALE_Y;
end;

procedure RenderModuleTexture(aTexture: TTiledTexture;
  aModuleDef: TG2ModuleDef);
var
  Rows, Cols, TileWidth, TileHeight: integer;

  procedure DrawModule(aRow: integer; aColor: TAlphaColor);
  var
    R: TRectF;
    i: integer;
    dy, d, lx, ly: Single;
  begin
    dy := TileHeight * aRow;
    R := RectF(0, dy, TileWidth, dy + TileHeight);

    aTexture.Canvas.BeginScene;
    try
      aTexture.Canvas.Stroke.Color := aColor;
      aTexture.Canvas.Fill.Color := aColor;
      d := SCALE_X * 2;
      aTexture.Canvas.StrokeThickness := d;
      R.Inflate(-d / 2, -d / 2);
      aTexture.Canvas.DrawRect(R, 0, 0, [], 1);

      aTexture.Canvas.Font.Size := SCALE_Y * 10;
      aTexture.Canvas.Font.Family := FONT_FAMILY;

      // Canvas.FillText(R, aModuleDef.ModuleName, False, 1, [],
      // TTextAlign.Leading, TTextAlign.Leading);

      for i := 0 to High(TextDefs) do
      begin
        if TextDefs[i].ModuleID = aModuleDef.ModuleID then
        begin
          aTexture.Canvas.Font.Size := (TextDefs[i].FontSize - 2) * SCALE_Y;

          R := RectF(TextDefs[i].XPos * SCALE_X, dy + TextDefs[i].YPos *
            SCALE_Y, TextDefs[i].XPos * SCALE_X + aTexture.Canvas.TextWidth
            (TextDefs[i].slText), dy + TextDefs[i].YPos * SCALE_Y +
            aTexture.Canvas.TextHeight(TextDefs[i].slText));

          aTexture.Canvas.Fill.Color := aColor;
          aTexture.Canvas.FillText(R, TextDefs[i].slText, False, 1, [],
            TTextAlign.Leading, TTextAlign.Leading);
        end;
      end;

      for i := 0 to High(LineDefs) do
      begin
        if LineDefs[i].ModuleID = aModuleDef.ModuleID then
        begin
          case LineDefs[i].LineWidth of
            lwThin:
              d := 1;
            lwThick:
              d := 3;
          end;

          aTexture.Canvas.StrokeThickness := d * SCALE_X;

          lx := d / 2;
          ly := d / 2;

          case LineDefs[i].Orientation of
            otVertical:
              begin
                aTexture.Canvas.DrawLine
                  (PointF((lx + LineDefs[i].XPos) * SCALE_X,
                  dy + (ly + LineDefs[i].YPos) * SCALE_Y),
                  PointF((lx + LineDefs[i].XPos) * SCALE_X,
                  dy + (ly + LineDefs[i].YPos + LineDefs[i].Length - d) *
                  SCALE_Y), 1);
              end;
            otHorizontal:
              begin
                aTexture.Canvas.DrawLine
                  (PointF((lx + LineDefs[i].XPos) * SCALE_X,
                  dy + (ly + LineDefs[i].YPos) * SCALE_Y),
                  PointF((lx + LineDefs[i].XPos + LineDefs[i].Length - d) *
                  SCALE_X, dy + (ly + LineDefs[i].YPos) * SCALE_Y), 1);
              end;
          end;

        end;
      end;

    finally
      aTexture.Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * UNITS_COL;
  TileHeight := SCALE_Y * UNITS_ROW * aModuleDef.Height;
  aTexture.SetSize(TileWidth * Cols, TileHeight * Rows);

  aTexture.Clear(0);
  DrawModule(0, TAlphaColorRec.Black); // Normal
  DrawModule(1, TAlphaColorRec.White); // Selected
end;

procedure CreateTextures(aTextureList: TTextureListGL; const aFileName: string;
  aLog: TStrings);
var
  Texture: TTiledTexture;
  Font: TFont;
  i: integer;
  id: string;
  sl: TStringList;
  Zip: TZipFile;
  Stream: TStream;
  LocalHeader: TZipHeader;
  RDial, RKnob, RBtns, RCntr: TRectF;

  procedure AddLog(const aText: string);
  begin
    if assigned(aLog) then
      aLog.Add(aText)
  end;

  procedure LoadFromZip(const aTexFileName: string; aTexture: TTiledTexture);
  begin
    Zip.Read(aTexFileName, Stream, LocalHeader);
    try
{$IFDEF ANDROID}
      aTexture.LoadFromStream(Stream);
      aTexture.Canvas.BeginScene;
      try
        aTexture.Canvas.DrawLine(PointF(0.0, 0.0), PointF(0.1, 0.0), 1);
      finally;
        aTexture.Canvas.EndScene;
      end;
{$ELSE}
      aTexture.LoadFromStream(Stream);
{$ENDIF}
    finally
      Stream.Free;
    end;
  end;

  procedure CreateBtn(const aWidth, aHeight: integer);
  begin
    id := GetBtnTexID(aWidth, aHeight);

    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;
      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture)
      end
      else
      begin

        AddLog(id);

        Texture.CreateBtn(aWidth, aHeight);
      end;
      aTextureList.Add(id, Texture);
    end;
  end;

  procedure CreateBtnText(const aWidth, aHeight: integer;
    const slImageIDS, slTexts: string);
  begin
    id := GetBtnTextTexID(aWidth, aHeight, slImageIDS, slTexts);

    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;
      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture)
      end
      else
      begin

        AddLog(id);

        Texture.CreateBtnText(aWidth, aHeight, slImageIDS, slTexts);
      end;
      aTextureList.Add(id, Texture);
    end;
  end;

  procedure CreateBtnIncDec(const aWidth, aHeight: integer;
    const aBtnType: TButtonIncDecType);
  begin
    id := GetBtnIncDecTexID(aWidth, aHeight, aBtnType = bidLeftRight);

    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;

      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture);
      end
      else
      begin

        AddLog(id);

        Texture.CreateBtnIncDec(aWidth, aHeight, aBtnType = bidLeftRight);
      end;
      aTextureList.Add(id, Texture);
    end;
  end;

  procedure CreateBtnRadio(const aWidth, aHeight: integer;
    const slImageIDS, slTexts: string);
  begin
    id := GetBtnRadioTexID(aWidth, aHeight, slImageIDS, slTexts);

    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;
      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture)
      end
      else
      begin

        AddLog(id);

        Texture.CreateBtnRadio(aWidth, aHeight, slImageIDS, slTexts);
      end;
      aTextureList.Add(id, Texture);
    end;

  end;

  procedure CreateKnob(const aKnobType: TKnobType);
  begin
    id := GetKnobIndicatorTexID(aKnobType);

    GetKnobMetrics(aKnobType, RDial, RKnob, RBtns, RCntr);

    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;
      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture)
      end
      else
      begin

        AddLog(id);

        if aKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
          Texture.CreateSliderIndicator(RDial, RKnob, RBtns,
            aKnobType = ktHSlider)
        else
          Texture.CreateKnobIndicator(RDial, RKnob);
      end;
      aTextureList.Add(id, Texture);
    end;

    id := GetKnobTexID(aKnobType);

    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;
      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture)
      end
      else
      begin

        AddLog(id);

        if aKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
          Texture.CreateSlider(RDial, RKnob, RBtns, False)
        else
          Texture.CreateKnob(RDial, RKnob);
      end;
      aTextureList.Add(id, Texture);
    end;
  end;

  procedure CreateLabel(const aFontFamily: string; const aFontSize: integer;
    const aFontStyle: TFontStyles; const aText: string);
  begin
    id := GetLabelTexID(aFontSize, aText);
    if not aTextureList.ContainsKey(id) then
    begin
      Texture := TTiledTexture.Create;
      if aFileName <> '' then
      begin
        LoadFromZip(id + '.png', Texture)
      end
      else
      begin
        AddLog(id);
        Texture.CreateLabel(aFontFamily, aFontSize, aFontStyle, aText);
      end;
      aTextureList.Add(id, Texture);
    end;
  end;

begin
  Zip := TZipFile.Create;
  sl := TStringList.Create;
  try
    if aFileName <> '' then
      Zip.Open(aFileName, zmRead);

    sl.Delimiter := ';';
    sl.StrictDelimiter := True;

    for i := 0 to High(ModuleDefs) do
    begin
      // Previews
      if aFileName <> '' then
      begin
        id := 'prev_' + IntToStr(ModuleDefs[i].ModuleID);
        Texture := TTiledTexture.Create;

        LoadFromZip(id + '.png', Texture);

        aTextureList.Add(id, Texture);
      end;
    end;

    Texture := TTiledTexture.Create;
    id := 'ModSeg';
    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin

      AddLog(id);

      Texture.CreateModuleSegment;
    end;
    aTextureList.Add(id, Texture);

    for i := 0 to High(TextDefs) do
      CreateLabel(FONT_FAMILY, TextDefs[i].FontSize, [], TextDefs[i].slText);

    for i := 0 to High(ButtonTextDefs) do
    begin
      CreateBtnText(ButtonTextDefs[i].Width, 12, ButtonTextDefs[i].slImageID,
        ButtonTextDefs[i].slText);
    end;

    for i := 0 to High(ButtonFlatDefs) do
    begin
      id := GetBtnFlatTexID(ButtonFlatDefs[i].Width, 12,
        ButtonFlatDefs[i].slImageID, ButtonFlatDefs[i].slText);

      if not aTextureList.ContainsKey(id) then
      begin
        if ButtonFlatDefs[i].slImageID <> '' then
          sl.DelimitedText := ButtonFlatDefs[i].slImageID
        else
          sl.DelimitedText := ButtonFlatDefs[i].slText;

        Texture := TTiledTexture.Create;
        if aFileName <> '' then
        begin
          LoadFromZip(id + '.png', Texture)
        end
        else
        begin

          AddLog(id);

          Texture.CreateBtnFlat(ButtonFlatDefs[i].Width, 12,
            ButtonFlatDefs[i].slImageID, ButtonFlatDefs[i].slText);
        end;
        aTextureList.Add(id, Texture);
      end;
    end;

    for i := 0 to High(LevelShiftDefs) do
    begin
      id := GetBtnLevelShiftTexID(13, 12, LevelShiftDefs[i].slImageID);

      if not aTextureList.ContainsKey(id) then
      begin
        sl.DelimitedText := LevelShiftDefs[i].slImageID;

        Texture := TTiledTexture.Create;

        if aFileName <> '' then
        begin
          LoadFromZip(id + '.png', Texture)
        end
        else
        begin

          AddLog(id);

          Texture.CreateBtnLevelShift(13, 12, LevelShiftDefs[i].slImageID);
        end;
        aTextureList.Add(id, Texture);
      end;
    end;

    for i := 0 to High(ButtonRadioDefs) do
    begin
      CreateBtnRadio(ButtonRadioDefs[i].ButtonWidth, 12,
        ButtonRadioDefs[i].slImageID, ButtonRadioDefs[i].slText);
    end;

    for i := 0 to High(ButtonIncDecDefs) do
      CreateBtnIncDec(13, 12, ButtonIncDecDefs[i].ButtonType);

    for i := 0 to High(TextEditDefs) do
      CreateBtn(TextEditDefs[i].Width, 10);

    for i := 0 to High(TextFieldDefs) do
    begin
      id := GetTextFieldTexID(TextFieldDefs[i].Width, 10);

      if not aTextureList.ContainsKey(id) then
      begin
        Texture := TTiledTexture.Create;
        if aFileName <> '' then
        begin
          LoadFromZip(id + '.png', Texture)
        end
        else
        begin

          AddLog(id);

          Texture.CreateTextField(TextFieldDefs[i].Width, 10);
        end;
        aTextureList.Add(id, Texture);
      end;
    end;

    for i := 0 to High(GraphDefs) do
    begin
      id := GetGraphTexID(GraphDefs[i].Width, GraphDefs[i].Height);

      if not aTextureList.ContainsKey(id) then
      begin
        Texture := TTiledTexture.Create;
        if aFileName <> '' then
        begin
          LoadFromZip(id + '.png', Texture)
        end
        else
        begin

          AddLog(id);

          Texture.CreateGraph(GraphDefs[i].Width, GraphDefs[i].Height);
        end;
        aTextureList.Add(id, Texture);
      end;
    end;

    for i := 0 to High(PartSelectorDefs) do
    begin
      id := GetPartSelectorListTexID(PartSelectorDefs[i].ImageWidth,
        PartSelectorDefs[i].Height, PartSelectorDefs[i].slImageID,
        PartSelectorDefs[i].slText);

      if not aTextureList.ContainsKey(id) then
      begin
        Texture := TTiledTexture.Create;
        if aFileName <> '' then
          LoadFromZip(id + '.png', Texture)
        else
        begin

          AddLog(id);

          Texture.CreatePartSelectorList(PartSelectorDefs[i].ImageWidth,
            PartSelectorDefs[i].Height, PartSelectorDefs[i].slImageID,
            PartSelectorDefs[i].slText);
        end;
        aTextureList.Add(id, Texture);
      end;
    end;

    for i := 0 to High(PartSelectorDefs) do
    begin
      id := GetPartSelectorBtnTexID(8, PartSelectorDefs[i].Height);

      if not aTextureList.ContainsKey(id) then
      begin
        Texture := TTiledTexture.Create;
        if aFileName <> '' then
        begin
          LoadFromZip(id + '.png', Texture);
        end
        else
        begin

          AddLog(id);

          Texture.CreatePartSelectorBtn(8, PartSelectorDefs[i].Height);
        end;
        aTextureList.Add(id, Texture);
      end;
    end;

    Texture := TTiledTexture.Create;
    id := GetLedTexID(False);

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);
      Texture.CreateLedGreen;
    end;
    aTextureList.Add(id, Texture);

    Texture := TTiledTexture.Create;
    id := GetLedTexID(True);

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);
      Texture.CreateLedSequencer;
    end;
    aTextureList.Add(id, Texture);

    Texture := TTiledTexture.Create;
    id := GetMiniVUTexID;

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);
      Texture.CreateMiniVU;
    end;
    aTextureList.Add(id, Texture);

    // Glyph texture
    Texture := TTiledTexture.Create;
    id := GetFontTexID;

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);

      Font := TFont.Create;
      try
        Font.Family := 'Roboto Mono';
        Font.Style := [TFontStyle.fsBold];
        Font.Size := 8 * SCALE_Y;
        Texture.CreateFont(Font);
      finally
        Font.Free;
      end;
    end;
    aTextureList.Add(id, Texture);

    // In connectors
    Texture := TTiledTexture.Create;
    id := GetConnectorInTexID;

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);
      Texture.CreateConnectorIn;
    end;
    aTextureList.Add(id, Texture);

    // Out connectors
    Texture := TTiledTexture.Create;
    id := GetConnectorOutTexID;

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);
      Texture.CreateConnectorOut;
    end;
    aTextureList.Add(id, Texture);

    for i := 0 to High(KnobDefs) do
    begin
      CreateKnob(KnobDefs[i].KnobType);
    end;

    // Centering buttons
    Texture := TTiledTexture.Create;
    id := 'CenterBtn';

    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      RCntr := RectF(0, 0, 10, 4);
      Texture.CreateCenterBtn(RCntr);
    end;
    aTextureList.Add(id, Texture);

    Texture := TTiledTexture.Create;
    id := 'KnobBtns';

    // Knobs
    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      RBtns := RectF(0, 0, 22, 9);
      Texture.CreateKnobBtns(RBtns);
    end;
    aTextureList.Add(id, Texture);

    // Sliders
    Texture := TTiledTexture.Create;
    id := 'SliderBtns';

    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      RBtns := RectF(0, 0, 11, 16);
      Texture.CreateSliderBtns(RBtns, False);
    end;
    aTextureList.Add(id, Texture);

    // Menu left
    Texture := TTiledTexture.Create;
    id := 'MenuBtnLeft';

    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      Texture.CreateMenuLeft;
    end;
    aTextureList.Add(id, Texture);

    // Menu right
    Texture := TTiledTexture.Create;
    id := 'MenuBtnRight';

    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      Texture.CreateMenuRight;
    end;
    aTextureList.Add(id, Texture);

    // BtnRadioEdit
    CreateBtn(43, 12);

    // Banks
    CreateBtn(36, 12); // Banks BtnFileType
    CreateBtn(18, 12); // Banks Bank
    CreateBtn(40, 12); // Banks BankSlot

    // Slot labels
    CreateLabel('Roboto Mono', 12, [TFontStyle.fsBold], 'A');
    CreateLabel('Roboto Mono', 12, [TFontStyle.fsBold], 'B');
    CreateLabel('Roboto Mono', 12, [TFontStyle.fsBold], 'C');
    CreateLabel('Roboto Mono', 12, [TFontStyle.fsBold], 'D');

    // Next, prev. menu
    // CreateBtnText(12, 12, '2;2', '');
    // CreateBtnText(12, 12, '3;3', '');

    // PerfSlot
    CreateBtn(43, 12);
    CreateBtnRadio(10, 12, '', '1;2;3;4;5;6;7;8');
    CreateBtnText(24, 12, '', 'Edit;Edit');
    CreateKnob(TKnobType.ktHSlider);
    CreateBtnText(24, 12, '', 'Mute;On');
    CreateBtnText(24, 12, '', 'Keyb;Keyb');
    CreateBtnText(24, 12, '', 'Hold;Hold');
    CreateBtnText(24, 12, '', 'Enab;Enab');

    CreateBtnText(24, 12, '', 'Run;Run');
    CreateBtnText(24, 12, '', 'Perf;Perf');

    CreateBtnIncDec(13, 12, bidLeftRight);
    CreateBtnRadio(16, 12, '', 'FX;VA;Pch');

    // Arp
    CreateBtnText(24, 12, '', 'Arp off;Arp on');
    CreateBtnRadio(24, 12, '', '1/8;1/8T;1/16;1/16T');
    CreateBtnRadio(24, 12, '', 'Up;Down;Up/Dn;Rnd');
    CreateBtnRadio(24, 12, '', '4 oct;3 oct; 2 oct; 1 oct');

    // FormSlot
    CreateBtnText(24, 12, '', 'Add;Add');

    // SelectModule
    CreateBtn(32, 12);

    // Rack
    Texture := TTiledTexture.Create;
    id := 'RackRails';

    if aFileName <> '' then
      LoadFromZip(id + '.png', Texture)
    else
    begin
      AddLog(id);
      Texture.CreateRackRails;
    end;
    aTextureList.Add(id, Texture);

    // Keyboard
    Texture := TTiledTexture.Create;
    id := 'KeyWhite';

    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      Texture.CreateWhiteKeys(16, 40, 10, 24);
    end;
    aTextureList.Add(id, Texture);

    Texture := TTiledTexture.Create;
    id := 'KeyBlack';

    if aFileName <> '' then
    begin
      LoadFromZip(id + '.png', Texture)
    end
    else
    begin
      AddLog(id);
      Texture.CreateBlackKey(10, 24);
    end;
    aTextureList.Add(id, Texture);

  finally
    sl.Free;
    Zip.Free;
  end;
end;

procedure CreateModulePreview(aModuleDef: TG2ModuleDef;
  aTextureList: TTextureListGL; aTexture: TTiledTexture);
var
  TempViewPort: TBufferedViewPortGL;
  ModulePanel: TG2ModuleGL;
  Module: IG2Module;
  R: TRect;
  Scale: Single;
begin
  Scale := 1;
  TempViewPort := TBufferedViewPortGL.Create(nil);
  try
    R := Rect(0, 0, Round(UNITS_COL * Scale),
      Round(UNITS_ROW * aModuleDef.Height * Scale));

    TempViewPort.Width := R.Width;
    TempViewPort.Height := R.Height;

    TempViewPort.Scale3D := Point3D(Scale / SCALE_X, Scale / SCALE_Y, 1);

    Module := TG2FileModule.Create(nil, aModuleDef.ModuleID, 0);
    Module.InitModule;

    ModulePanel := TG2ModuleGL.Create(nil);
    ModulePanel.TextureList := aTextureList;
    ModulePanel.Parent := TempViewPort;
    ModulePanel.Module := Module;
    ModulePanel.CreateModuleControls(True, True, TKnobControl.kcCircular, nil);

    aTexture.SetSize(R.Width, R.Height);
    TempViewPort.Render(aTexture);

    ModulePanel.Module := nil;
    Module := nil;

    TempViewPort.Context.CopyToBitmap(aTexture, R);
  finally
    TempViewPort.Free;
  end;
end;

procedure CreateModulePreviews(aTextureList: TTextureListGL; aLog: TStrings);
var
  i: integer;
  id: string;
  Texture: TTiledTexture;

  procedure AddLog(const aText: string);
  begin
    if assigned(aLog) then
      aLog.Add(aText)
  end;

begin
  for i := 0 to High(ModuleDefs) do
  begin
    // Create module preview

    id := 'prev_' + IntToStr(ModuleDefs[i].ModuleID);

    AddLog(id);

    if not aTextureList.Link(id, Texture) then
    begin
      Texture := TTiledTexture.Create;
      aTextureList.Add(id, Texture);
    end;
    CreateModulePreview(ModuleDefs[i], aTextureList, Texture);

    // Create module label

    id := GetLabelTexID(12, ModuleDefs[i].ModuleName);

    AddLog(id);

    Texture := TTiledTexture.Create;
    Texture.CreateLabel(FONT_FAMILY, 12, [], ModuleDefs[i].ModuleName);
    aTextureList.Add(id, Texture);
  end;
end;

{ TG2ModuleGL }

constructor TG2ModuleGL.Create(AOwner: TComponent);
begin
  inherited;

  TexRows := 8;
  TexCols := 1;

  AutoCapture := True;

  FModuleLabel := TMatrixTextObject.Create;
  FModuleLabel.TextureID := GetFontTexID;
  FModuleLabel.TextAlign := TTextAlign.Leading;

  FLines := TObjectList<TColorObject>.Create(True);
  FLabels := TObjectList<TTexObject>.Create(True);

  FSelected := False;

  SetWeak(@FWModule, nil);
end;

destructor TG2ModuleGL.Destroy;
begin
  if assigned(Module) then
    Module := nil;

  FreeAndNil(FModuleLabel);
  FLabels.Free;
  FLines.Free;

  inherited;
end;

procedure TG2ModuleGL.Init;
begin
  if not assigned(Module) then
    exit;

  Position := Point3D(
    Module.Col * (UNITS_COL + UNIT_MARGIN) * SCALE_X,
    Module.Row * (UNITS_ROW + UNIT_MARGIN) * SCALE_Y,
    0);

  // Width := UNITS_COL * SCALE_X;
  // Height := Module.HeightUnits * UNITS_ROW * SCALE_Y;

  ColCount := 1;
  RowCount := Module.HeightUnits * 2;

  Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);
  Caption := Module.ModuleName;

  Selected := Module.Selected;

  // TextureID := GetModuleTexID(Module.TypeID);
  TextureID := 'ModSeg';
  Invalidate;
end;

procedure TG2ModuleGL.InvalidateConnectors;
var
  FMXObject: TFMXObject;
begin
  if not assigned(Children) then
    exit;

  for FMXObject in Children do
  begin
    if FMXObject is TG2ConnectorGL then
    begin
      (FMXObject as TG2ConnectorGL).InvalidateCables;
    end;
  end;
end;

function TG2ModuleGL.GetCaption: string;
begin
  Result := FModuleLabel.Text;
end;

{ function TG2ModuleGL.GetColor: TAlphaColor;
  begin
  Result := FBackGrndMat.Color;
  end; }

{procedure TG2ModuleGL.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;

  if ssLeft in Shift then
  begin
    Selected := True;
  end;
end;}

procedure TG2ModuleGL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  i: integer;
begin
  inherited;

  SaveMatrix := aContext.CurrentMatrix;
  try
    for i := 0 to FLines.Count - 1 do
    begin
      aContext.SetMatrix(
          TMatrix3D.CreateTranslation(FLines[i].Position)
        * SaveMatrix);
      FLines[i].Render(aContext);
    end;

    for i := 0 to FLabels.Count - 1 do
    begin
      aContext.SetMatrix(
          TMatrix3D.CreateTranslation(FLabels[i].Position)
        * SaveMatrix);
      FLabels[i].Render(aContext);
    end;

    aContext.SetMatrix(
        TMatrix3D.CreateTranslation(Point3D(2 * SCALE_X, 2 * SCALE_Y, 0))
      * SaveMatrix);
    FModuleLabel.Render(aContext);
  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

procedure TG2ModuleGL.SetCaption(const Value: string);
begin
  FModuleLabel.Text := Value;
  FModuleLabel.Width := FModuleLabel.TextWidth;
  FModuleLabel.Height := FModuleLabel.TextHeight;
end;

procedure TG2ModuleGL.SetSelected(const Value: boolean);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    if FSelected then
      ImageIndex := 1
    else
      ImageIndex := 0;

    UpdateCellIndex;

    if assigned(FWModule) then
      FWModule.Selected := FSelected;

    if assigned(FOnSelect) then
      FOnSelect(self);
  end;
end;

procedure TG2ModuleGL.SetTextureList(const Value: TTextureListGL);
var
  i: integer;
begin
  inherited;

  for i := 0 to FLabels.Count - 1 do
    FLabels[i].TextureList := Value;

  FModuleLabel.TextureList := Value;
end;

procedure TG2ModuleGL.SetModule(const Value: IG2Module);
begin
  if FWModule <> Value then
  begin
    if assigned(FWModule) then
      FWModule.RemoveObserver(self);

    SetWeak(@FWModule, Value);

    if assigned(FWModule) then
    begin
      Init;
      FWModule.RegisterObserver(self);
    end;
  end;
end;

procedure TG2ModuleGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtModuleColor:
      begin
        if assigned(Module) then
        begin
          Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);
        end;
      end;
    EvtModuleAdd:
      ;
    EvtModuleDelete, EvtDestroy:
      begin
        // G2 Object will be destroyed
        Module := nil;
      end;
    EvtDisposeControl:
      begin
        // Signal that panel must be destroyed
      end;
    EvtModuleLabel:
      begin
        if assigned(Module) then
        begin
          // ModuleLabel := FWModule.ModuleName;
          // Repaint;
        end;
      end;
  end;
end;

procedure TG2ModuleGL.UpdateCellIndex;
var
  i, Offs: integer;
begin
  if Selected then
    Offs := 4
  else
    Offs := 0;

  if RowCount > 1 then
  begin
    CellIndex[0] := Offs + 0;
    i := 1;
    while (i < RowCount - 1) do
    begin
      CellIndex[i] := Offs + 1;
      inc(i);
    end;
    CellIndex[RowCount - 1] := Offs + 3;
  end;
end;

procedure TG2ModuleGL.CreateModuleControls(aConnectParams, aConnectConnectors
  : boolean; aKnobControl: TKnobControl;
  ProcCreateControl: TCreateG2ControlEvent);
var
  c: integer;

  procedure SetCommonProperties(aControl: TTexControlGL;
    aID, aXPos, aYPos: Single);
  begin
    aControl.Parent := self;
    aControl.Position := Point3D(aXPos * SCALE_X, aYPos * SCALE_Y, 0);

    if assigned(ProcCreateControl) then
      ProcCreateControl(nil, FWModule, aControl);
  end;

  function CreateInput(aConnectorDef: TG2ConnectorDef): TG2ConnectorGL;
  var
    Connector: IG2Connector;
  begin
    Connector := FWModule.InConnector[aConnectorDef.CodeRef];
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aConnectorDef.CodeRef) +
        ' not found in module ' + FWModule.ModuleName);

    Result := TG2ConnectorGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetConnectorInTexID;

    SetCommonProperties(Result, aConnectorDef.id, aConnectorDef.XPos,
      aConnectorDef.YPos);

    Result.Connector := Connector;
  end;

  function CreateOutput(aConnectorDef: TG2ConnectorDef): TG2ConnectorGL;
  var
    Connector: IG2Connector;
  begin
    Connector := FWModule.OutConnector[aConnectorDef.CodeRef];
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aConnectorDef.CodeRef) +
        ' not found in module ' + FWModule.ModuleName);

    Result := TG2ConnectorGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetConnectorOutTexID;

    SetCommonProperties(Result, aConnectorDef.id, aConnectorDef.XPos,
      aConnectorDef.YPos);

    Result.Connector := Connector;
  end;

  function GetLed(aGroupID: integer): IG2Led;
  var
    PatchPart: IG2PatchPart;
    i: integer;
  begin
    Result := nil;

    if not assigned(FWModule.PatchPart) then
      exit;

    PatchPart := FWModule.PatchPart;

    if assigned(PatchPart) then
    begin
      i := 0;
      while (i < PatchPart.LedList.Count) and
        not((PatchPart.LedList[i].ModuleIndex = FWModule.ModuleIndex) and
        (PatchPart.LedList[i].GroupID = aGroupID)) do
        inc(i);

      if (i < PatchPart.LedList.Count) then
        Result := PatchPart.LedList[i]
    end;
  end;

  function GetLedStrip(aGroupID: integer): IG2Led;
  var
    PatchPart: IG2PatchPart;
    i: integer;
  begin
    Result := nil;
    if not assigned(FWModule.PatchPart) then
      exit;

    PatchPart := FWModule.PatchPart;

    if assigned(PatchPart) then
    begin
      i := 0;
      while (i < PatchPart.LedStripList.Count) and
        not((PatchPart.LedStripList[i].ModuleIndex = FWModule.ModuleIndex) and
        (PatchPart.LedStripList[i].GroupID = aGroupID)) do
        inc(i);

      if (i < PatchPart.LedStripList.Count) then
        Result := PatchPart.LedStripList[i];
    end;
  end;

  function CreateLed(aLedDef: TG2LedDef): TG2LedGL;
  begin
    Result := TG2LedGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetLedTexID(aLedDef.LedType = ltSequencer);

    SetCommonProperties(Result, aLedDef.id, aLedDef.XPos, aLedDef.YPos);

    case aLedDef.LedType of
      ltSequencer:
        begin
          Result.CodeRef := aLedDef.CodeRef;
          Result.Led := GetLedStrip(aLedDef.GroupID);
        end;
      ltGreen:
        begin
          Result.CodeRef := aLedDef.CodeRef;
          if GetDataLedsInGroup(FWModule.TypeID, aLedDef.GroupID) > 1 then
            Result.Led := GetLedStrip(aLedDef.GroupID)
          else
            Result.Led := GetLed(aLedDef.GroupID);
        end;
    end;
  end;

  function CreateMiniVU(aMiniVUDef: TG2MiniVUDef): TG2MiniVUGL;
  begin
    Result := TG2MiniVUGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetMiniVUTexID;

    SetCommonProperties(Result, aMiniVUDef.id, aMiniVUDef.XPos,
      aMiniVUDef.YPos);

    Result.Led := GetLedStrip(aMiniVUDef.GroupID);
  end;

  function CreateTextField(aTextFieldDef: TG2TextFieldDef): TG2TextFieldGL;
  var
    G2Function: IG2Function;
  begin
    Result := TG2TextFieldGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetTextFieldTexID(aTextFieldDef.Width, 10);

    // Result.Width := aTextFieldDef.Width * SCALE_X;
    // Result.Height := 10 * SCALE_Y;

    SetCommonProperties(Result, aTextFieldDef.id, aTextFieldDef.XPos,
      aTextFieldDef.YPos);

    G2Function := FWModule.AddTextFunction(aTextFieldDef.TextFunc,
      aTextFieldDef.MasterRef, aTextFieldDef.slDependencies);
    Result.Func := G2Function;
  end;

  function CreateGraph(aGraphDef: TG2GraphDef): TG2GraphGL;
  var
    G2Function: IG2Function;
  begin
    Result := TG2GraphGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetGraphTexID(aGraphDef.Width, aGraphDef.Height);

    // Result.Width := aGraphDef.Width * SCALE_X;
    // Result.Height := aGraphDef.Height * SCALE_Y;

    SetCommonProperties(Result, aGraphDef.id, aGraphDef.XPos, aGraphDef.YPos);

    // G2Function := FWModule.AddTextFunction(aGraphDef.TextFunc,
    // aGraphDef.MasterRef, aGraphDef.slDependencies);
    Result.Func := G2Function;
  end;

  function CreateTextEdit(aTextEditDef: TG2TextEditDef): TG2TextEditGL;
  begin
    Result := TG2TextEditGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnTexID(aTextEditDef.Width, 10);

    SetCommonProperties(Result, aTextEditDef.id, aTextEditDef.XPos,
      aTextEditDef.YPos);

    Result.BtnType := aTextEditDef.TextEditType;

    Result.Caption := aTextEditDef.slText;

    Result.Param := FWModule.Param[aTextEditDef.CodeRef];
    FWModule.Param[aTextEditDef.CodeRef].InfoFunctionIndex :=
      aTextEditDef.InfoFunc;
  end;

  function CreateKnob(aKnobDef: TG2KnobDef): TG2KnobGL;
  begin
    Result := TG2KnobGL.Create(self);
    Result.KnobType := aKnobDef.KnobType;
    Result.TextureList := TextureList;

    SetCommonProperties(Result, aKnobDef.id, aKnobDef.XPos, aKnobDef.YPos);

    Result.Param := FWModule.Param[aKnobDef.CodeRef];
    FWModule.Param[aKnobDef.CodeRef].InfoFunctionIndex := aKnobDef.InfoFunc;
  end;

  function CreateButtonText(aButtonTextDef: TG2ButtonTextDef): TG2BtnTextGL;
  begin
    Result := TG2BtnTextGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnTextTexID(aButtonTextDef.Width, 12,
      aButtonTextDef.slImageID, aButtonTextDef.slText);

    SetCommonProperties(Result, aButtonTextDef.id, aButtonTextDef.XPos,
      aButtonTextDef.YPos);

    Result.Param := FWModule.Param[aButtonTextDef.CodeRef];
    FWModule.Param[aButtonTextDef.CodeRef].InfoFunctionIndex :=
      aButtonTextDef.InfoFunc;
  end;

  function CreateButtonFlat(aButtonFlatDef: TG2ButtonFlatDef): TG2BtnFlatGL;
  begin
    Result := TG2BtnFlatGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnFlatTexID(aButtonFlatDef.Width, 12,
      aButtonFlatDef.slImageID, aButtonFlatDef.slText);

    SetCommonProperties(Result, aButtonFlatDef.id, aButtonFlatDef.XPos,
      aButtonFlatDef.YPos);

    Result.Param := FWModule.Param[aButtonFlatDef.CodeRef];
    FWModule.Param[aButtonFlatDef.CodeRef].InfoFunctionIndex :=
      aButtonFlatDef.InfoFunc;
  end;

  function CreateButtonIncDec(aButtonIncDecDef: TG2ButtonIncDecDef)
    : TG2BtnIncDecGL;
  begin
    Result := TG2BtnIncDecGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnIncDecTexID(13, 12,
      aButtonIncDecDef.ButtonType = bidLeftRight);

    case aButtonIncDecDef.ButtonType of
      bidLeftRight:
        begin
          Result.ColCount := 2;
          Result.RowCount := 1;
        end;
      bidUpDown:
        begin
          Result.ColCount := 1;
          Result.RowCount := 2;
        end;
    else
      raise Exception.Create('Unknown button type');
    end;

    SetCommonProperties(Result, aButtonIncDecDef.id, aButtonIncDecDef.XPos,
      aButtonIncDecDef.YPos);

    Result.Param := FWModule.Param[aButtonIncDecDef.CodeRef];
    FWModule.Param[aButtonIncDecDef.CodeRef].InfoFunctionIndex :=
      aButtonIncDecDef.InfoFunc;
  end;

  function CreateLevelShift(aLevelShiftDef: TG2LevelShiftDef)
    : TG2BtnLevelShiftGL;
  begin
    Result := TG2BtnLevelShiftGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnLevelShiftTexID(13, 12, aLevelShiftDef.slImageID);

    SetCommonProperties(Result, aLevelShiftDef.id, aLevelShiftDef.XPos,
      aLevelShiftDef.YPos);

    Result.Param := FWModule.Param[aLevelShiftDef.CodeRef];
    FWModule.Param[aLevelShiftDef.CodeRef].InfoFunctionIndex :=
      aLevelShiftDef.InfoFunc;
  end;

  function CreateButtonRadio(aButtonRadioDef: TG2ButtonRadioDef): TG2BtnRadioGL;
  begin
    Result := TG2BtnRadioGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnRadioTexID(aButtonRadioDef.ButtonWidth, 12,
      aButtonRadioDef.slImageID, aButtonRadioDef.slText);

    case aButtonRadioDef.Orientation of
      otHorizontal:
        begin
          Result.Horizontal := True;
          Result.ColCount := aButtonRadioDef.ButtonCount;
          Result.RowCount := 1;
        end;
      otVertical:
        begin
          Result.Horizontal := False;
          Result.ColCount := 1;
          Result.RowCount := aButtonRadioDef.ButtonCount;
          Result.UpsideDown := True;
        end;
    else
      raise Exception.Create('Unknown orientation type');
    end;

    SetCommonProperties(Result, aButtonRadioDef.id, aButtonRadioDef.XPos,
      aButtonRadioDef.YPos);

    Result.Param := FWModule.Param[aButtonRadioDef.CodeRef];
    FWModule.Param[aButtonRadioDef.CodeRef].InfoFunctionIndex :=
      aButtonRadioDef.InfoFunc;
  end;

  function CreateButtonRadioEdit(aButtonRadioEditDef: TG2ButtonRadioEditDef)
    : TG2BtnRadioEditGL;
  begin
    Result := TG2BtnRadioEditGL.Create(self);

    Result.ColCount := aButtonRadioEditDef.ButtonColumns;
    Result.RowCount := aButtonRadioEditDef.ButtonRows;
    Result.TextureList := TextureList;
    Result.TextureID := GetBtnTexID(43, 12);

    SetCommonProperties(Result, aButtonRadioEditDef.id,
      aButtonRadioEditDef.XPos, aButtonRadioEditDef.YPos);

    Result.Texts := aButtonRadioEditDef.slText;

    Result.Param := FWModule.Param[aButtonRadioEditDef.CodeRef];
    FWModule.Param[aButtonRadioEditDef.CodeRef].InfoFunctionIndex :=
      aButtonRadioEditDef.InfoFunc;
  end;

  function CreatePartSelector(aPartSelectorDef: TG2PartSelectorDef)
    : TG2SelectorGL;
  begin
    Result := TG2SelectorGL.Create(self);
    Result.TextureList := TextureList;
    Result.TextureID := GetPartSelectorListTexID(aPartSelectorDef.ImageWidth,
      aPartSelectorDef.Height, aPartSelectorDef.slImageID,
      aPartSelectorDef.slText);
    Result.List.TextureID := Result.TextureID;
    Result.Btn.TextureID := GetPartSelectorBtnTexID(8, aPartSelectorDef.Height);

    SetCommonProperties(Result, aPartSelectorDef.id, aPartSelectorDef.XPos,
      aPartSelectorDef.YPos);

    Result.List.ColCount := 1;
    Result.List.RowCount := aPartSelectorDef.ImageCount;

    Result.Param := FWModule.Mode[aPartSelectorDef.CodeRef];
    // FWModule.Mode[aPartSelectorDef.CodeRef].InfoFunctionIndex := aPartSelectorDef.InfoFunc;
  end;

  procedure CreateLine(aLineDef: TG2LineDef);
  var
    d: Single;
    ColorObject: TColorObject;
  begin
    ColorObject := TColorObject.Create;
    FLines.Add(ColorObject);

    ColorObject.Color := TAlphaColorRec.Black;

    d := 1;
    case aLineDef.LineWidth of
      lwThin:
        d := 1;
      lwThick:
        d := 3;
    end;

    case aLineDef.Orientation of
      otVertical:
        begin
          ColorObject.Position := Point3D(aLineDef.XPos * SCALE_X,
            aLineDef.YPos * SCALE_Y, 0);
          ColorObject.Mesh.CreateMesh(d * SCALE_X,
            aLineDef.Length * SCALE_Y, 0, 1, 1);
        end;
      otHorizontal:
        begin
          ColorObject.Position := Point3D(aLineDef.XPos * SCALE_X,
            aLineDef.YPos * SCALE_Y, 0);
          ColorObject.Mesh.CreateMesh(aLineDef.Length * SCALE_X,
            d * SCALE_Y, 0, 1, 1);
        end;
    end;
  end;

  procedure CreateLabel(aTextDef: TG2TextDef);
  var
    TexObject: TTexObject;
  begin
    TexObject := TTexObject.Create;
    FLabels.Add(TexObject);
    TexObject.Position := Point3D(aTextDef.XPos * SCALE_X,
      aTextDef.YPos * SCALE_Y, 0);
    TexObject.TextureList := TextureList;
    TexObject.TextureID := GetLabelTexID(aTextDef.FontSize, aTextDef.slText);
  end;

begin
  for c := 0 to High(LineDefs) do
  begin
    if LineDefs[c].ModuleID = FWModule.TypeID then
    begin
      //
    end;
  end;

  for c := 0 to High(BitmapDefs) do
  begin
    if BitmapDefs[c].ModuleID = FWModule.TypeID then
    begin
      //
    end;
  end;

  for c := 0 to High(TextDefs) do
  begin
    if TextDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateLabel(TextDefs[c]);
    end;
  end;

  for c := 0 to High(ModuleInputs) do
  begin
    if ModuleInputs[c].ModuleID = FWModule.TypeID then
    begin
      CreateInput(ModuleInputs[c]);
    end;
  end;

  for c := 0 to High(ModuleOutputs) do
  begin
    if ModuleOutputs[c].ModuleID = FWModule.TypeID then
    begin
      CreateOutput(ModuleOutputs[c]);
    end;
  end;

  for c := 0 to High(TextFieldDefs) do
  begin
    if TextFieldDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateTextField(TextFieldDefs[c]);
    end;
  end;

  for c := 0 to High(GraphDefs) do
  begin
    if GraphDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateGraph(GraphDefs[c]);
    end;
  end;

  for c := 0 to High(LedDefs) do
  begin
    if LedDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateLed(LedDefs[c]);
    end;
  end;

  for c := 0 to High(MiniVUDefs) do
  begin
    if MiniVUDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateMiniVU(MiniVUDefs[c]);
    end;
  end;

  for c := 0 to High(PartSelectorDefs) do
  begin
    if PartSelectorDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreatePartSelector(PartSelectorDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonTextDefs) do
  begin
    if ButtonTextDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateButtonText(ButtonTextDefs[c]);
    end;
  end;

  for c := 0 to High(TextEditDefs) do
  begin
    if TextEditDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateTextEdit(TextEditDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonFlatDefs) do
  begin
    if ButtonFlatDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateButtonFlat(ButtonFlatDefs[c]);
    end;
  end;

  for c := 0 to High(LineDefs) do
  begin
    if LineDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateLine(LineDefs[c]);
    end;
  end;

  for c := 0 to High(LevelShiftDefs) do
  begin
    if LevelShiftDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateLevelShift(LevelShiftDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonRadioDefs) do
  begin
    if ButtonRadioDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateButtonRadio(ButtonRadioDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonRadioEditDefs) do
  begin
    if ButtonRadioEditDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateButtonRadioEdit(ButtonRadioEditDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonIncDecDefs) do
  begin
    if ButtonIncDecDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateButtonIncDec(ButtonIncDecDefs[c]);
    end;
  end;

  for c := 0 to High(KnobDefs) do
  begin
    if KnobDefs[c].ModuleID = FWModule.TypeID then
    begin
      CreateKnob(KnobDefs[c]);
    end;
  end;
end;

{ TG2CableGL }

constructor TG2CableGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWCable, nil);
end;

destructor TG2CableGL.Destroy;
begin
  if assigned(Cable) then
    Cable := nil;

  inherited;
end;

procedure TG2CableGL.Init;
begin
  if not assigned(FConnector1) then
    exit;

  if not assigned(FConnector2) then
    exit;

  P0 := FConnector1.Point;
  P1 := FConnector2.Point;

  // Color := ConvertToAlpha(CableColors[ord(FWCable.CableColor)]);

  InitCable;
end;

procedure TG2CableGL.SetCable(const Value: IG2Cable);
begin
  if FWCable <> Value then
  begin
    if assigned(FWCable) then
      FWCable.RemoveObserver(self);

    SetWeak(@FWCable, Value);

    if assigned(FWCable) then
    begin
      Init;
      FWCable.RegisterObserver(self);
    end;
  end;
end;

procedure TG2CableGL.SetConnector1(const Value: TG2ConnectorGL);
begin
  if Value <> FConnector1 then
  begin
    FConnector1 := Value;
    Init;
  end;
end;

procedure TG2CableGL.SetConnector2(const Value: TG2ConnectorGL);
begin
  if Value <> FConnector2 then
  begin
    FConnector2 := Value;
    Init;
  end;
end;

procedure TG2CableGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtCableAdd:
      ;
    EvtCableDelete, EvtDestroy:
      begin
        // G2 Object will be destroyed
        Cable := nil;
      end;
    EvtDisposeControl:
      begin
        // Signal that ui cable must be destroyed
      end;
    EvtCableColor:
      ;
  end;
end;

{ TG2ConnectorGL }

constructor TG2ConnectorGL.Create(AOwner: TComponent);
begin
  inherited;
  TexRows := 2;
  TexCols := 4;

  SetWeak(@FWConnector, nil);
end;

destructor TG2ConnectorGL.Destroy;
begin
  if assigned(FWConnector) then
    Connector := nil;

  inherited;
end;

function TG2ConnectorGL.GetPoint: TPointF;
begin
  Result := PointF((Parent as TControlGL).Position.X + Position.X + Width / 2,
    (Parent as TControlGL).Position.Y + Position.Y + Height / 2);
end;

procedure TG2ConnectorGL.Init;
var
  Texture: TTiledTexture;
begin
  if assigned(FWConnector) then
  begin
    FWConnector.CalcDefColor;

    Texture := GetTexture;

    if FWConnector.Connected and assigned(Texture) then
      ImageIndex := ord(FWConnector.ConnectorColor) + TexCols
    else
      ImageIndex := ord(FWConnector.ConnectorColor);
  end;
end;

procedure TG2ConnectorGL.InvalidateCables;
var
  CableList: TList<IG2Cable>;
  Cable: IG2Cable;
begin
  if not assigned(FWConnector) then
    exit;

  CableList := FWConnector.CreateCableList;
  try
    for Cable in CableList do
      Cable.Invalidate;
  finally
    CableList.Free;
  end;
end;

procedure TG2ConnectorGL.SetConnector(const Value: IG2Connector);
begin
  if FWConnector <> Value then
  begin
    if assigned(FWConnector) then
      FWConnector.RemoveObserver(self);

    SetWeak(@FWConnector, Value);

    if assigned(FWConnector) then
    begin
      Init;
      FWConnector.RegisterObserver(self);
    end;
  end;
end;

procedure TG2ConnectorGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtCableAdd:
      begin
        Init;
      end;
    EvtCableDelete:
      begin
        Init;
      end;
    EvtCableColor:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Connector := nil;
      end;
    EvtDisposeControl:
      begin
        //
      end;
  end;
end;

{ TG2KnobGL }

constructor TG2KnobGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWParam, nil);
end;

destructor TG2KnobGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2KnobGL.Init;
begin
  if assigned(Param) then
  begin
    inherited Value := Param.Value;
    MorphValue := Param.GetSelectedMorphValue;
    MinValue := Param.LowValue;
    MaxValue := Param.HighValue;

    if KnobType in [ktSlider, ktSeqSlider, ktHSlider] then
      ShowBtns := True
    else
      ShowBtns := Param.Module.Focussed and Param.Selected;
  end;
end;

procedure TG2KnobGL.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    if assigned(Param) then
      Param.Selected := True;
  end;

  inherited;
end;

procedure TG2KnobGL.MouseLeave;
begin
  inherited;

  if not(KnobType in [ktSlider, ktSeqSlider, ktHSlider]) then
    if Param.Selected and (Param.Module.Selected) then
      ShowBtns := True
    else
      ShowBtns := False;
end;

procedure TG2KnobGL.SetParam(const Value: IG2Param);
begin
  if Value <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, Value);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2KnobGL.SetValue(const aValue: Single);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Round(aValue);
  end;
end;

procedure TG2KnobGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    evtValueChange:
      begin
        Init;
      end;
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2TextFieldGL }

constructor TG2TextFieldGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWFunc, nil);
end;

destructor TG2TextFieldGL.Destroy;
begin
  Func := nil;
  inherited;
end;

procedure TG2TextFieldGL.Init;
begin
  if assigned(Func) then
  begin
    Caption := Func.AsText;
  end;
end;

procedure TG2TextFieldGL.SetFunc(const Value: IG2Function);
begin
  if Value <> FWFunc then
  begin
    if assigned(FWFunc) then
      FWFunc.RemoveObserver(self);

    SetWeak(@FWFunc, Value);

    if assigned(FWFunc) then
    begin
      FWFunc.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2TextFieldGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Func := nil;
      end;
  end;
end;

{ TG2GraphGL }

constructor TG2GraphGL.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  SetWeak(@FWFunc, nil);
end;

destructor TG2GraphGL.Destroy;
begin
  Func := nil;
  inherited;
end;

procedure TG2GraphGL.Init;
begin

end;

procedure TG2GraphGL.SetFunc(const Value: IG2Function);
begin
  if Value <> FWFunc then
  begin
    if assigned(FWFunc) then
      FWFunc.RemoveObserver(self);

    SetWeak(@FWFunc, Value);

    if assigned(FWFunc) then
    begin
      FWFunc.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2GraphGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Func := nil;
      end;
  end;
end;

{ TG2TextEditGL }

constructor TG2TextEditGL.Create(AOwner: TComponent);
begin
  inherited;

  SetWeak(@FWParam, nil);
end;

destructor TG2TextEditGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2TextEditGL.Init;
begin
  if assigned(Param) then
  begin
    inherited Value := Param.Value;
    if Param.ParamLabel[0] <> '' then
      Caption := Param.ParamLabel[0];
  end;
end;

procedure TG2TextEditGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2TextEditGL.SetValue(const aValue: integer);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Value;
  end;
end;

procedure TG2TextEditGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2BtnTextGL }

constructor TG2BtnTextGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWParam, nil);
end;

destructor TG2BtnTextGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2BtnTextGL.Init;
begin
  if assigned(Param) then
  begin
    inherited Value := Param.Value;
  end;
end;

procedure TG2BtnTextGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2BtnTextGL.SetValue(const aValue: integer);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Value;
  end;
end;

procedure TG2BtnTextGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2BtnFlatGL }

constructor TG2BtnFlatGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWParam, nil);
end;

destructor TG2BtnFlatGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2BtnFlatGL.Init;
begin
  if assigned(Param) then
  begin
    MaxValue := Param.HighValue;
    MinValue := Param.LowValue;
    inherited Value := Param.Value;
  end;
end;

procedure TG2BtnFlatGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2BtnFlatGL.SetValue(const aValue: integer);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Value;
  end;
end;

procedure TG2BtnFlatGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2BtnLevelShiftGL }

constructor TG2BtnLevelShiftGL.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2BtnLevelShiftGL.Destroy;
begin
  inherited;
end;

{ TG2BtnRadioGL }

constructor TG2BtnRadioGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWParam, nil);
end;

destructor TG2BtnRadioGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2BtnRadioGL.Init;
begin
  if assigned(Param) then
  begin
    inherited Value := Param.Value;
    MaxValue := Param.HighValue;
    UpdateCellIndex;
  end;
end;

procedure TG2BtnRadioGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2BtnRadioGL.SetValue(const aValue: integer);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Value;
  end;
end;

procedure TG2BtnRadioGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2BtnRadioEditGL }

constructor TG2BtnRadioEditGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWParam, nil);
end;

destructor TG2BtnRadioEditGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2BtnRadioEditGL.Init;
begin
  if assigned(Param) then
  begin
    inherited Value := Param.Value;
    MaxValue := Param.HighValue;
    UpdateCellIndex;
  end;
end;

procedure TG2BtnRadioEditGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2BtnRadioEditGL.SetValue(const aValue: integer);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Value;
  end;
end;

procedure TG2BtnRadioEditGL.Update(aG2Event: TG2Event;
  const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2BtnIncDecGL }

constructor TG2BtnIncDecGL.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWParam, nil);
end;

destructor TG2BtnIncDecGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2BtnIncDecGL.Init;
begin
  if assigned(Param) then
  begin
    MaxValue := Param.HighValue;
    MinValue := Param.LowValue;
    inherited Value := Param.Value;
  end;
end;

procedure TG2BtnIncDecGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2BtnIncDecGL.SetValue(const aValue: integer);
begin
  if aValue <> Value then
  begin
    inherited;

    if assigned(Param) then
      Param.Value := Value;
  end;
end;

procedure TG2BtnIncDecGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2SelectorListGL }

constructor TG2SelectorListGL.Create(aSelector: TG2SelectorGL);
begin
  inherited Create(aSelector);
  FSelector := aSelector;
end;

{ procedure TG2SelectorListGL.CreateVertices;
  begin
  inherited;
  end; }

destructor TG2SelectorListGL.Destroy;
begin
  inherited;
end;

procedure TG2SelectorListGL.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  i, Idx: integer;
begin
  if ssLeft in Shift then
  begin
    Idx := IndexFromPoint(X, Y);
    for i := 0 to ColCount * RowCount - 1 do
      if i = Idx then
        CellIndex[i] := 1
      else
        CellIndex[i] := 0;
    FSelector.Value := Idx;
    FSelector.CloseUp;
  end;

  inherited;
end;

procedure TG2SelectorListGL.MouseMove(Shift: TShiftState; X, Y: Single);
var
  i, Idx: integer;
begin
  Idx := IndexFromPoint(X, Y);
  for i := 0 to ColCount * RowCount - 1 do
    if i = Idx then
      CellIndex[i] := 1
    else
      CellIndex[i] := 0;

  inherited;
end;

procedure TG2SelectorListGL.RenderControl(aContext: TContext3D);
begin
  inherited;
end;

procedure TG2SelectorListGL.UpdateCellIndex;
begin
end;

{ TG2SelectorGL }

constructor TG2SelectorGL.Create(AOwner: TComponent);
begin
  inherited;

  FBtn := TTexControlGL.Create(self);
  FBtn.Parent := self;

  FList := TG2SelectorListGL.Create(self);
  FList.Parent := nil;
  FList.Position := Point3D(0, Height, 0);

  FValue := 0;
  FMaxValue := 0;

  SetWeak(@FWParam, nil);
end;

destructor TG2SelectorGL.Destroy;
begin
  Param := nil;
  inherited;
end;

procedure TG2SelectorGL.DropDown;
var
  R: TRectF;
begin
  R := AbsBoundsRect;
  FList.Position := Point3D(R.Left, R.Top + Height, 0);
  FList.Invalidate;
  if assigned(FDropDown) then
    FDropDown(self);
end;

procedure TG2SelectorGL.CloseUp;
begin
  FList.Parent := nil;
  FList.Invalidate;
  if assigned(FCloseUp) then
    FCloseUp(self);
end;

procedure TG2SelectorGL.Init;
begin
  if assigned(Param) then
  begin
    FValue := Param.Value;
    MaxValue := Param.HighValue;
    ImageIndex := FValue;
  end;
end;

procedure TG2SelectorGL.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    if FList.Parent = nil then
      DropDown
    else
      CloseUp;
  end;

  inherited;
end;

procedure TG2SelectorGL.RenderControl(aContext: TContext3D);
var
  R: TRectF;
begin
  FBtn.Position := Point3D(Width, 0, 0);

  R := AbsBoundsRect;
  FList.Position := Point3D(R.Left, R.Top + Height, 0);

  inherited;

end;

procedure TG2SelectorGL.SetMaxValue(const aValue: integer);
begin
  if aValue <> FMaxValue then
  begin
    FMaxValue := aValue;

    FList.ColCount := 1;
    FList.RowCount := aValue + 1;
    FList.TexCols := aValue + 1;
    FList.TexRows := 2;

    TexCols := aValue + 1;
    TexRows := 2;
  end;

end;

procedure TG2SelectorGL.SetParam(const aValue: IG2Param);
begin
  if aValue <> FWParam then
  begin
    if assigned(FWParam) then
      FWParam.RemoveObserver(self);

    SetWeak(@FWParam, aValue);

    if assigned(FWParam) then
    begin
      FWParam.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2SelectorGL.SetTextureList(const aValue: TTextureListGL);
begin
  inherited;
  FBtn.TextureList := aValue;
  FList.TextureList := aValue;
end;

procedure TG2SelectorGL.SetValue(const aValue: integer);
begin
  if aValue <> FValue then
  begin
    FValue := aValue;
    ImageIndex := FValue;
    if assigned(Param) then
      Param.Value := FValue;
  end;
end;

procedure TG2SelectorGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Param := nil;
      end;
  end;
end;

{ TG2LedGL }

constructor TG2LedGL.Create(AOwner: TComponent);
begin
  inherited;

  TexRows := 2;
  TexCols := 1;

  SetWeak(@FWLed, nil);
end;

destructor TG2LedGL.Destroy;
begin
  Led := nil;
  inherited;
end;

procedure TG2LedGL.Init;
begin
  if assigned(Led) then
  begin
    if Led.GroupCount > 1 then
    begin
      if Led.GetValue = FCodeRef then
        ImageIndex := 1
      else
        ImageIndex := 0;
    end
    else
      ImageIndex := Led.GetValue;
  end;
end;

procedure TG2LedGL.SetLed(const Value: IG2Led);
begin
  if Value <> FWLed then
  begin
    if assigned(FWLed) then
      FWLed.RemoveObserver(self);

    SetWeak(@FWLed, Value);

    if assigned(FWLed) then
    begin
      FWLed.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2LedGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    evtValueChange:
      begin
        Init;
      end;
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Led := nil;
      end;
  end;
end;

{ TG2MiniVUGL }

constructor TG2MiniVUGL.Create(AOwner: TComponent);
begin
  inherited;

  TexRows := 16;
  TexCols := 1;

  SetWeak(@FWLed, nil);
end;

destructor TG2MiniVUGL.Destroy;
begin
  Led := nil;

  inherited;
end;

procedure TG2MiniVUGL.Init;
begin
  if assigned(Led) then
  begin
    ImageIndex := Led.GetValue;
    Invalidate;
  end;
end;

procedure TG2MiniVUGL.SetLed(const Value: IG2Led);
begin
  if Value <> FWLed then
  begin
    if assigned(FWLed) then
      FWLed.RemoveObserver(self);

    SetWeak(@FWLed, Value);

    if assigned(FWLed) then
    begin
      FWLed.RegisterObserver(self);
      Init;
    end;
  end;
end;

procedure TG2MiniVUGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    evtValueChange:
      begin
        Init;
      end;
    EvtInvalidate:
      begin
        Init;
      end;
    EvtDestroy:
      begin
        Led := nil;
      end;
  end;
end;

{ TG2FormGL }

constructor TG2FormGL.Create(AOwner: TComponent);
begin
  inherited;
  FConnectionIndex := -1;
end;

destructor TG2FormGL.Destroy;
begin
  ConMan := nil; // Set Weak pointers to nil berfore destroy!
  inherited;
end;

function TG2FormGL.GetConnection: IG2Connection;
begin
  if assigned(FWConMan) and (FConnectionIndex >= 0) and
    (FConnectionIndex < FWConMan.ConnectionList.Count) then
    Result := FWConMan.ConnectionList[FConnectionIndex]
  else
    Result := nil;
end;

procedure TG2FormGL.RegisterAsObserver;
begin
  if assigned(ConMan) then
    ConMan.RegisterObserver(self);

  if assigned(Connection) then
    Connection.RegisterObserver(self);
end;

procedure TG2FormGL.RemoveAsObserver;
begin
  if assigned(ConMan) then
    ConMan.RemoveObserver(self);

  if assigned(Connection) then
    Connection.RemoveObserver(self);
end;

procedure TG2FormGL.SetConnectionIndex(const Value: integer);
var
  FMXObject: TFMXObject;
begin
  if Value <> FConnectionIndex then
  begin
    RemoveAsObserver;
    FConnectionIndex := Value;
    RegisterAsObserver;

    if assigned(Children) then
      for FMXObject in Children do
        if FMXObject is TG2PanelGL then
        begin
          (FMXObject as TG2PanelGL).ConnectionIndex := Value;
        end;
  end;
end;

procedure TG2FormGL.SetConnectionManager(const Value: IG2ConnectionManager);
var
  FMXObject: TFMXObject;
begin
  if Value <> FWConMan then
  begin
    RemoveAsObserver;
    SetWeak(@FWConMan, Value);
    RegisterAsObserver;

    if assigned(Children) then
      for FMXObject in Children do
        if FMXObject is TG2PanelGL then
        begin
          (FMXObject as TG2PanelGL).ConMan := Value;
        end;
  end;
end;

procedure TG2FormGL.SetTextureList(const Value: TTextureListGL);
var
  FMXObject: TFMXObject;
begin
  FTextureList := Value;

  if assigned(Children) then
    for FMXObject in Children do
      if FMXObject is TG2PanelGL then
      begin
        (FMXObject as TG2PanelGL).TextureList := Value;
      end;
end;

procedure TG2FormGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  //
end;

{ TG2PanelGL }

constructor TG2PanelGL.Create(AOwner: TComponent);
begin
  inherited;
  FConnectionIndex := -1;
end;

destructor TG2PanelGL.Destroy;
begin
  ConMan := nil; // Set Weak pointers to nil berfore destroy!
  inherited;
end;

function TG2PanelGL.GetConnection: IG2Connection;
begin
  if assigned(FWConMan) and (FConnectionIndex >= 0) and
    (FConnectionIndex < FWConMan.ConnectionList.Count) then
    Result := FWConMan.ConnectionList[FConnectionIndex]
  else
    Result := nil;
end;

function TG2PanelGL.GetPerf: IG2Perf;
begin
  if assigned(Connection) then
    Result := Connection.Perf
  else
    Result := nil;
end;

function TG2PanelGL.GetSynth: IG2Synth;
begin
  if assigned(Connection) then
    Result := Connection.Synth
  else
    Result := nil;
end;

procedure TG2PanelGL.LayoutControls;
var
  FMXObject: TFMXObject;
  ControlGL: TControlGL;
  DX, dy, Margin: Single;
begin
  Margin := 3 * SCALE_X;
  if assigned(Content) and assigned(Content.Children) then
  begin
    DX := Margin;
    for FMXObject in Content.Children do
      if (FMXObject is TControlGL) then
      begin
        ControlGL := FMXObject as TControlGL;
        dy := (Height - ControlGL.Height) / 2;
        ControlGL.Position := Point3D(DX, dy, 0);
        DX := DX + ControlGL.Width + Margin;
      end;
    LayoutRect := CalcLayoutRect;
    Invalidate;
  end;
end;

procedure TG2PanelGL.RegisterAsObserver;
begin
  if assigned(ConMan) then
    ConMan.RegisterObserver(self);

  if assigned(Connection) then
    Connection.RegisterObserver(self);
end;

procedure TG2PanelGL.RemoveAsObserver;
begin
  if assigned(ConMan) then
    ConMan.RemoveObserver(self);

  if assigned(Connection) then
    Connection.RemoveObserver(self);
end;

procedure TG2PanelGL.SetConnectionIndex(const Value: integer);
begin
  if Value <> FConnectionIndex then
  begin
    RemoveAsObserver;
    FConnectionIndex := Value;
    RegisterAsObserver;
  end;
end;

procedure TG2PanelGL.SetConnectionManager(const Value: IG2ConnectionManager);
begin
  if Value <> FWConMan then
  begin
    RemoveAsObserver;
    SetWeak(@FWConMan, Value);
    RegisterAsObserver;
  end;
end;

procedure TG2PanelGL.SetTextureList(const Value: TTextureListGL);
var
  FMXObject: TFMXObject;
begin
  FTextureList := Value;
  if assigned(Content) and assigned(Content.Children) then
    for FMXObject in Content.Children do
      if (FMXObject is TTexControlGL) then
        (FMXObject as TTexControlGL).TextureList := Value;
end;

procedure TG2PanelGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  //
end;

{ TG2SlotFormGL }

constructor TG2SlotFormGL.Create(AOwner: TComponent);
begin
  inherited;
  FSlotIndex := -1;
end;

destructor TG2SlotFormGL.Destroy;
begin
  inherited;
end;

function TG2SlotFormGL.GetSlot: IG2Slot;
begin
  if assigned(Connection) and (FSlotIndex <> -1) then
    Result := Connection.Slot[FSlotIndex]
  else
    Result := nil;
end;

procedure TG2SlotFormGL.RegisterAsObserver;
begin
  inherited;
  if assigned(Slot) then
    Slot.RegisterObserver(self);
end;

procedure TG2SlotFormGL.RemoveAsObserver;
begin
  inherited;
  if assigned(Slot) then
    Slot.RemoveObserver(self);
end;

procedure TG2SlotFormGL.SetSlotIndex(const Value: integer);
var
  FMXObject: TFMXObject;
begin
  if Value <> FSlotIndex then
  begin
    RemoveAsObserver;
    FSlotIndex := Value;
    RegisterAsObserver;

    if assigned(Children) then
      for FMXObject in Children do
        if FMXObject is TG2SlotPanelGL then
        begin
          (FMXObject as TG2SlotPanelGL).SlotIndex := Value;
        end;
  end;
end;

{ TG2SlotPanelGL }

constructor TG2SlotPanelGL.Create(AOwner: TComponent);
begin
  inherited;
  FSlotIndex := -1;
end;

destructor TG2SlotPanelGL.Destroy;
begin
  inherited;
end;

function TG2SlotPanelGL.GetSlot: IG2Slot;
begin
  if assigned(Connection) and (FSlotIndex <> -1) then
    Result := Connection.Slot[FSlotIndex]
  else
    Result := nil;
end;

procedure TG2SlotPanelGL.RegisterAsObserver;
begin
  inherited;
  if assigned(Slot) then
    Slot.RegisterObserver(self);
end;

procedure TG2SlotPanelGL.RemoveAsObserver;
begin
  inherited;
  if assigned(Slot) then
    Slot.RemoveObserver(self);
end;

procedure TG2SlotPanelGL.SetSlotIndex(const Value: integer);
begin
  if Value <> FSlotIndex then
  begin
    RemoveAsObserver;
    FSlotIndex := Value;
    RegisterAsObserver;
  end;
end;

end.

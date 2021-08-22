unit BVE.NMG2GraphFMX;
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
{$INCLUDE Capabilities.inc}
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.UIConsts, System.Math, System.Generics.Defaults,
{$IF Defined(VER270) or Defined(VER280) or Defined(VER340)}
  System.Math.Vectors,
{$ENDIF}
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
{$IF Defined(VER260) or Defined(VER270) or Defined(VER280) or Defined(VER340)}
  FMX.Graphics,
{$ENDIF}
  //Spring.Collections,
  BVE.NMG2Types,
  BVE.NMG2Data,
  BVE.NMG2File,
  BVE.NMG2USB,
{$IF Defined(TCPIPNETWORK)}
  BVE.NMG2TCPIP,
{$ENDIF}
  BVE.NMG2ControlsFMX,
  BVE.NMG2Pathdata;
type
  TG2GraphSlotFMX = class;
  TG2GraphModuleFMX = class;
  TG2GraphCableFMX = class;
  TG2GraphPatchPartFMX = class;
  TCreateGraphModuleFMXEvent = procedure(Sender : TObject; aLocation : TLocationType; aModule : TG2GraphModuleFMX) of Object;
  TCreateGraphCableFMXEvent = procedure(Sender : TObject; aLocation : TLocationType; aCable : TG2GraphCableFMX) of Object;
  TCreateG2ControlEvent = procedure(Sender : TObject; aControl : TG2BaseControl) of Object;
  TG2ParamEvent = (g2pSet, g2pChange);
  TModuleListItem = class;
  TModuleSelectionList = class(TObjectList<TModuleListItem>)
  private
    [Weak] FScrollbox : TVertScrollbox;
    FItemIndex : integer;
    FFillColor : TAlphaColor;
    procedure SetItemIndex(const Value: integer);
  public
    constructor Create( AOwnsObjects : boolean);
    destructor Destroy; override;
    property ItemIndex : integer read FItemIndex write SetItemIndex;
    property ScrollBox : TVertScrollBox read FScrollbox write FScrollbox;
    property FillColor : TAlphaColor read FFillColor write FFillColor;
  end;
  TModuleListItem = class(TRectangle)
  private
    FTypeID : byte;
    FPageIndex : byte;
    FLabel : TG2Label;
    FImage : TImage;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property PageIndex : byte read FPageIndex write FPageIndex;
    property Image : TImage read FImage;
    property ImageLabel : TG2Label read FLabel;
    property TypeID : byte read FTypeID write FTypeID;
  end;
{$IF Defined(TCPIPNETWORK)}
  TG2GraphFMX = class( TG2TCPIP)
{$ELSE}
  TG2GraphFMX = class( TG2USB)
{$ENDIF}
  private
    [Weak] FSelectedControl    : TG2BaseControl;
    FKnobControl : TKnobControl;
    FCableStyle : TCableStyle;
    FModulePanelDefs: TModuleDefList;
    procedure SetCableStyle(const Value: TCableStyle);
    procedure SetKnobControl(const Value: TKnobControl);
  protected
    procedure SelectControl(Sender: TObject);
    function GetSelectedSlot : TG2GraphSlotFMX;
    procedure SetSelectedControl(const Value: TG2BaseControl);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePerformance : TG2FilePerformance; override;
    property SelectedSlot : TG2GraphSlotFMX read GetSelectedSlot;
    property SelectedControl : TG2BaseControl read FSelectedControl write SetSelectedControl;
    property KnobControl : TKnobControl read FKnobControl write SetKnobControl;
    property CableStyle : TCableStyle read FCableStyle write SetCableStyle;
  end;
{$IF Defined(TCPIPNETWORK)}
  TG2GraphPerformanceFMX = class( TG2TCPIPPerformance)
{$ELSE}
  TG2GraphPerformanceFMX = class( TG2USBPerformance)
{$ENDIF}
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateSlot : TG2FileSlot; override;
  end;
{$IF Defined(TCPIPNETWORK)}
  TG2GraphSlotFMX = class( TG2TCPIPSlot)
{$ELSE}
  TG2GraphSlotFMX = class( TG2USBSlot)
{$ENDIF}
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePatch : TG2FilePatch; override;
  end;
  TG2GraphPatchPartFMX = class(TG2FilePatchPart)
  private
    FModulePanelDefs : TModuleDefList;
  public
    constructor Create( aPatch : TG2FilePatch); override;
    destructor Destroy; override;
    property ModulePanelDefs : TModuleDefList read FModulePanelDefs write FModulePanelDefs;
  end;
  //TG2GraphParameterFMX = class;
  TG2GraphPatchFMX = class( TG2USBPatch)
  private
    FVisible            : boolean;
    FZoomSetting : integer;
    FSplitterVisible : boolean;
    FSelectedMorphIndex : integer;
    FSelectMultiple: boolean;
    FOnCreateModuleFMX : TCreateGraphModuleFMXEvent;
    FOnCreateCableFMX : TCreateGraphCableFMXEvent;
  protected
    procedure SetSelectedLocation( aLocation : TLocationType); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override;
    function CreatePatchPart( aLocation : TLocationType): TG2FilePatchPart; override;
    function CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; override;
    function CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; override;
    //function CreateParameter( aModuleIndex : byte): TG2FileParameter; override;
    //function CreateLed( const aLedType : TLedType; const aLocation : TLocationType; const aModuleIndex, aGroupID : byte; const aGroupCount : integer): IG2DataLed; override;
    procedure SetCablesVisible( aColor : integer; aVisible : boolean);
    function GetCableVisible( aColor : integer): boolean;
    //function GetMorphParameter( aIndex : integer): TG2FileParameter;
    //function GetPatchParameter( aIndex : integer): TG2FileParameter;
    property ZoomSetting : integer read FZoomSetting write FZoomSetting;
    property SplitterVisible : boolean read FSplitterVisible write FSplitterVisible;
    property SelectMultiple : boolean read FSelectMultiple write FSelectMultiple default False;
    property OnCreateModuleFMX : TCreateGraphModuleFMXEvent read FOnCreateModuleFMX write FOnCreateModuleFMX;
    property OnCreateCableFMX : TCreateGraphCableFMXEvent read FOnCreateCableFMX write FOnCreateCableFMX;
  end;
  TG2GraphModuleFMX = class( TG2FileModule)
  private
    [Weak] FG2Module : TG2Module;
    FOnCreateG2Control : TCreateG2ControlEvent;
  protected
    function GetOutlineRect: TRectF;
    //procedure SetSelected( aValue: boolean); override;
    procedure SetCol( aValue : TBits7); override;
    procedure SetRow( aValue : TBits7); override;
    procedure SetModuleColor( aValue : TBits8); override;
    procedure SetModuleName( aValue : string); override;
    procedure SetModuleIndex( const aValue : TBits8); override;
    procedure SetLocation( const aValue : TLocationType); override;
  public
    constructor Create( aPatchPart : TG2FilePatchPart); override;
    constructor CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModuleFMX);
    function CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; override;
    destructor Destroy; override;
    procedure SelectModule(const aValue, aExclusive : boolean); override;
    procedure InitPanel;
    procedure FreePanel;
    //function CreateParameter: TG2FileParameter; override;
    function CreateConnector: TG2FileConnector; override;
    procedure InvalidateControl; override;
    procedure PaintOnCanvas( aCanvas : TCanvas; aScale : single);
    property G2Module : TG2Module read FG2Module write FG2Module;
    property OutlineRect : TRectF read GetOutlineRect;
    property OnCreateG2Control : TCreateG2ControlEvent read FOnCreateG2Control write FOnCreateG2Control;
  end;
  {TG2GraphParameterFMX = class(TG2FileParameter)
  protected
    procedure GetLabel(Sender : TObject; const aIndex : integer; var aLabel : string);
    procedure SetLabel(Sender : TObject; const aIndex : integer; const aLabel : string);
    procedure SetInfoFunc(Sender : TObject; const aInfoFunc : integer);
    procedure GetInfoFunc(Sender : TObject; var aInfoFunc : string);
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor Destroy; override;
    procedure GetTextFunc(Sender : TObject; var aTextFunc : string);
  end;}
  TG2GraphConnectorFMX = class(TG2FileConnector)
  private
    [Weak] FG2Connector : TG2Connector;
    procedure SetG2Connector( const aValue : TG2Connector);
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModule : TG2FileModule); override;
    destructor Destroy; override;
    procedure CreateCable( Sender : TObject; aToConnector : TG2Connector);
    procedure InvalidateControl; override;
    property G2Connector : TG2Connector read FG2Connector write SetG2Connector;
  end;
  {TG2GraphLedFMX = class(TG2FileLed)
  private
    FGroupCount : integer;
    FOldValue : byte;
  public
    constructor Create( const aLedType : TLedType;
        const aLocationType : TLocationType; const aModuleIndex,
        aGroupID : byte; const aGroupCount : integer);
    destructor Destroy; override;
  end;}
  TG2GraphCableFMX = class( TG2FileCable)
  protected
    [Weak] FG2Cable : TG2Cable;
    FSelected       : boolean;
  private
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
  protected
    procedure SetCableColor( Value : byte); override;
  public
    constructor Create( aPatchPart : TG2FilePatchPart); override;
    destructor Destroy; override;
    procedure InitCable;
    procedure ConnectorMoved; override;
    property Visible : boolean read GetVisible write SetVisible;
    property G2Control : TG2Cable read FG2Cable write FG2Cable;
  end;
procedure CreateModuleImages( aBitmapList : TObjectDictionary<integer, TBitmap>);
procedure CreateModuleImage( const aModuleType : byte; aBitmap : TBitmap);
procedure CreateModuleControls( aModule : TG2GraphModuleFMX; aControl : TControl; aConnectParams, aConnectConnectors : boolean);
procedure ParseDependencies( aModule : TG2FileModule; aControl : TG2MultiReadControl);
procedure ComponentSetStateStylesRecursive( aControl : TControl; aStateStyleList : TG2StateStyleList);
function ConvertToAlpha( aColor : integer): TAlphaColor;
procedure AdvanceFocus( aControl : IControl; const MoveForward: Boolean);
function GetFocusedControl( aFMXObject : TFMXObject): IControl;
implementation
//------------------------------------------------------------------------------
//
//                                 Utils
//
//------------------------------------------------------------------------------
function ConvertToAlpha( aColor : integer): TAlphaColor;
begin
  Result := $ff000000
          + (Cardinal(aColor) and $000000ff) shl 16
          + (Cardinal(aColor) and $0000ff00)
          + (Cardinal(aColor) and $00ff0000) shr 16;
end;
function CreateUnityMatrix : TMatrix;
begin
  Result.m11 := 1;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := 1;
  Result.m23 := 0;
  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;
{$IF Defined(VER260) or Defined(VER270) or Defined(VER280)}
function CreateScaleMatrix( sx, sy : single): TMatrix;
begin
  Result.m11 := sx;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := sy;
  Result.m23 := 0;
  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;
function CreateTranslateMatrix( tx, ty : single): TMatrix;
begin
  Result.m11 := 1;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := 1;
  Result.m23 := 0;
  Result.m31 := tx;
  Result.m32 := ty;
  Result.m33 := 1;
end;
function MatrixMultiply(const M1, M2: TMatrix): TMatrix;
begin
  Result.m11 := M1.m11 * M2.m11 + M1.m12 * M2.m21 + M1.m13 * M2.m31;
  Result.m12 := M1.m11 * M2.m12 + M1.m12 * M2.m22 + M1.m13 * M2.m32;
  Result.m13 := M1.m11 * M2.m13 + M1.m12 * M2.m23 + M1.m13 * M2.m33;
  Result.m21 := M1.m21 * M2.m11 + M1.m22 * M2.m21 + M1.m23 * M2.m31;
  Result.m22 := M1.m21 * M2.m12 + M1.m22 * M2.m22 + M1.m23 * M2.m32;
  Result.m23 := M1.m21 * M2.m13 + M1.m22 * M2.m23 + M1.m23 * M2.m33;
  Result.m31 := M1.m31 * M2.m11 + M1.m32 * M2.m21 + M1.m33 * M2.m31;
  Result.m32 := M1.m31 * M2.m12 + M1.m32 * M2.m22 + M1.m33 * M2.m32;
  Result.m33 := M1.m31 * M2.m13 + M1.m32 * M2.m23 + M1.m33 * M2.m33;
end;
{$ENDIF}
procedure ComponentSetStateStylesRecursive( aControl : TControl; aStateStyleList : TG2StateStyleList);
var i : integer;
begin
  for i := 0 to aControl.ChildrenCount - 1 do begin
    if aControl.Children[i] is TG2StyleControl then begin
      (aControl.Children[i] as TG2StyleControl).StateStyleList := aStateStyleList;
    end else
      if aControl.Children[i] is TControl then
        ComponentSetStateStylesRecursive( aControl.Children[i] as TControl, aStateStyleList);
  end;
end;
procedure GetTabOrderList( aFMXObject : TFMXObject; const List: TInterfaceList; AChildren: Boolean);
var
  I: Integer;
  Control: IControl;
begin
  //if Assigned(FTabList) then
    for I := 0 to aFMXObject.ChildrenCount - 1 do
    begin
{$IF Defined(VER280) or Defined(VER340)}
      if True then begin
{$ELSE}
      if aFMXObject.Children[I].IsIControl then begin
{$ENDIF}
        Control := IControl(aFMXObject.Children[I] as TControl);
        List.Add(Control);
        if AChildren and (Control.GetObject is TFmxObject) then
          GetTabOrderList(TFmxObject(Control), List, AChildren);
      end;
    end;
end;
procedure AdvanceFocus( aControl : IControl; const MoveForward: Boolean);
var
  NewFocus: IControl;
  List: TInterfaceList;
  I, CurIdx: Integer;
begin
  if not assigned(aControl) then
    exit;
  NewFocus := nil;
  List := TInterfaceList.Create;
  try
    GetTabOrderList(aControl.Parent, List, True);
    CurIdx := List.IndexOf(aControl);
    for I := 0 to List.Count-1 do
    begin
      if MoveForward then
        CurIdx := (CurIdx + 1) mod List.Count
      else
      begin
        CurIdx := CurIdx - 1;
        if CurIdx < 0 then CurIdx := List.Count - 1;
      end;
      if IControl(List[CurIdx]).CheckForAllowFocus then
      begin
        NewFocus := IControl(List[CurIdx]);
        break;
      end;
    end;
  finally
    FreeAndNil(List);
  end;
  if Assigned(NewFocus) then
    NewFocus.SetFocus;
end;
function GetFocusedControl( aFMXObject : TFMXObject): IControl;
begin
  if aFMXObject is TForm then
    Result := (aFMXObject as TForm).Focused
  else
    if assigned(aFMXObject.Parent) then
      Result := GetFocusedControl( aFMXObject.Parent)
    else
      Result := nil;
end;

procedure CreateModuleImage( const aModuleType : byte; aBitmap : TBitmap);
var Module : TG2GraphModuleFMX;
    Scale : single;
    i : integer;
begin
  Module := TG2GraphModuleFMX.Create(nil);
  try

    Module.ModuleIndex := 0;
    Module.Location := ltVA;
    Module.TypeID := aModuleType;
    Module.InitModule( ltVA, aModuleType);
    Module.G2Module := TG2Module.Create(Module);
    Module.G2Module.Parent := nil;
    Module.InitPanel;
    CreateModuleControls(Module, Module.FG2Module, True, True);
    Scale := aBitmap.Width/(UNITS_COL+UNIT_MARGIN*2);
    aBitmap.Clear(TAlphaColorRec.Alpha);
    Module.PaintOnCanvas( aBitmap.Canvas, Scale);

    //ModuleImage.Bitmap.SaveToFile(Module.ModuleFileName + '.png');
  finally
    Module.DisposeOf;
  end;
end;
procedure CreateModuleImages( aBitmapList : TObjectDictionary<integer, TBitmap>);
var i : integer;
    Bitmap : TBitmap;
begin
  i := 0;
  while i <= High(ModuleDefs) do begin
    //i := 118;

    Bitmap := TBitmap.Create(UNITS_COL+UNIT_MARGIN*2, (UNITS_ROW+UNIT_MARGIN*2)*ModuleDefs[i].Height);

    CreateModuleImage( ModuleDefs[i].ModuleID, Bitmap);

    aBitmapList.Add( ModuleDefs[i].ModuleID, Bitmap);
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
//
//                            TModuleSelectionList
//
//------------------------------------------------------------------------------
constructor TModuleSelectionList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
  FItemIndex := -1;
  FFillColor := claWhite;
end;
destructor TModuleSelectionList.Destroy;
begin
  inherited;
end;
procedure TModuleSelectionList.SetItemIndex(const Value: integer);
var ItemTop, ItemBottom : single;
    i : integer;
    NewViewportPosition : TPointF;
begin
  if FItemIndex <> Value then begin
    if FItemIndex <> -1 then begin
      //Items[FItemIndex].Fill.Color := $FFE0E0E0;
      Items[FItemIndex].Fill.Color := FFillColor;
      Items[FItemIndex].FLabel.StateStyleList.StateStyle[csDefault].FontColor := claBlack;
    end;
    FItemIndex := Value;
    if FItemIndex <> -1 then begin
      Items[FItemIndex].Fill.Color := claCyan;
      ItemTop := 0;
      for i := 0 to FItemIndex - 1 do begin
        ItemTop := ItemTop + Items[i].Height;
      end;
      ItemBottom := ItemTop + Items[FItemIndex].Height;
      NewViewportPosition := FScrollbox.ViewportPosition;
      if ItemBottom > FScrollbox.ViewportPosition.Y + FScrollbox.Height then
        NewViewportPosition.Y := ItemBottom - FScrollbox.Height;
      if ItemTop < FScrollbox.ViewportPosition.Y then
        NewViewportPosition.Y := ItemTop;
      FScrollbox.ViewportPosition := NewViewportPosition;
      //if ItemBottom >  (FScrollbox.ViewportPosition.Y + FScrollbox.Height) then
      //  FScrollbox.ScrollTo(0, (FScrollbox.ViewportPosition.Y + FScrollbox.Height) - ItemBottom);
      //if ItemTop < FScrollbox.ViewportPosition.Y then
      //  FScrollbox.ScrollTo(0, FScrollbox.ViewportPosition.Y - ItemTop);
    end;
  end;
end;
//------------------------------------------------------------------------------
//
//                              TModuleListItem
//
//------------------------------------------------------------------------------
constructor TModuleListItem.Create(AOwner: TComponent);
begin
  inherited;
  Stroke.Kind := TBrushKind.bkNone;
  FLabel := TG2Label.Create(self);
  FLabel.Parent := self;
  FLabel.Align := TAlignLayout.alTop;
  FLabel.Height := 20;
  FLabel.TextAlign := TTextAlign.taCenter;
  FLabel.HitTest := False;
  FLabel.StateStyleList.StateStyle[csDefault].Font.Style := [TFontStyle.fsBold];
  FLabel.StateStyleList.StateStyle[csDefault].Font.Size := 12;
  FLabel.StateStyleList.StateStyle[csSelected].Font.Style := [TFontStyle.fsBold];
  FLabel.StateStyleList.StateStyle[csSelected].Font.Size := 12;
  FImage := TImage.Create(self);
  FImage.Parent := self;
  FImage.Align := TAlignLayout.alClient;
  FImage.HitTest := False;
end;
destructor TModuleListItem.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------
//
//                              TG2GraphFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphFMX.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedControl := nil;
end;
destructor TG2GraphFMX.Destroy;
begin
  inherited;
end;
function TG2GraphFMX.GetSelectedSlot: TG2GraphSlotFMX;
begin
  Result := GetSlot(Performance.SelectedSlot) as TG2GraphSlotFMX;
end;
procedure TG2GraphFMX.SelectControl(Sender: TObject);
begin
  SelectedControl := Sender as TG2BaseControl;
end;
procedure TG2GraphFMX.SetCableStyle(const Value: TCableStyle);
var p, l, c : integer;
    Patch : TG2FilePatch;
    Cable : TG2GraphCableFMX;
    G2Cable : TG2Cable;
    FMXObject : TFMXObject;
begin
  if FCableStyle <> Value then begin
    FCableStyle := Value;
    for p := 0 to 3 do begin
      Patch := GetSlot(p).Patch;
      for l := 0 to 1 do begin
        for c := 0 to Patch.PatchPart[l].CableList.Count - 1 do begin
          Cable := Patch.PatchPart[l].CableList[c] as TG2GraphCableFMX;
          G2Cable := Cable.FG2Cable;
          G2Cable.CableStyle := Value;
        end;
      end;
    end;
  end;
end;
procedure TG2GraphFMX.SetKnobControl(const Value: TKnobControl);
var p, l, m, c : integer;
    Patch : TG2FilePatch;
    Module : TG2GraphModuleFMX;
    G2Module : TG2Module;
    FMXObject : TFMXObject;
begin
  if FKnobControl <> Value then begin
    FKnobControl := Value;
    for p := 0 to 3 do begin
      Patch := GetSlot(p).Patch;
      for l := 0 to 1 do begin
        for m := 0 to Patch.PatchPart[l].ModuleList.Count - 1 do begin
          Module := Patch.PatchPart[l].ModuleList[m] as TG2GraphModuleFMX;
          G2Module := Module.G2Module;
          for c := 0 to G2Module.ChildrenCount - 1 do begin
            FMXObject := G2Module.Children[c];
            if FMXObject is TG2Knob then
              (FMXObject as TG2Knob).KnobControl := Value;
          end;
        end;
      end;
    end;
  end;
end;
function TG2GraphFMX.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2GraphPerformanceFMX.Create(self);
end;
procedure TG2GraphFMX.SetSelectedControl(const Value: TG2BaseControl);
begin
  if Value <> FSelectedControl then begin
    FSelectedControl := Value;
  end;
end;
//------------------------------------------------------------------------------
//
//                          TG2GraphPerformanceFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphPerformanceFMX.Create(AOwner: TComponent);
begin
  inherited;
end;
function TG2GraphPerformanceFMX.CreateSlot: TG2FileSlot;
begin
  Result := TG2GraphSlotFMX.Create( self);
end;
destructor TG2GraphPerformanceFMX.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------
//
//                             TG2GraphSlotFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphSlotFMX.Create(AOwner: TComponent);
begin
  inherited;
end;
function TG2GraphSlotFMX.CreatePatch: TG2FilePatch;
begin
  Result := TG2GraphPatchFMX.Create( self);
end;
destructor TG2GraphSlotFMX.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
//
//                              TG2GraphPatchFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphPatchFMX.Create(AOwner: TComponent);
begin
  FSelectedMorphIndex := 0;
  inherited;
end;
destructor TG2GraphPatchFMX.Destroy;
begin
  inherited;
end;
procedure TG2GraphPatchFMX.Init;
begin
  // Todo, check per patch
  if assigned(G2) then
    (G2 as TG2GraphFMX).SelectedControl := nil;
  inherited;
end;
// CHECK: volgende 2 moeten naar patch part!
function TG2GraphPatchFMX.CreateModule(aLocation: TLocationType; aModuleIndex,
  aModuleType: byte): TG2FileModule;
var i : integer;
    Module : TG2GraphModuleFMX;
    ModuleDefStream : TModuleDefStream;
begin
  // Create a module in a patch file
  Result := nil;
  Module := TG2GraphModuleFMX.Create( PatchPart[ ord(aLocation)]);
  Module.ModuleIndex := aModuleIndex;
  Module.Location := aLocation;
  Module.TypeID := aModuleType;
  if aLocation <> ltPatch then begin
    Module.InitModule( aLocation, aModuleType);
    CreateLeds( aLocation, aModuleIndex, aModuleType);
  end;
  if assigned(FOnCreateModuleFMX) then
    FOnCreateModuleFMX( self, aLocation, Module);
  Result := Module;
end;
function TG2GraphPatchFMX.CreateCable(aLocation: TLocationType; aColor,
  aFromModule, aFromConnector, aLinkType, aToModule,
  aToConnector: byte): TG2FileCable;
var i : integer;
    FromConnKind : TConnectorKind;
    ModuleFrom, ModuleTo : TG2GraphModuleFMX;
    Cable : TG2GraphCableFMX;
    ConnectorFrom, ConnectorTo : TG2Connector;
begin
  // Create a cable connection in a patch file
  Result := nil;
  ModuleFrom := GetModule( ord(aLocation), aFromModule) as TG2GraphModuleFMX;
  if not assigned(ModuleFrom) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aFromModule) + ' not found.');
  ModuleTo := GetModule( ord(aLocation), aToModule) as TG2GraphModuleFMX;
  if not assigned(ModuleTo) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aToModule) + ' not found.');
  Cable               := TG2GraphCableFMX.Create( PatchPart[ord(aLocation)]);
  Cable.CableColor    := aColor;
  Cable.ModuleFrom    := aFromModule;
  Cable.ConnectorFrom := aFromConnector;
  Cable.LinkType      := aLinkType;
  Cable.ModuleTo      := aToModule;
  Cable.ConnectorTo   := aToConnector;
  // If Linktype is 1 then the first connector is an output, else it's an input
  if aLinkType = 1 then
    FromConnKind := ckOutput
  else
    FromConnKind := ckInput;

  // Link connectors to cable
  if FromConnKind = ckInput then
    Cable.FromConnector := ModuleFrom.InConnector[ aFromConnector]
  else
    Cable.FromConnector := ModuleFrom.OutConnector[ aFromConnector];
  Cable.ToConnector := ModuleTo.InConnector[ aToConnector];
  // Add cable to connectors
  Cable.FromConnector.AddCable(Cable);
  Cable.ToConnector.AddCable(Cable);
  if assigned(FOnCreateCableFMX) then
    FOnCreateCableFMX(self, aLocation, Cable);
  if aLocation = ltFX then begin
    ConnectorFrom := (Cable.FromConnector as TG2GraphConnectorFMX).G2Connector;
    ConnectorTo := (Cable.ToConnector as TG2GraphConnectorFMX).G2Connector;
    if assigned(ConnectorFrom) and assigned(ConnectorTo) then
      Cable.InitCable;
    Cable.Visible := GetCableVisible( Cable.CableColor);
  end else begin
    ConnectorFrom := (Cable.FromConnector as TG2GraphConnectorFMX).G2Connector;
    ConnectorTo := (Cable.ToConnector as TG2GraphConnectorFMX).G2Connector;
    if assigned(ConnectorFrom) and assigned(ConnectorTo) then
      Cable.InitCable;
    Cable.Visible := GetCableVisible( Cable.CableColor);
  end;
  Result := Cable;
end;
{function TG2GraphPatchFMX.CreateParameter(aModuleIndex: byte): TG2FileParameter;
begin
  Result := TG2GraphParameterFMX.Create( self, ltPatch, aModuleIndex, nil);
end;}
function TG2GraphPatchFMX.CreatePatchPart(
  aLocation: TLocationType): TG2FilePatchPart;
begin
  Result := TG2GraphPatchPartFMX.Create( Self);
  Result.Location := aLocation;
end;
{function TG2GraphPatchFMX.CreateLed(const aLedType : TLedType;
  const aLocation : TLocationType; const aModuleIndex, aGroupID: byte;
  const aGroupCount : integer): IG2DataLed;
begin
  Result := TG2GraphLedFMX.Create( aLedType, aLocation, aModuleIndex, aGroupID, aGroupCount);
end;}
function TG2GraphPatchFMX.GetCableVisible(aColor: integer): boolean;
begin
  case aColor of
  0 : Result := PatchDescription.RedVisible = 1;
  1 : Result := PatchDescription.BlueVisible = 1;
  2 : Result := PatchDescription.YellowVisible = 1;
  3 : Result := PatchDescription.OrangeVisible = 1;
  4 : Result := PatchDescription.GreenVisible = 1;
  5 : Result := PatchDescription.PurpleVisible = 1;
  6 : Result := PatchDescription.WhiteVisible = 1;
  else
    Result := False;
  end;
end;
{function TG2GraphPatchFMX.GetMorphParameter(
  aIndex: integer): TG2FileParameter;
begin
  Result := nil;
  case aIndex of
  0 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[0];
  1 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[1];
  2 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[2];
  3 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[3];
  4 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[4];
  5 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[5];
  6 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[6];
  7 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[7];
  8 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[8];
  9 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[9];
  10 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[10];
  11 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[11];
  12 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[12];
  13 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[13];
  14 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[14];
  15 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[15];
  else
    raise Exception.Create('Knob index out of range.');
  end;
end;
function TG2GraphPatchFMX.GetPatchParameter(
  aIndex: integer): TG2FileParameter;
begin
  Result := nil;
  case aIndex of
  0 : Result := Modules[ord(ltPatch), PATCH_MASTERCLOCK].Parameter[0];
  1 : Result := Modules[ord(ltPatch), PATCH_VOICES].Parameter[0];
  2 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_SPEED];
  3 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_DIRECTION];
  4 : Result := Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_DEPTH];
  5 : Result := Modules[ord(ltPatch), PATCH_GLIDE].Parameter[GLIDE_SPEED];
  6 : Result := Modules[ord(ltPatch), PATCH_BEND].Parameter[BEND_RANGE];
  7 : Result := Modules[ord(ltPatch), PATCH_VOLUME].Parameter[VOLUME_LEVEL];
  8 : Result := Modules[ord(ltPatch), PATCH_MASTERCLOCK].Parameter[1];
  9 : Result := Modules[ord(ltPatch), PATCH_VOICES].Parameter[1];
  10 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_ON_OFF];
  11 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_OCTAVES];
  12 : Result := Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_MOD];
  13 : Result := Modules[ord(ltPatch), PATCH_GLIDE].Parameter[GLIDE_TYPE];
  14 : Result := Modules[ord(ltPatch), PATCH_BEND].Parameter[BEND_ON_OFF];
  15 : Result := Modules[ord(ltPatch), PATCH_VOLUME].Parameter[VOLUME_MUTE];
  else
    raise Exception.Create('Knob index out of range.');
  end;
end;}
procedure TG2GraphPatchFMX.SetCablesVisible(aColor: integer; aVisible: boolean);
var i, j : integer;
begin
  for i := 0 to 1 do begin
    for j := 0 to PatchPart[i].CableList.Count - 1 do begin
      if PatchPart[i].CableList[j].CableColor = aColor then
        (PatchPart[i].CableList[j] as TG2GraphCableFMX).Visible := aVisible;
    end;
  end;
end;
procedure TG2GraphPatchFMX.SetSelectedLocation(aLocation: TLocationType);
begin
  //if aLocation in [ltFX, ltVA] then
    inherited;
end;

//------------------------------------------------------------------------------
//
//                          TG2GraphPatchPartFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphPatchPartFMX.Create(aPatch: TG2FilePatch);
begin
  inherited;
end;
destructor TG2GraphPatchPartFMX.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------
//
//                           TG2GraphConnectorFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphConnectorFMX.Create(aPatch: TG2FilePatch;
  aLocation: TLocationType; aModule: TG2FileModule);
begin
  inherited Create( aPatch, aLocation, aModule);
end;
procedure TG2GraphConnectorFMX.CreateCable(Sender: TObject;
  aToConnector: TG2Connector);
var ConnectorTo : TG2FileConnector;
    ModuleTo : TG2FileModule;
begin
  ModuleTo := Module.PatchPart.Patch.Modules[ ord(Location), aToConnector.ModuleIndex];
  if assigned(ModuleTo) then begin
    if aToConnector.ConnectorKind = ckInput then
      ConnectorTo := ModuleTo.InConnector[ aToConnector.CodeRef]
    else
      ConnectorTo := ModuleTo.OutConnector[ aToConnector.CodeRef];
    if assigned(ConnectorTo) then
      (Module.PatchPart.Patch as TG2GraphPatchFMX).MessAddConnection( Location, self, ConnectorTo);
  end;
end;
destructor TG2GraphConnectorFMX.Destroy;
begin
  inherited;
end;
procedure TG2GraphConnectorFMX.InvalidateControl;
begin
  inherited;
  if assigned(FG2Connector) then begin
    FG2Connector.ConnectorColor := ConvertToAlpha( CableColors[ConnectorColor]);
    FG2Connector.Connected := CableCount > 0;
  end;
end;
procedure TG2GraphConnectorFMX.SetG2Connector(const aValue: TG2Connector);
begin
  if FG2Connector <> aValue then begin
    FG2Connector := aValue;
    if assigned(FG2Connector) then begin
      ConnectorIndex := aValue.CodeRef;
      ConnectorType := aValue.ConnectorType;
      BandWidth := aValue.BandWidth;
      CalcDefColor;
      FG2Connector.ModuleIndex := Module.ModuleIndex;
      //FG2Connector.Location := Module.Location;
      FG2Connector.ConnectorColor := ConvertToAlpha(CableColors[ ConnectorColor]);
      FG2Connector.ConnectorKind := ConnectorKind;
      FG2Connector.OnCreateCable := CreateCable;
    end;
  end;
end;
//------------------------------------------------------------------------------
//
//                          TG2GraphParameterFMX
//
//------------------------------------------------------------------------------
{constructor TG2GraphParameterFMX.Create(aPatch: TG2FilePatch;
  aLocation: TLocationType; aModuleIndex: integer; aModule: TG2FileModule);
begin
  inherited Create( aPatch, aLocation, aModuleIndex, aModule);
end;
destructor TG2GraphParameterFMX.Destroy;
begin
  inherited;
end;
procedure TG2GraphParameterFMX.GetInfoFunc(Sender: TObject;
  var aInfoFunc: string);
begin
  aInfoFunc := InfoFunction( InfoFunctionIndex);
end;
{procedure TG2GraphParameterFMX.GetLabel(Sender: TObject; const aIndex: integer;
  var aLabel: string);
begin
  aLabel := GetParamLabel( aIndex);
end;
procedure TG2GraphParameterFMX.GetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  aTextFunc := TextFunction;
end;
procedure TG2GraphParameterFMX.SetInfoFunc(Sender: TObject; const aInfoFunc: integer);
begin
  InfoFunctionIndex := aInfoFunc;
end;
procedure TG2GraphParameterFMX.SetLabel(Sender: TObject; const aIndex: integer;
  const aLabel: string);
begin
  SetParamLabel( aIndex, aLabel);
end;}
//------------------------------------------------------------------------------
//
//                              TG2GraphLedFMX
//
//------------------------------------------------------------------------------
{constructor TG2GraphLedFMX.Create( const aLedType : TLedType;
  const aLocationType : TLocationType; const aModuleIndex, aGroupID: byte;
  const aGroupCount : integer);
begin
  inherited;
  FOldValue := 0;
end;
destructor TG2GraphLedFMX.Destroy;
begin
  inherited;
end;}
//------------------------------------------------------------------------------
//
//                             TG2GraphCableFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphCableFMX.Create( aPatchPart : TG2FilePatchPart);
var i : integer;
begin
  inherited;
  FromConnector := nil;
  ToConnector   := nil;
  FG2Cable := nil;
end;
destructor TG2GraphCableFMX.Destroy;
begin  // Remove the cable from the connectors
  if assigned(FromConnector) then
    FromConnector.DelCable( self);
  if assigned(ToConnector) then
    ToConnector.DelCable( self);
  inherited;
end;
function TG2GraphCableFMX.GetVisible: boolean;
begin
  if assigned(FG2Cable) then
    Result := FG2Cable.Visible
  else
    Result := False;
end;
procedure TG2GraphCableFMX.SetVisible(const Value: boolean);
begin
  if assigned(FG2Cable) then
    FG2Cable.Visible := Value;
end;
procedure TG2GraphCableFMX.ConnectorMoved;
var ModuleFrom, ModuleTo : TG2Module;
    ConnectorFrom, ConnectorTo : TG2Connector;
    P : TPointF;
begin
  if not(assigned(FG2Cable)
         and assigned(FromConnector)
         and assigned(ToConnector)
         and assigned(FromConnector.Module)
         and assigned(ToConnector.Module)) then
    exit;
  ModuleFrom := (FromConnector.Module as TG2GraphModuleFMX).G2Module;
  ModuleTo := (ToConnector.Module as TG2GraphModuleFMX).G2Module;
  ConnectorFrom := (FromConnector as TG2GraphConnectorFMX).G2Connector;
  ConnectorTo := (ToConnector as TG2GraphConnectorFMX).G2Connector;
  P.X := ConnectorFrom.Position.X + ConnectorFrom.Width/2;
  P.Y := ConnectorFrom.Position.Y + ConnectorFrom.Height/2;
  FG2Cable.Point1X := P.X + ModuleFrom.Position.X;
  FG2Cable.Point1Y := P.Y + ModuleFrom.Position.Y;
  P.X := ConnectorTo.Position.X + ConnectorTo.Width/2;
  P.Y := ConnectorTo.Position.Y + ConnectorTo.Height/2;
  FG2Cable.Point2X := P.X + ModuleTo.Position.X;
  FG2Cable.Point2Y := P.Y + ModuleTo.Position.Y;
  FG2Cable.Color := ConvertToAlpha( CableColors[ CableColor]);
  //FG2Cable.StartTimer;
  FG2Cable.InitCable;
end;
procedure TG2GraphCableFMX.InitCable;
var ModuleFrom, ModuleTo : TG2Module;
    ConnectorFrom, ConnectorTo : TG2Connector;
    P : TPointF;
begin
  if not(assigned(FG2Cable)
         and assigned(FromConnector)
         and assigned(ToConnector)
         and assigned(FromConnector.Module)
         and assigned(ToConnector.Module)) then
    exit;
  ModuleFrom := (FromConnector.Module as TG2GraphModuleFMX).G2Module;
  ModuleTo := (ToConnector.Module as TG2GraphModuleFMX).G2Module;
  ConnectorFrom := (FromConnector as TG2GraphConnectorFMX).G2Connector;
  ConnectorTo := (ToConnector as TG2GraphConnectorFMX).G2Connector;
  P.X := ConnectorFrom.UnscaledLeft + ConnectorFrom.UnscaledWidth/2;
  P.Y := ConnectorFrom.UnscaledTop + ConnectorFrom.UnscaledHeight/2;
  FG2Cable.Point1X := P.X + ModuleFrom.UnscaledLeft;
  FG2Cable.Point1Y := P.Y + ModuleFrom.UnscaledTop;
  P.X := ConnectorTo.UnscaledLeft + ConnectorTo.UnscaledWidth/2;
  P.Y := ConnectorTo.UnscaledTop + ConnectorTo.UnscaledHeight/2;
  FG2Cable.Point2X := P.X + ModuleTo.UnscaledLeft;
  FG2Cable.Point2Y := P.Y + ModuleTo.UnscaledTop;
  FG2Cable.Color := ConvertToAlpha( CableColors[ CableColor]);
  FG2Cable.InitCable;
end;
procedure TG2GraphCableFMX.SetCableColor(Value: byte);
begin
  inherited;
  if Assigned(FG2Cable) then
    FG2Cable.Color := ConvertToAlpha( CableColors[ Value]);
end;
//------------------------------------------------------------------------------
//
//                             TG2GraphModuleFMX
//
//------------------------------------------------------------------------------
constructor TG2GraphModuleFMX.Create(aPatchPart: TG2FilePatchPart);
begin
  inherited Create( aPatchPart);
  FG2Module := nil;
end;
constructor TG2GraphModuleFMX.CopyCreate(aPatchPart: TG2FilePatchPart;
  aModule: TG2GraphModuleFMX);
begin
  inherited Create( aPatchPart);
  FG2Module := nil;
  Copy( aModule);
end;
procedure TG2GraphModuleFMX.InitPanel;
begin
  if not assigned(FG2Module) then
    exit;
  FG2Module.ModuleIndex := ModuleIndex;
  FG2Module.Location := Location;
  if assigned(PatchPart) then
    FG2Module.ModuleLabel := PatchPart.GetModuleLabel( ModuleIndex)
  else
    FG2Module.ModuleLabel := ModuleName;
  //FG2Module.Fill.Color := ConvertToAlpha(ModuleColors[ModuleColor]);
  FG2Module.UnscaledHeight := (UNITS_ROW+UNIT_MARGIN*2) * HeightUnits;
  FG2Module.Row := Row;
  FG2Module.Col := Col;
  FG2Module.StateStyleList.DefaultFill.Color := ConvertToAlpha(ModuleColors[ModuleColor]);
  FG2Module.StateStyleList.SelectedFill.Color := ConvertToAlpha(ModuleColors[ModuleColor]);
end;
function TG2GraphModuleFMX.CreateCopy( aPatchPart: TG2FilePatchPart): TG2FileModule;
begin
  Result := TG2GraphModuleFMX.CopyCreate( aPatchPart, self);
end;
{function TG2GraphModuleFMX.CreateParameter: TG2FileParameter;
var Patch : TG2FilePatch;
begin
  Patch := nil;
  if assigned(PatchPart) then
    Patch := PatchPart.Patch;
  Result := TG2GraphParameterFMX.Create( Patch, TLocationType(Location), ModuleIndex, self);
end;}
function TG2GraphModuleFMX.CreateConnector: TG2FileConnector;
var Patch : TG2FilePatch;
begin
  Patch := nil;
  if assigned(PatchPart) then
    Patch := PatchPart.Patch;
  Result := TG2GraphConnectorFMX.Create( Patch, TLocationType(Location), self);
end;
destructor TG2GraphModuleFMX.Destroy;
begin
  inherited;
end;
procedure TG2GraphModuleFMX.FreePanel;
var i : integer;
begin
  for i := 0 to InConnectorCount - 1 do
    (InConnector[i] as TG2GraphConnectorFMX).FG2Connector := nil;
  for i := 0 to OutConnectorCount - 1 do
    (OutConnector[i] as TG2GraphConnectorFMX).FG2Connector := nil;
  FG2Module.DisposeOf;
end;
function TG2GraphModuleFMX.GetOutlineRect: TRectF;
begin
  Result.Left := Col * (UNITS_COL+UNIT_MARGIN*2);
  Result.Top := Row * (UNITS_ROW+UNIT_MARGIN*2);
  Result.Right := (Col + 1) * (UNITS_COL+UNIT_MARGIN*2);
  Result.Bottom := (Row + HeightUnits) * (UNITS_ROW+UNIT_MARGIN*2);
end;
procedure TG2GraphModuleFMX.InvalidateControl;
begin
  if assigned(FG2Module) then begin
    FG2Module.ModuleLabel := PatchPart.GetModuleLabel( ModuleIndex);
    FG2Module.Redraw;
  end;
end;
procedure TG2GraphModuleFMX.SetCol(aValue: TBits7);
begin
  inherited;
  if assigned(FG2Module) then
    FG2Module.Col := aValue;
  InvalidateCables;
end;
procedure TG2GraphModuleFMX.SetLocation(const aValue: TLocationType);
begin
  inherited;
  if assigned(FG2Module) then
    FG2Module.Location := aValue;
end;
procedure TG2GraphModuleFMX.SetModuleColor(aValue: TBits8);
begin
  inherited;
  if assigned(FG2Module) then begin
    FG2Module.StateStyleList.DefaultFill.Color := ConvertToAlpha(ModuleColors[aValue]);
    FG2Module.StateStyleList.SelectedFill.Color := ConvertToAlpha(ModuleColors[aValue]);
    FG2Module.Redraw;
  end;
end;
procedure TG2GraphModuleFMX.SetModuleIndex(const aValue: TBits8);
begin
  inherited;
  if assigned(FG2Module) then
    FG2Module.ModuleIndex := aValue;
end;
procedure TG2GraphModuleFMX.SetModuleName( aValue: string);
begin
  inherited;
  if assigned(FG2Module) then
    FG2Module.ModuleLabel := aValue;
end;
procedure TG2GraphModuleFMX.SetRow(aValue: TBits7);
begin
  inherited;
  if assigned(FG2Module) then
    FG2Module.Row := aValue;
  InvalidateCables;
end;
{procedure TG2GraphModuleFMX.SetSelected(aValue: boolean);
var Control : TFMXObject;
begin
  if aValue <> Selected then begin
    inherited;
    if assigned(FG2Module) then
      FG2Module.Selected := aValue;
      if aValue and assigned(FG2Module) then begin
        // Set focus on selected parameter control
        for Control in FG2Module.Children do begin
          if Control is TG2WriteControl then begin
            if (Control as TG2WriteControl).DataWriter = SelectedParam as IG2DataParam then begin
              (Control as TG2WriteControl).SetFocus;
              exit;
            end;
          end;
        end;
      end;
  end;
end;}
procedure TG2GraphModuleFMX.SelectModule(const aValue, aExclusive : boolean);
var Control : TFMXObject;
begin
  if aValue <> Selected then begin
    inherited;
    if assigned(FG2Module) then
      FG2Module.Selected := aValue;
      if aValue and aExclusive and assigned(FG2Module) and assigned(SelectedParam) then begin
        // Set focus on selected parameter control
        if FG2Module.ChildrenCount > 0 then begin
          for Control in FG2Module.Children do begin
            if Control is TG2WriteControl then begin
              if (Control as TG2WriteControl).DataWriter = SelectedParam as IG2DataParam then begin
                (Control as TG2WriteControl).SetFocus;
                exit;
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TG2GraphModuleFMX.PaintOnCanvas(aCanvas: TCanvas; aScale : single);
var i : Integer;
    G2Control : TG2BaseControl;
    SaveMatrix, M : TMatrix;
begin
  if not assigned(FG2Module) then
    exit;
  aCanvas.BeginScene;
  try
    SaveMatrix := aCanvas.Matrix;
    {$IF Defined(VER340)}
    M := TMatrix.CreateScaling( aScale, aScale);
    {$ELSE}
    M := CreateScaleMatrix( aScale, aScale);
    {$ENDIF}
    aCanvas.SetMatrix(M);
    G2Module.PaintOn( aCanvas);
    for i := 0 to G2Module.ChildrenCount - 1 do begin
      G2Control := (G2Module.Children[i] as TG2BaseControl);
      SaveMatrix := aCanvas.Matrix;
      try
        {$IF Defined(VER340)}
        aCanvas.SetMatrix( TMatrix.CreateTranslation( G2Control.UnscaledLeft, G2Control.UnscaledTop) * M);
        {$ELSE}
        aCanvas.SetMatrix( MatrixMultiply(CreateTranslateMatrix( G2Control.UnscaledLeft, G2Control.UnscaledTop), M));
        {$ENDIF}
        G2Control.PaintOn( aCanvas);
      finally
        aCanvas.SetMatrix( SaveMatrix);
      end;
    end;
    aCanvas.SetMatrix(SaveMatrix);
  finally
    aCanvas.EndScene;
  end;
end;
procedure ParseDependencies( aModule : TG2FileModule; aControl : TG2MultiReadControl);
var sl : TStringList;
    i, Value, c : integer;
    Param : TG2FileParameter;
begin
  sl := TStringList.Create;
  try
    Param := nil;
    sl.DelimitedText := aControl.Dependencies;
    // Find ref to master parameter
    if aControl.MasterRef >= 0  then begin
      i := 0;
      while (i<sl.Count) and (sl[i]<>IntToStr(aControl.MasterRef)) and (sl[i]<>'s'+IntToStr(aControl.MasterRef)) do
        inc(i);
      if (i<sl.Count) then begin
        // Assign master parameter first
        if (Lowercase(sl[ i][1]) = 's') then begin // One of the static params
          Param := aModule.Mode[ aControl.MasterRef];
          Param.TextFunctionIndex := aControl.TextFunc;
          aControl.AddDataDependency( Param, nil);
          //aControl.OnGetTextFunc := Param.GetTextFunc;
        end else begin
          Param := aModule.Parameter[ aControl.MasterRef];
          Param.TextFunctionIndex := aControl.TextFunc;
          aControl.AddDataDependency( Param, nil);
          //aControl.OnGetTextFunc := Param.GetTextFunc;
        end;
      end else begin
        Param := aModule.Parameter[ aControl.MasterRef];
        Param.TextFunctionIndex := aControl.TextFunc;
        aControl.AddDataDependency( Param, nil);
        //aControl.OnGetTextFunc := Param.GetTextFunc;
      end;
    end;
    if assigned(Param) then begin
      for i := 0 to sl.Count - 1 do begin
        if (sl[i].Length > 0) and (Lowercase(sl[i].Chars[0]) = 's') then begin
          val(sl[i].Substring(1, sl[i].Length - 1), Value, c);
          if c = 0 then begin
            Param.AddTextDependency( ptMode, Value);
            aControl.AddDataDependency( aModule.Mode[ value], nil);
          end;
        end else begin
          val(sl[i], Value, c);
          if c = 0 then begin
            Param.AddTextDependency( ptParam, Value);
            aControl.AddDataDependency( aModule.Parameter[ value], nil);
          end;
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;
//------------------------------------------------------------------------------
//
//                          Creation of module controls
//
//------------------------------------------------------------------------------
procedure CreateModuleControls( aModule : TG2GraphModuleFMX; aControl : TControl;
    aConnectParams, aConnectConnectors : boolean);
var G2 : TG2GraphFMX;
    G2ReadControl : TG2ReadControl;
    G2WriteControl : TG2WriteControl;
    G2MultiReadControl : TG2MultiReadControl;
    c : integer;
  procedure AssignToInConnector( aControl : TG2Connector);
  var Connector : TG2GraphConnectorFMX;
  begin
    if not aConnectConnectors then
      exit;
    Connector := aModule.InConnector[ aControl.CodeRef] as TG2GraphConnectorFMX;
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    Connector.G2Connector := aControl;
  end;
  procedure AssignToOutConnector( aControl : TG2Connector);
  var Connector : TG2GraphConnectorFMX;
  begin
    if not aConnectConnectors then
      exit;
    Connector := aModule.OutConnector[ aControl.CodeRef] as TG2GraphConnectorFMX;
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    Connector.G2Connector := aControl as TG2Connector;
  end;
  procedure AssignReaderToParameter( aControl : TG2ReadControl);
  var Param : TG2FileParameter;
  begin
    if not aConnectParams then
      exit;
    Param := aModule.Parameter[ aControl.CodeRef];
    if not assigned(Param) then
      raise Exception.Create('Parameter ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    aControl.DataReader := Param;
    //aControl.OnGetTextFunc := Param.GetTextFunc;
    if aControl.InfoFunc <> -1 then
      Param.InfoFunctionIndex := aControl.InfoFunc;
  end;
  procedure AssignWriterToParameter( aControl : TG2WriteControl);
  var Param : TG2FileParameter;
  begin
    if not aConnectParams then
      exit;
    Param := aModule.Parameter[ aControl.CodeRef];
    if not assigned(Param) then
      raise Exception.Create('Parameter ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    aControl.DataWriter := Param;
    //aControl.OnGetTextFunc := Param.GetTextFunc;
    if aControl.InfoFunc <> -1 then
      Param.InfoFunctionIndex := aControl.InfoFunc;
  end;
  procedure AssignWriterToMode( aControl : TG2WriteControl);
  var Param : TG2FileParameter;
  begin
    if not aConnectParams then
      exit;
    Param := aModule.Mode[ aControl.CodeRef];
    if not assigned(PAram) then
      raise Exception.Create('Mode ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    aControl.DataWriter := Param;
  end;
  procedure AddToLedList( aLed : TG2Led);
  var Patch : TG2GraphPatchFMX;
      i : integer;
  begin
    Patch := nil;
    if Assigned(aModule.PatchPart) then
      Patch := aModule.PatchPart.Patch as TG2GraphPatchFMX;
    if assigned(Patch) then begin
      i := 0;
      while (i<Patch.LedList.Count) and not((Patch.LedList[i].Location = aModule.Location)
                                        and (Patch.LedList[i].ModuleIndex = aModule.ModuleIndex)
                                        and (Patch.LedList[i].GroupID = aLed.LedGroupID)) do
        inc(i);
      if (i<Patch.LedList.Count) then begin
        aLed.DataLed := Patch.LedList[i];
      end;
    end;
  end;
  procedure AddToLedStripList( aLed : TG2Led);
  var Patch : TG2GraphPatchFMX;

      i : integer;

  begin
    Patch := nil;
    if Assigned(aModule.PatchPart) then
      Patch := aModule.PatchPart.Patch as TG2GraphPatchFMX;
    if assigned(Patch) then begin
      i := 0;
      while (i<Patch.LedStripList.Count) and not((Patch.LedStripList[i].Location = aModule.Location)
                                             and (Patch.LedStripList[i].ModuleIndex = aModule.ModuleIndex)
                                             and (Patch.LedStripList[i].GroupID = aLed.LedGroupID)) do
        inc(i);
      if (i<Patch.LedStripList.Count) then begin
        aLed.DataLed := Patch.LedStripList[i];
      end;
    end;
  end;
  procedure SetCommonProperties( aG2Control : TG2BaseControl; aID, aXPos, aYPos : integer);
  begin
    aG2Control.Parent := aControl;
    aG2Control.ModuleIndex := aModule.ModuleIndex;
    aG2Control.Location := aModule.Location;
    aG2Control.ID := aID;
    aG2Control.UnscaledLeft := aXPos + UNIT_MARGIN;
    aG2Control.UnscaledTop := aYPos + (1 + aYPos / UNITS_ROW) * UNIT_MARGIN;
    if assigned(aModule.OnCreateG2Control) then
      aModule.OnCreateG2Control(aModule, aG2Control);
  end;
  function CreateInput( aConnectorDef : TG2ConnectorDef): TG2Connector;
  begin
    Result := TG2Connector.Create(aControl);
    SetCommonProperties( Result, aConnectorDef.ID, aConnectorDef.XPos, aConnectorDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aConnectorDef.CodeRef;
    Result.ConnectorType := aConnectorDef.ConnectorType;
    Result.BandWidth := aConnectorDef.Bandwidth;
    AssignToInConnector( Result);
    Result.EndUpdate;
  end;
  function CreateOutput( aConnectorDef : TG2ConnectorDef): TG2Connector;
  begin
    Result := TG2Connector.Create(aControl);
    SetCommonProperties( Result, aConnectorDef.ID, aConnectorDef.XPos, aConnectorDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aConnectorDef.CodeRef;
    Result.ConnectorType := aConnectorDef.ConnectorType;
    Result.BandWidth := aConnectorDef.Bandwidth;
    AssignToOutConnector( Result);
    Result.EndUpdate;
  end;
  function CreateLabel( aTextDef : TG2TextDef): TG2Label;
  var StyleSet : TG2StyleSet;
  begin
    Result := TG2Label.Create(aControl);
    SetCommonProperties( Result, aTextDef.ID, aTextDef.XPos, aTextDef.YPos);
    Result.BeginUpdate;
    Result.LabelText := aTextDef.slText;
    StyleSet := Result.StateStyleList.StateStyle[ csDefault];
    StyleSet.Font.Size := aTextDef.FontSize-2;
    Result.EndUpdate;
  end;
  function CreateLed( aLedDef : TG2LedDef): TG2LedGreen;
  begin
    Result := TG2LedGreen.Create(aControl);
    SetCommonProperties( Result, aLedDef.ID, aLedDef.XPos, aLedDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aLedDef.CodeRef;
    Result.InfoFunc := aLedDef.InfoFunc;
    Result.LedType := aLedDef.LedType;
    Result.LedGroupID := aLedDef.GroupID;
    case aLedDef.LedType of
      ltSequencer:
        begin
          Result.UnscaledWidth := 12;
          Result.UnscaledHeight := 7;
          AddToLedStripList( Result);
        end;
      ltGreen:
        begin
          Result.UnscaledWidth := 8;
          Result.UnscaledHeight := 8;
          if GetDataLedsInGroup( aModule.TypeID, aLedDef.GroupID) > 1 then
            AddToLedStripList( Result)
          else
           AddToLedList( Result);
        end;
    end;
    Result.EndUpdate;
  end;
  function CreateMiniVU( aMiniVUDef : TG2MiniVUDef): TG2MiniVU;
  begin
    Result := TG2MiniVU.Create(aControl);
    SetCommonProperties( Result, aMiniVUDef.ID, aMiniVUDef.XPos, aMiniVUDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aMiniVUDef.CodeRef;
    Result.InfoFunc := aMiniVUDef.InfoFunc;
    Result.LegGroupID := aMiniVUDef.GroupID;
    Result.LedType := ltMiniVU;
    AddToLedStripList( Result);
    Result.EndUpdate;
  end;
  function CreateTextField( aTextFieldDef : TG2TextFieldDef): TG2TextField;
  begin
    Result := TG2TextField.Create(aControl);
    SetCommonProperties( Result, aTextFieldDef.ID, aTextFieldDef.XPos, aTextFieldDef.YPos);
    Result.BeginUpdate;
    Result.UnscaledWidth := aTextFieldDef.Width;
    Result.MasterRef := aTextFieldDef.MasterRef;
    Result.TextFunc := aTextFieldDef.TextFunc;
    Result.Dependencies := aTextFieldDef.slDependencies;
    ParseDependencies( aModule, Result);
    Result.EndUpdate;
  end;
  function CreateTextEdit( aTextEditDef :  TG2TextEditDef): TG2BtnTextEdit;
  begin
    Result := TG2BtnTextEdit.Create(aControl);
    SetCommonProperties( Result, aTextEditDef.ID, aTextEditDef.XPos, aTextEditDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aTextEditDef.CodeRef;
    Result.InfoFunc := aTextEditDef.InfoFunc;
    Result.ButtonText.DelimitedText := aTextEditDef.slText;
    case aTextEditDef.TextEditType of
      bttPush:
        begin
          Result.ButtonKind := bkMomentary
        end;
      bttCheck:
        begin
          Result.ButtonKind := bkToggle
        end;
    end;
    Result.UnscaledWidth := aTextEditDef.Width;
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreateKnob( aKnobDef : TG2KnobDef): TG2Knob;
  begin
    Result := TG2Knob.Create(aControl);
    SetCommonProperties( Result, aKnobDef.ID, aKnobDef.XPos, aKnobDef.YPos);
    Result.BeginUpdate; // SetParent sets FUpdating to 0
    Result.CodeRef := aKnobDef.CodeRef;
    Result.InfoFunc := aKnobDef.InfoFunc;
    Result.KnobType := aKnobDef.KnobType;
    Result.SetDefaultDimensions;
    AssignWriterToParameter( Result);
    if assigned(aModule.PatchPart) and assigned(aModule.PatchPart.Patch) and assigned(aModule.PatchPart.Patch.G2) then begin
      G2 := aModule.PatchPart.Patch.G2 as TG2GraphFMX;
      Result.KnobControl := G2.KnobControl;
    end;
    Result.EndUpdate;
  end;
  function CreateButtonText( aButtonTextDef : TG2ButtonTextDef): TG2BtnText;
  begin
    Result := TG2BtnText.Create(aControl);
    SetCommonProperties( Result, aButtonTextDef.ID, aButtonTextDef.XPos, aButtonTextDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonTextDef.CodeRef;
    Result.InfoFunc := aButtonTextDef.InfoFunc;
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonTextDef.slText;
    Result.ButtonType := aButtonTextDef.ButtonType;
    Result.UnscaledWidth := aButtonTextDef.Width;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aButtonTextDef.slImageID;
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreateButtonFlat( aButtonFlatDef : TG2ButtonFlatDef): TG2BtnFlat;
  begin
    Result := TG2BtnFlat.Create(aControl);
    SetCommonProperties( Result, aButtonFlatDef.ID, aButtonFlatDef.XPos, aButtonFlatDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonFlatDef.CodeRef;
    Result.InfoFunc := aButtonFlatDef.InfoFunc;
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonFlatDef.slText;
    Result.UnscaledWidth := aButtonFlatDef.Width;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aButtonFlatDef.slImageID;
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreateButtonIncDec( aButtonIncDecDef : TG2ButtonIncDecDef): TG2BtnIncDec;
  begin
    Result := TG2BtnIncDec.Create(aControl);
    SetCommonProperties( Result, aButtonIncDecDef.ID, aButtonIncDecDef.XPos, aButtonIncDecDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonIncDecDef.CodeRef;
    Result.InfoFunc := aButtonIncDecDef.InfoFunc;
    case aButtonIncDecDef.ButtonType of
      bidLeftRight:
        begin
          Result.Orientation := otHorizontal;
        end;
      bidUpDown:
        begin
          Result.Orientation := otVertical;
        end;
    end;
    Result.CalcDimensionsFromBtnSize( Result.ButtonWidth, Result.ButtonHeight);
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreateLevelShift( aLevelShiftDef : TG2LevelShiftDef): TG2BtnFlat;
  begin
    Result := TG2BtnFlat.Create(aControl);
    SetCommonProperties( Result, aLevelShiftDef.ID, aLevelShiftDef.XPos, aLevelShiftDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aLevelShiftDef.CodeRef;
    Result.InfoFunc := aLevelShiftDef.InfoFunc;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aLevelShiftDef.slImageID;
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreateButtonRadio( aButtonRadioDef : TG2ButtonRadioDef): TG2BtnRadio;
  begin
    Result := TG2BtnRadio.Create(aControl);
    SetCommonProperties( Result, aButtonRadioDef.ID, aButtonRadioDef.XPos, aButtonRadioDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonRadioDef.CodeRef;
    Result.InfoFunc := aButtonRadioDef.InfoFunc;
    Result.Orientation := aButtonRadioDef.Orientation;
    Result.ButtonCount := aButtonRadioDef.ButtonCount;
    Result.ButtonWidth := aButtonRadioDef.ButtonWidth;
    Result.CalcDimensionsFromBtnSize( Result.ButtonWidth, Result.ButtonHeight);
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonRadioDef.slText;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aButtonRadioDef.slImageID;
    Result.ImageWidth := aButtonRadioDef.ImageWidth;
    Result.UpsideDown := True;
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreateButtonRadioEdit( aButtonRadioEditDef : TG2ButtonRadioEditDef): TG2BtnRadioEdit;
  begin
    Result := TG2BtnRadioEdit.Create(aControl);
    SetCommonProperties( Result, aButtonRadioEditDef.ID, aButtonRadioEditDef.XPos, aButtonRadioEditDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonRadioEditDef.CodeRef;
    Result.InfoFunc := aButtonRadioEditDef.InfoFunc;
    Result.ButtonColumns := aButtonRadioEditDef.ButtonColumns;
    Result.ButtonRows := aButtonRadioEditDef.ButtonRows;
    Result.CalcDimensionsFromBtnSize( Result.ButtonWidth, Result.ButtonHeight);
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonRadioEditDef.slText;
    AssignWriterToParameter( Result);
    Result.EndUpdate;
  end;
  function CreatePartSelector( aPartSelectorDef : TG2PartSelectorDef): TG2PartSelector;
  begin
    Result := TG2PartSelector.Create(aControl);
    SetCommonProperties( Result, aPartSelectorDef.ID, aPartSelectorDef.XPos, aPartSelectorDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aPartSelectorDef.CodeRef;
    Result.InfoFunc := aPartSelectorDef.InfoFunc;
    Result.UnscaledWidth := aPartSelectorDef.Width;
    Result.UnscaledHeight := aPartSelectorDef.Height;
    Result.OptionsText.Delimiter := ';';
    Result.OptionsText.DelimitedText := aPartSelectorDef.slText;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aPartSelectorDef.slImageID;
    //Result.ImageWidth := aPartSelectorDef.ImageWidth;
    //Result.ImageCount := aPartSelectorDef.ImageCount;
    AssignWriterToMode( Result);
    Result.EndUpdate;
  end;
begin
  for c := 0 to High(LineDefs) do begin
    if LineDefs[c].ModuleID = aModule.TypeID then begin
      //
    end;
  end;
  for c := 0 to High(BitmapDefs) do begin
    if BitmapDefs[c].ModuleID = aModule.TypeID then begin
      //
    end;
  end;
  for c := 0 to High(TextDefs) do begin
    if TextDefs[c].ModuleID = aModule.TypeID then begin
      CreateLabel( TextDefs[c]);
    end;
  end;
  for c := 0 to High(ModuleInputs) do begin
    if ModuleInputs[c].ModuleID = aModule.TypeID then begin
      CreateInput( ModuleInputs[c]);
    end;
  end;
  for c := 0 to High(ModuleOutputs) do begin
    if ModuleOutputs[c].ModuleID = aModule.TypeID then begin
      CreateOutput( ModuleOutputs[c]);
    end;
  end;
  for c := 0 to High(TextFieldDefs) do begin
    if TextFieldDefs[c].ModuleID = aModule.TypeID then begin
      CreateTextField( TextFieldDefs[c]);
    end;
  end;
  for c := 0 to High(GraphDefs) do begin
    if GraphDefs[c].ModuleID = aModule.TypeID then begin
    end;
  end;
  for c := 0 to High(LedDefs) do begin
    if LedDefs[c].ModuleID = aModule.TypeID then begin
      CreateLed( LedDefs[c]);
    end;
  end;
  for c := 0 to High(MiniVUDefs) do begin
    if MiniVUDefs[c].ModuleID = aModule.TypeID then begin
      CreateMiniVU( MiniVUDefs[c]);
    end;
  end;
  for c := 0 to High(PartSelectorDefs) do begin
    if PartSelectorDefs[c].ModuleID = aModule.TypeID then begin
      CreatePartSelector( PartSelectorDefs[c]);
    end;
  end;
  for c := 0 to High(ButtonTextDefs) do begin
    if ButtonTextDefs[c].ModuleID = aModule.TypeID then begin
      CreateButtonText( ButtonTextDefs[c]);
    end;
  end;
  for c := 0 to High(TextEditDefs) do begin
    if TextEditDefs[c].ModuleID = aModule.TypeID then begin
      CreateTextEdit( TextEditDefs[c]);
    end;
  end;
  for c := 0 to High(ButtonFlatDefs) do begin
    if ButtonFlatDefs[c].ModuleID = aModule.TypeID then begin
      CreateButtonFlat( ButtonFlatDefs[c]);
    end;
  end;
  for c := 0 to High(LevelShiftDefs) do begin
    if LevelShiftDefs[c].ModuleID = aModule.TypeID then begin
      CreateLevelShift( LevelShiftDefs[c]);
    end;
  end;
  for c := 0 to High(ButtonRadioDefs) do begin
    if ButtonRadioDefs[c].ModuleID = aModule.TypeID then begin
      CreateButtonRadio( ButtonRadioDefs[c]);
    end;
  end;
  for c := 0 to High(ButtonRadioEditDefs) do begin
    if ButtonRadioEditDefs[c].ModuleID = aModule.TypeID then begin
      CreateButtonRadioEdit( ButtonRadioEditDefs[c]);
    end;
  end;
  for c := 0 to High(ButtonIncDecDefs) do begin
    if ButtonIncDecDefs[c].ModuleID = aModule.TypeID then begin
      CreateButtonIncDec( ButtonIncDecDefs[c]);
    end;
  end;
  for c := 0 to High(KnobDefs) do begin
    if KnobDefs[c].ModuleID = aModule.TypeID then begin
      CreateKnob( KnobDefs[c]);
    end;
  end;
end;
end.

{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

// BVE Contains FIX for TTabItem tab order

unit FMX.TabControl;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.UIConsts, System.Types, System.Actions,
  FMX.ActnList, FMX.Types, FMX.Objects, FMX.StdCtrls, FMX.Controls, FMX.MultiResBitmap,
  FMX.Effects, FMX.Graphics;

{$SCOPEDENUMS ON}

type

  TTabControl = class;
  TTabItem = class;

  TTabTransition = (ttNone, ttSlide);
  TTabTransitionDirection = (tdNormal, tdReversed);

  TChangeTabAction = class(TCustomAction)
  private
    FTransition: TTabTransition;
    FDirection: TTabTransitionDirection;
    FTab: TTabItem;
    procedure SetTab(const Value: TTabItem);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure CustomTextChanged; override;
    function Update: Boolean; override;
  published
    property CustomText;
    property Tab: TTabItem read FTab write SetTab;
    property Direction: TTabTransitionDirection read FDirection write FDirection default TTabTransitionDirection.tdNormal;
    property Transition: TTabTransition read FTransition write FTransition default TTabTransition.ttNone;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnUpdate;
  end;

{ TTabItem }

  TTabItem = class(TTextControl)
  private
    FTabControl: TTabControl;
    FContent: TContent;
    FAutoWidth: Single;
    FAutoHeight: Single;
    FAutoSize: Boolean;
    FLeftOffset: Single;
    FRightOffset: Single;
    FIsSelected: Boolean;
    FSizeCalculating: Boolean;
    FCustomIcon: TImageData;
    FItemStyle: TControl;
    FIconObject: TImage;
    FItemText: TControl;
    function GetLeftOffset: Single;
    function GetRightOffset: Single;
    function GetAutoWidth: Single;
    function GetAutoHeight: Single;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetIsSelected(const Value: Boolean);
    procedure SetCustomIcon(const Value: TImageData);
    procedure DoIconChanged(AObject: TObject);
  protected
    procedure CalcSize;
    procedure UpdateIcon;
    procedure SetWidth(const Value: Single); override;
    procedure SetHeight(const Value: Single); override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoChanged; override;
    procedure SetText(const Value: string); override;
    function FindTextObject: TFmxObject; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChangeParent; override;
    procedure Hide; override;
    procedure Show; override;
    function DoSetHeight(var Value: Single; NewValue: single; var LastValue: Single): boolean; override;
    function DoSetWidth(var Value: Single; NewValue: single; var LastValue: Single): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { props }
    property Align;
    property AutoTranslate default True;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property WordWrap default True;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property CustomIcon: TImageData read FCustomIcon write SetCustomIcon;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Font;
    property StyledSettings;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    { trigger }
    property IsSelected: Boolean read FIsSelected write SetIsSelected;
    property Index stored False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property Text;
    property TextAlign default TTextAlign.taCenter;
    property Trimming;
    property TouchTargetExpansion;
    property VertTextAlign;
    property Visible default True;
    property Width;
    property OnApplyStyleLookup;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TTabControl }

  TTabPosition = (tpTop, tpBottom, tpNone, tpDots);

  TTabControl = class(TStyledControl, IItemsContainer)
  private
    FContent: TContent;
    FNoItemsContent: TControl;
    FTabIndex: Integer;
    FTabHeight: Single;
    FFullSize: Boolean;
    FTabPosition: TTabPosition;
    FBackground: TControl;
    FFixedTabHeight: Boolean;
    FOnChange: TNotifyEvent;
    FTransitionRunning: Boolean;
    FTransitionTabs: array of TTabItem;
    procedure SetTabIndex(const Value: integer);
    procedure SetTabHeight(const Value: Single);
    procedure SetFullSize(const Value: Boolean);
    function GetActiveTab: TTabItem;
    procedure SetActiveTab(const Value: TTabItem);
    procedure SetTabPosition(const Value: TTabPosition);
    procedure ApplyTabsStyleLookup;
    { IItemContainer }
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    procedure UpdateItemContentBounds(LItem: TTabItem);
    procedure WebBrowserReallign(Sender: TObject);
    procedure AnimationFinished(Sender: TObject);
  protected
    function GetTabItem(AIndex: Integer): TTabItem;
    function GetTabCount: Integer;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure PaintChildren; override;
    procedure RealignTabs; virtual;
    procedure DoRealign; override;
    procedure ContentAddObject(const AObject: TFmxObject);
    procedure ContentRemoveObject(const AObject: TFmxObject);
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetActiveTabWithTransition(const ATab: TTabItem; ATransition: TTabTransition;
      const ADirection: TTabTransitionDirection = TTabTransitionDirection.tdNormal);
    function HasActiveTab: Boolean;
    property TabCount: Integer read GetTabCount;
    property Tabs[AIndex: Integer]: TTabItem read GetTabItem;
  published
    property Align;
    property Anchors;
    property ActiveTab: TTabItem read GetActiveTab write SetActiveTab stored False;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property FullSize: Boolean read FFullSize write SetFullSize default False;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabHeight: Single read FTabHeight write SetTabHeight;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property TabOrder;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default TTabPosition.tpTop;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property OnApplyStyleLookup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

implementation

uses
  FMX.Consts, FMX.Ani, FMX.Layouts, System.Math, FMX.Forms, FMX.Platform,
  FMX.Pickers, FMX.WebBrowser, FMX.TextLayout.GPU, System.Rtti;

{ TTabItemContent }

type
  TTabItemContent = class(TContent);
  TOpenControl = class(TControl);
  TOpenStyledControl = class(TStyledControl);

{ TChangeTabAction }

procedure TChangeTabAction.CustomTextChanged;
begin
  if CustomText = '' then
  begin
    if Assigned(FTab) then
      Text := Format(SGotoTab, [FTab.Text])
    else
      Text := SGotoNilTab;
  end
  else
    Text := CustomText;
end;

procedure TChangeTabAction.ExecuteTarget(Target: TObject);
var LDirection: TTabTransitionDirection;
begin
  inherited;
  if Assigned(FTab) and Assigned(FTab.FTabControl) then
  begin
    LDirection := FDirection;
    if FTab.Index < FTab.FTabControl.TabIndex then
    begin
      if LDirection = TTabTransitionDirection.tdNormal then
        LDirection := TTabTransitionDirection.tdReversed
      else if LDirection = TTabTransitionDirection.tdReversed then
        LDirection := TTabTransitionDirection.tdNormal;
    end;
    FTab.FTabControl.SetActiveTabWithTransition(FTab, FTransition, LDirection);
  end;
end;

function TChangeTabAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Supported;
end;

function TChangeTabAction.Update: Boolean;
begin
  if Supported and ([csLoading, csDestroying] * ComponentState = []) then
    CustomTextChanged;
  Result := inherited Update;
  if not Result then
    Enabled := Supported and
               Assigned(FTab) and
               Assigned(FTab.FTabControl) and
               (FTab.FTabControl.TabIndex <> FTab.Index);
end;

procedure TChangeTabAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTab) then
    Tab := nil;
end;

procedure TChangeTabAction.SetTab(const Value: TTabItem);
begin
  if FTab <> Value then
  begin
    FTab := Value;
    CustomTextChanged;
  end;
end;

{ TTabItem }

constructor TTabItem.Create(AOwner: TComponent);
var
  DefaultValueService: IInterface;
  TrimmingDefault: TValue;
begin
  inherited;
  FCustomIcon := TImageData.Create;
  FCustomIcon.OnChange := DoIconChanged;
  FAutoSize := True;
  FContent := TTabItemContent.Create(nil);
  FContent.Parent := Self;
  FContent.Locked := True;
  FContent.Stored := False;
  FContent.HitTest := False;
  FContent.Visible := False;
  FContent.ClipChildren := True;
  FAutoWidth := 0;
  FAutoHeight := 0;
  FDesignInteractive := True;
  AutoTranslate := True;
  TextAlign := TTextAlign.taCenter;
  HitTest := True;

  if (csDesigning in ComponentState)
    and SupportsPlatformService(IFMXDefaultPropertyValueService, DefaultValueService) then
  begin
    TrimmingDefault := IFMXDefaultPropertyValueService(DefaultValueService).GetDefaultPropertyValue(Self.ClassName, 'trimming');
    if not TrimmingDefault.IsEmpty then
      Trimming := TrimmingDefault.AsType<TTextTrimming>;
  end;
end;

destructor TTabItem.Destroy;
begin
  FreeAndNil(FCustomIcon);
  inherited;
end;

procedure TTabItem.DoAddObject(const AObject: TFmxObject);
var
  ControlTmp: TControl;
begin
  if Assigned(FContent) and not AObject.Equals(FContent) and not AObject.Equals(ResourceLink) then
  begin
    FContent.AddObject(AObject);

    AddToTabList(AObject);  // FIX

    // Correct control position in desgin time
    if (csDesigning in ComponentState) and (AObject is TControl) then
    begin
      ControlTmp := AObject as TControl;
      ControlTmp.Position.X := Max(0, ControlTmp.Position.X);
      ControlTmp.Position.Y := Max(0, ControlTmp.Position.Y);
    end;
  end
  else
    inherited;
end;

procedure TTabItem.DoIconChanged(AObject: TObject);
begin

end;

procedure TTabItem.UpdateIcon;
var
  ImageGenSrv: IFMXImageGeneratorService;
begin
  if Assigned(FIconObject) then
  begin
    if SupportsPlatformService(IFMXImageGeneratorService, IInterface(ImageGenSrv)) then
    begin
      if Assigned(Scene) and (Scene.GetSceneScale > 1.0) then
      begin
        if not FCustomIcon.BitmapHiRes.IsEmpty then
          ImageGenSrv.GenerateTabIcon(FCustomIcon.BitmapHiRes, FIconObject.Bitmap, IsSelected, True)
        else
          ImageGenSrv.GenerateTabIcon(FCustomIcon.Bitmap, FIconObject.Bitmap, IsSelected, True);
      end else
        ImageGenSrv.GenerateTabIcon(FCustomIcon.Bitmap, FIconObject.Bitmap, IsSelected, False);
    end
    else
    begin
      FIconObject.Bitmap := FCustomIcon.Bitmap;
      if not FCustomIcon.BitmapHiRes.IsEmpty then
        FIconObject.Bitmap := FCustomIcon.BitmapHiRes
      else
        FIconObject.Bitmap := FCustomIcon.Bitmap;
    end;
  end;
end;

procedure TTabItem.ApplyStyle;
var
  B: TFmxObject;
begin
  if Assigned(FTabControl) and Assigned(ResourceLink) then
  begin
    case FTabControl.TabPosition of
      TTabPosition.tpTop: begin
        B := ResourceLink.FindStyleResource('top');
        if Assigned(B) and (B is TControl) then
        begin
          FItemStyle := TControl(B);
          FItemStyle.Visible := True;
          B := ResourceLink.FindStyleResource('bottom');
          if Assigned(B) and (B is TControl) then
            TControl(B).Visible := False;
        end;
      end;
      TTabPosition.tpBottom: begin
        B := ResourceLink.FindStyleResource('bottom');
        if Assigned(B) and (B is TControl) then
        begin
          FItemStyle := TControl(B);
          FItemStyle.Visible := True;
          B := ResourceLink.FindStyleResource('top');
          if Assigned(B) and (B is TControl) then
            TControl(B).Visible := False;
        end;
      end;
    end;
    if (not Assigned(FItemStyle)) and Assigned(ResourceControl) then
    begin
      FItemStyle := ResourceControl;
      FItemStyle.Visible := True;
    end;
    // custom icon
    if Assigned(FItemStyle) then
    begin
      B := TControl(FItemStyle.FindStyleResource('icon'));
      if Assigned(B) then
        FIconObject := TImage(B);
    end;
  end;
  inherited;
  CalcSize;
  IsSelected := Assigned(FTabControl) and (FTabControl.TabIndex = Index);
end;

function TTabItem.FindTextObject: TFmxObject;
begin
  if Assigned(FItemStyle) then
    Result := FItemStyle.FindStyleResource('text')
  else
    Result := inherited FindTextObject;
end;

procedure TTabItem.FreeStyle;
begin
  inherited;
  FIconObject := nil;
  FItemStyle := nil;
  FItemText := nil;
end;

procedure TTabItem.Loaded;
begin
  inherited;
  // necessary to update FLastWidth and FLastHeight for anchors
  TTabItemContent(FContent).Loaded;
  // necessary to apply TextSettings from style
  IsSelected := Assigned(FTabControl) and (FTabControl.TabIndex = Index);
end;

procedure TTabItem.CalcSize;
var
  S: TAlignLayout;
  TextMarginRect: TRectF;
begin
  if FSizeCalculating or Text.IsEmpty then
    Exit;

  FSizeCalculating := True;
  try
    if Assigned(FItemStyle) then
    begin
      FLeftOffset := FItemStyle.Margins.Left;
      FRightOffset := FItemStyle.Margins.Right;
    end;

    ApplyStyleLookup; // force load style
    if not Assigned(FItemStyle) then
      Exit;

    if Assigned(TextObject) then
    begin
      TextMarginRect := TextObject.Margins.Rect;
      TText(TextObject).Text := Text;
      TText(TextObject).WordWrap := False;
      S := TextObject.Align;
      try
        TextObject.Align := TAlignLayout.alNone;
        TText(TextObject).AutoSize := True;
        FAutoWidth := TextObject.Width + TextMarginRect.Left + TextMarginRect.Right;
        FAutoHeight := TextObject.Height + TextMarginRect.Top + TextMarginRect.Bottom;
        TText(TextObject).AutoSize := False;
      finally
        TextObject.Align := S;
      end;
    end;
  finally
    FSizeCalculating := False;
  end;
end;

procedure TTabItem.DoChanged;
begin
  inherited;
  if FSizeCalculating then
    Exit;
  CalcSize;
  if Assigned(FTabControl) then
    FTabControl.RealignTabs;
end;

procedure TTabItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = TMouseButton.mbLeft then
  begin
    if Assigned(FTabControl) then
      FTabControl.TabIndex := Index;
    {$IFDEF MSWINDOWS}
    if (csDesigning in ComponentState) and IsSelected and (Owner is TCommonCustomForm) and Assigned(TCommonCustomForm(Owner).Designer) then
      TCommonCustomForm(Owner).Designer.SelectComponent(Self);
    {$ENDIF}
  end;
end;

function TTabItem.DoSetHeight(var Value: Single; NewValue: single; var LastValue: Single): boolean;
begin
  Result := inherited DoSetHeight(Value, NewValue, LastValue);
  if Result and Assigned(FTabControl) and (FTabControl.TabHeight <> 0) and
    (FTabControl.TabPosition in [TTabPosition.tpTop, TTabPosition.tpBottom]) then
  begin
    Result := not SameValue(NewValue + Margins.Top + Margins.Bottom, FTabControl.TabHeight);
  end;
end;

function TTabItem.DoSetWidth(var Value: Single; NewValue: single; var LastValue: Single): boolean;
var
  SaveValue: Single;
begin
  SaveValue := Value;
  Result := inherited DoSetWidth(Value, NewValue, LastValue);
  if Result and (csDesigning in ComponentState) and Assigned(FTabControl) and (FTabControl.FullSize) and
     not (FTabControl.FDisableAlign) and (FTabControl.TabPosition in [TTabPosition.tpTop, TTabPosition.tpBottom]) then
    Value := SaveValue;
end;

procedure TTabItem.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Height := GetAutoHeight;
    Width := GetAutoWidth;
    DoChanged;
  end;
end;

procedure TTabItem.SetCustomIcon(const Value: TImageData);
begin
  FCustomIcon.Assign(Value);
end;

procedure TTabItem.SetHeight(const Value: Single);
begin
  if FAutoSize then
    FHeight := FAutoHeight;
  inherited;
  DoChanged;
end;

procedure TTabItem.SetIsSelected(const Value: Boolean);
begin
  FIsSelected := Value;
  StartTriggerAnimation(Self, 'IsSelected');
  ApplyTriggerEffect(Self, 'IsSelected');
  UpdateIcon;
end;

procedure TTabItem.ChangeParent;

  function FindTabControl: TTabControl;
  var
    P: TFmxObject;
  begin
    P := Parent;
    while Assigned(P) do
    begin
      if P is TTabControl then
      begin
        Result := P as TTabControl;
        Exit;
      end;
      P := P.Parent;
    end;
    Result := nil;
  end;

begin
  inherited ChangeParent;
  FTabControl := FindTabControl;
  NeedStyleLookup;
end;

procedure TTabItem.SetText(const Value: string);
begin
  if Text <> Value then
  begin
    NeedStyleLookup;
    inherited;
    CalcSize;
    if Assigned(FTabControl) then
      FTabControl.RealignTabs;
  end;
end;

procedure TTabItem.SetWidth(const Value: Single);
begin
  if FAutoSize then
    FWidth := FAutoWidth;
  inherited;
  if not (csLoading in ComponentState) then
    DoChanged;
end;

procedure TTabItem.Show;
begin
  inherited Show;
  if Assigned(FTabControl) then
    FTabControl.Realign;
end;

function TTabItem.GetAutoHeight: Single;
begin
  if FAutoSize then
  begin
    if not (FAutoHeight > 0) then
      CalcSize;
    Result := FAutoHeight
  end
  else
    Result := FHeight;
end;

function TTabItem.GetAutoWidth: Single;
begin
  if FAutoSize then
  begin
    if not (FAutoWidth > 0)  then
      CalcSize;
    Result := FAutoWidth
  end
  else
    Result := FWidth;
end;

function TTabItem.GetLeftOffset: Single;
begin
  Result := FLeftOffset;
end;

function TTabItem.GetRightOffset: Single;
begin
  Result := FRightOffset;
end;

procedure TTabItem.Hide;
begin
  inherited Hide;
  IsSelected := False;
  if Assigned(FTabControl) and (FTabControl.TabIndex = Index) then
    FTabControl.ActiveTab := nil;
end;

type
  TTabControlContent = class(TContent)
  protected
    procedure DoRealign; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  end;

{ TTabContent }

procedure TTabControlContent.DoRealign;
begin
  // We have own realign method in TTabControl
end;

procedure TTabControlContent.DoAddObject(const AObject: TFmxObject);
begin
  if AObject is TTabItem then
    inherited DoAddObject(AObject);

  if Assigned(Parent) and (Parent is TTabControl) then
    TTabControl(Parent).ContentAddObject(AObject);
end;

procedure TTabControlContent.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TTabControl) then
    TTabControl(Parent).ContentRemoveObject(AObject);
end;

{ TTabControl }

constructor TTabControl.Create(AOwner: TComponent);
begin
  inherited;
  FContent := TTabControlContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
  FNoItemsContent := TControl.Create(nil);
  FNoItemsContent.Parent := Self;
  FNoItemsContent.Stored := False;
  FNoItemsContent.Locked := True;
  FNoItemsContent.HitTest := False;
  FTabIndex := -1;
  FTabPosition := TTabPosition.tpTop;
  AutoCapture := True;
  SetBounds(0, 0, 200, 200);
  SetAcceptsControls(True);
end;

function TTabControl.GetActiveTab: TTabItem;
begin
  if InRange(TabIndex, 0, TabCount - 1) then
    Result := Tabs[TabIndex]
  else
    Result := nil;
end;

function TTabControl.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Tabs[AIndex];
end;

function TTabControl.GetItemsCount: Integer;
begin
  Result := TabCount;
end;

destructor TTabControl.Destroy;
begin
  inherited;
end;

procedure TTabControl.DoAddObject(const AObject: TFmxObject);
begin
  // If AObject is TabItem, then we add it to the tab items container (FContent)
  // If AObject is Effect, Animation, Style resource, we add it to the Self
  // In all other cases, we add AObject to active tab or special container (FNoItemsContent)
  if AObject is TTabItem then
  begin
    FContent.AddObject(AObject);

    AddToTabList(AObject);//FIX

    RealignTabs;
    if not (csLoading in ComponentState) and (csDesigning in ComponentState) and
       not HasActiveTab then
      ActiveTab := AObject as TTabItem;
  end
  else
    if (AObject is TEffect) or (AObject is TAnimation) or AObject.Equals(FContent) or
        AObject.Equals(FNoItemsContent) or AObject.Equals(ResourceLink) then
      inherited DoAddObject(AObject)
    else begin
      if HasActiveTab then
        ActiveTab.AddObject(AObject)
      else
        FNoItemsContent.AddObject(AObject);

      AddToTabList(AObject);//FIX
    end;
end;

procedure TTabControl.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTabControl.ContentAddObject(const AObject: TFmxObject);
begin
  if not (AObject is TTabItem) then
     FNoItemsContent.AddObject(AObject)
  else
    if FUpdating = 0 then
      Realign;
end;

procedure TTabControl.ContentRemoveObject(const AObject: TFmxObject);
begin
  if (FUpdating = 0) and (AObject is TTabItem) then
    Realign;
end;

procedure TTabControl.AnimationFinished(Sender: TObject);
begin
  WebBrowserReallign(Sender);
  TAnimation(Sender).Release;
end;

procedure TTabControl.ApplyStyle;
var
  B: TFmxObject;
begin
  inherited ApplyStyle;
  B := FindStyleResource('background');
  if Assigned(B) and (B is TControl) then
    FBackground := TControl(B);
  B := FindStyleResource('FullSize');
  if Assigned(B) then
    FFullSize := True;
  B := FindStyleResource('TabHeight');
  if Assigned(B) then
  begin
    FFixedTabHeight := False;
    TabHeight := B.TagFloat;
    FFixedTabHeight := True;
  end;
  ApplyTabsStyleLookup;
  RealignTabs;
end;

procedure TTabControl.FreeStyle;
begin
  inherited FreeStyle;
  FBackground := nil;
  FFixedTabHeight := False;
end;

procedure TTabControl.PaintChildren;
var
  I: Integer;
  Tab: TTabItem;
begin
  if (FTabPosition = TTabPosition.tpDots) then
  begin
    try
      inherited PaintChildren;
    finally
    for I := 0 to TabCount - 1 do
    begin
        Tabs[I].FDesignSelectionMarks := True;
        Tabs[I].FDisablePaint := False;
        Tabs[I].PaintInternal;
      Tabs[I].FDesignSelectionMarks := False;
      end;
    end;
  end
  else if ((FTabPosition = TTabPosition.tpNone) and (csDesigning in ComponentState)) then
  begin
    for I := 0 to TabCount - 1 do
    begin
      Tabs[I].FDesignSelectionMarks := False;
      if Assigned(Tabs[I].ResourceControl) then
        TOpenControl(Tabs[I].ResourceControl).FDisablePaint := True;
    end;
    try
      inherited PaintChildren;
      if HasActiveTab then
        ActiveTab.PaintInternal;
    finally
      for I := 0 to TabCount - 1 do
      begin
        Tabs[I].FDesignSelectionMarks := True;
        if Assigned(Tabs[I].ResourceControl) then
          TOpenControl(Tabs[I].ResourceControl).FDisablePaint := False;
      end;
    end;
    Canvas.SetMatrix(AbsoluteMatrix);
    for I := 0 to TabCount - 1 do
      if Tabs[I].Visible then
      begin
        Canvas.Fill.Kind := TBrushKind.bkSolid;
        if I = TabIndex then
          Canvas.Fill.Color := $FF1072C5
        else
          Canvas.Fill.Color := $FF303030;
        Canvas.FillEllipse(Tabs[I].ParentedRect, 0.3);
      end;
  end
  else if FTransitionRunning then
  begin
    inherited PaintChildren;
    for Tab in FTransitionTabs do
      Tab.PaintInternal;
  end
  else if HasActiveTab and ActiveTab.Visible then
  begin
    ActiveTab.FDisablePaint := True;
    TOpenControl(FNoItemsContent).FDisablePaint := True;
    try
      inherited PaintChildren;
    finally
      ActiveTab.FDisablePaint := False;
      TOpenControl(FNoItemsContent).FDisablePaint := False;
    end;
    ActiveTab.PaintInternal;
    TOpenControl(FNoItemsContent).PaintInternal;
  end
  else
    inherited PaintChildren;
end;

procedure TTabControl.RealignTabs;
const
  DesignTabWidth = 10;
  DesignTabHeight = 10;
var
  CountVisibleTab, I: Integer;
  CurX, CurY: Single;
  L, R, AutoWidth, MaxHeight: Single;
  ItemRect: TRectF;
  WidthErr: Single;
  LItem: TTabItem;
begin
  { calc max height }
  if TabCount > 0 then
  begin
    { Calculate count of visible tabs and Max Height of tabs }
    MaxHeight := 0;
    CountVisibleTab := 0;
    for i := 0 to TabCount - 1 do
    begin
      LItem := Tabs[i];
      if not LItem.Visible then
        Continue;
      LItem.CalcSize;
      if LItem.GetAutoHeight + LItem.Margins.Top + LItem.Margins.Bottom > MaxHeight then
        MaxHeight := Trunc(LItem.GetAutoHeight + LItem.Margins.Top + LItem.Margins.Bottom);
      Inc(CountVisibleTab);
    end;
    if MaxHeight < 5 then
      MaxHeight := 5;
    if CountVisibleTab = 0 then
      MaxHeight := 0
    else if FTabHeight > 0 then
      MaxHeight := FTabHeight;
    { align }
    AutoWidth := Width / AbsoluteScale.X;
    if Assigned(FBackground) then
      AutoWidth := Width - FBackground.Padding.Left - FBackground.Padding.Right;
    if FFullSize and (CountVisibleTab > 0) then
      AutoWidth := AutoWidth / CountVisibleTab;

    case FTabPosition of
      TTabPosition.tpTop: FContent.SetBounds(0, 0, Width, MaxHeight);
      TTabPosition.tpBottom: FContent.SetBounds(0, Height - MaxHeight, Width, MaxHeight);
      TTabPosition.tpNone: FContent.SetBounds(0, 0, 0, 0);
      TTabPosition.tpDots: FContent.SetBounds(0, 0, 0, 0);
    end;
    FNoItemsContent.SetBounds(FContent.Position.X, FContent.Position.Y, FContent.Width, FContent.Height);
    FNoItemsContent.Visible := not (FTabPosition in [TTabPosition.tpNone, TTabPosition.tpDots]);

    if ((FTabPosition = TTabPosition.tpNone) and (csDesigning in ComponentState)) or
        (FTabPosition = TTabPosition.tpDots) then
    begin
      CurX := (Width - (TabCount * DesignTabWidth)) / 2;
      CurY := Height - DesignTabHeight * 2;
    end
    else
    begin
      CurX := 0;
      CurY := 0;
    end;

    WidthErr := 0;
    for I := 0 to TabCount - 1 do
    begin
      LItem := Tabs[I];

      UpdateItemContentBounds(LItem);

      if not LItem.Visible then
        Continue;

      if FTabPosition = TTabPosition.tpNone then
      begin
        if csDesigning in ComponentState then
        begin
          LItem.SetBounds(CurX + 1, CurY + 1, DesignTabWidth - 2, DesignTabHeight - 2);
          CurX := CurX + DesignTabWidth;
        end
        else
          LItem.Position.Y := $FFFF;
        Continue;
      end;
      if FTabPosition = TTabPosition.tpDots then
      begin
        LItem.SetBounds(CurX + 1, CurY + 1, DesignTabWidth - 2, DesignTabHeight - 2);
        CurX := CurX + DesignTabWidth;
        Continue;
      end;

      if I = 0 then
        L := LItem.GetLeftOffset
      else
        L := 0;
      if I = Self.FContent.Controls.Count - 1 then
        R := LItem.GetRightOffset
      else
        R := 0;

      ItemRect := TRectF.Create(TPointF.Create(CurX + LItem.Margins.Left - L, CurY + LItem.Margins.Top),
        LItem.GetAutoWidth, MaxHeight - LItem.Margins.Top - LItem.Margins.Bottom);

      if FFullSize then
      begin
        if CurX + L + AutoWidth - R > Width then
          ItemRect.Width := Width - (CurX + LItem.Margins.Left + LItem.Margins.Right + L - R)
        else
          ItemRect.Width := AutoWidth + R - LItem.Margins.Left - LItem.Margins.Right;
      end;

      WidthErr := WidthErr + (ItemRect.Width - Round(ItemRect.Width));
      ItemRect.Width := Round(ItemRect.Width);
      if WidthErr >= 0.5 then
      begin
        WidthErr := WidthErr - 1.0;
        ItemRect.Width := ItemRect.Width + 1.0;
      end;

      LItem.SetBounds(ItemRect.Left, ItemRect.Top, ItemRect.Width, ItemRect.Height);
      CurX := CurX - L + LItem.Margins.Left + LItem.Width + LItem.Margins.Right + R;
      if not FFullSize and (CurX > Width) then
        LItem.Position.Y := $FFFF;
    end;
  end
  else
    FContent.Height := 0;
end;

procedure TTabControl.DoRealign;
var
  I: Integer;
  LItem: TTabItem;
  B: TControl;
begin
  if FTransitionRunning then Exit;
  if FDisableAlign then Exit;
  FDisableAlign := True;
  try
    RealignTabs;
    if FContent.ControlsCount > 0 then
      for I := 0 to TabCount - 1 do
      begin
        LItem := Tabs[I];
        if not LItem.Visible then
          Continue;
        LItem.FContent.Visible := LItem.Index = TabIndex;
        LItem.FContent.DesignVisible := LItem.Index = TabIndex;
        if LItem.FContent.Visible then
        begin
          UpdateItemContentBounds(LItem);
          LItem.FContent.BringToFront;
        end;
      end;
    if Assigned(ResourceControl) then
    begin
      B := ResourceControl;
      case FTabPosition of
        TTabPosition.tpTop:
          B.SetBounds(B.Margins.Left, FContent.Height + B.Margins.Top, Width - B.Margins.Left - B.Margins.Top,
            Height - FContent.Height - B.Margins.Top - B.Margins.Bottom);
        TTabPosition.tpBottom:
          B.SetBounds(B.Margins.Left, B.Margins.Top, Width - B.Margins.Left - B.Margins.Top - B.Margins.Bottom,
            Height - FContent.Height - B.Margins.Top - B.Margins.Bottom);
        TTabPosition.tpNone:
          B.SetBounds(B.Margins.Left, B.Margins.Top, Width - B.Margins.Left - B.Margins.Top,
            Height - B.Margins.Top - B.Margins.Bottom);
        TTabPosition.tpDots:
          B.SetBounds(B.Margins.Left, B.Margins.Top,
            Width - B.Margins.Left - B.Margins.Top - B.Margins.Bottom,
            Height - FContent.Height - B.Margins.Top - B.Margins.Bottom);
      end;
      B.BringToFront;
    end;
  finally
    FDisableAlign := False;
  end;
end;

function TTabControl.GetTabCount: Integer;
begin
  Result := FContent.ControlsCount;
end;

function TTabControl.GetTabItem(AIndex: Integer): TTabItem;
begin
  if InRange(AIndex, 0, FContent.ControlsCount - 1) then
    Result := FContent.Controls[AIndex] as TTabItem
  else
    Result := nil;
end;

function TTabControl.HasActiveTab: Boolean;
begin
  Result := Assigned(ActiveTab);
end;

procedure TTabControl.UpdateItemContentBounds(LItem: TTabItem);
var
  ContentHeight: Single;
  P: TPointF;
  ContentWidth: Single;
begin
  case FTabPosition of
    TTabPosition.tpTop:
      begin
        P.X := Self.Padding.Left + LItem.FContent.Margins.Left;
        P.Y := Self.FContent.Height + Self.Padding.Top + LItem.FContent.Margins.Top;
      end;
    TTabPosition.tpBottom:
      begin
        P.X := Self.Padding.Left + LItem.FContent.Margins.Left;
        P.Y := Self.Padding.Top + LItem.FContent.Margins.Top;
      end;
    TTabPosition.tpNone:
      begin
        P.X := Self.Padding.Left + LItem.FContent.Margins.Left;
        P.Y := Self.Padding.Top + LItem.FContent.Margins.Top;
      end;
    TTabPosition.tpDots:
      begin
        P.X := Self.Padding.Left + LItem.FContent.Margins.Left;
        P.Y := Self.Padding.Top + LItem.FContent.Margins.Top;
      end;
  end;
  P := LItem.AbsoluteToLocal(Self.LocalToAbsolute(P));
  ContentWidth := Self.Width - Self.Padding.Left - Self.Padding.Right - LItem.FContent.Margins.Left - LItem.FContent.Margins.Right;
  ContentHeight := Self.Height - FContent.Height - Self.Padding.Top - Self.Padding.Bottom - LItem.FContent.Margins.Top - LItem.FContent.Margins.Bottom;
  LItem.FContent.SetBounds(P.X, P.Y, ContentWidth, ContentHeight);
end;

procedure TTabControl.WebBrowserReallign(Sender: TObject);
var
  BrowserManager : IFMXWBService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService, IInterface(BrowserManager)) then
    BrowserManager.RealignBrowsers;
end;

procedure TTabControl.SetTabIndex(const Value: Integer);

  procedure DeselectActiveTab;
  begin
    if HasActiveTab then
      ActiveTab.IsSelected := False;
  end;

  procedure SelectActiveTab;
  begin
    if HasActiveTab then
      ActiveTab.IsSelected := True;
  end;

var
  PickerService: IFMXPickerService;
begin
  if FTabIndex <> Value then
  begin
    DeselectActiveTab;
    FTabIndex := Value;
    SelectActiveTab;
    Realign;
    DoChange;
    // When user change active tab (iOS), we need close all active pickers
    if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, IInterface(PickerService)) then
      PickerService.CloseAllPickers;
    WebBrowserReallign(nil);
  end;
end;

procedure TTabControl.SetActiveTab(const Value: TTabItem);
begin
  if Assigned(Value) and IsChild(Value) then
    TabIndex := Value.Index
  else
    TabIndex := -1;
end;

procedure TTabControl.SetActiveTabWithTransition(const ATab: TTabItem; ATransition: TTabTransition;
      const ADirection: TTabTransitionDirection = TTabTransitionDirection.tdNormal);

  procedure ForceStyleLookupForChildren(const AControl: TControl);
  var
    I: Integer;
  begin
    if AControl is TStyledControl then
    begin
      // To avoide free style during animation
      TOpenStyledControl(AControl).DisableDisappear := True;
      TStyledControl(AControl).ApplyStyleLookup;
    end;
    if not Supports(AControl, IItemsContainer) then
      for I := 0 to AControl.ControlsCount - 1 do
        ForceStyleLookupForChildren(AControl.Controls[I]);
  end;

  procedure LocalAnimateInt(AParent : TFmxObject; const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
    AType: TAnimationType = TAnimationType.atIn;
    AInterpolation: TInterpolationType = TInterpolationType.itLinear);
  var
    A: TIntAnimation;
  begin
    StopPropertyAnimation(APropertyName);
    A := TIntAnimation.Create(AParent);
    A.Parent := AParent;
    A.AnimationType := AType;
    A.Interpolation := AInterpolation;
    A.OnFinish := AnimationFinished;
    A.OnProcess := WebBrowserReallign;
    A.Duration := Duration;
    A.PropertyName := APropertyName;
    A.StartFromCurrent := True;
    A.StopValue := NewValue;
    A.Start;
  end;

  procedure LocalAnimateIntWait(AParent : TFmxObject; const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
    AType: TAnimationType = TAnimationType.atIn;
    AInterpolation: TInterpolationType = TInterpolationType.itLinear);
  var
    A: TIntAnimation;
  begin
    StopPropertyAnimation(APropertyName);
    A := TIntAnimation.Create(AParent);
    try
      A.Parent := AParent;
      A.AnimationType := AType;
      A.Interpolation := AInterpolation;
      A.Duration := Duration;
      A.PropertyName := APropertyName;
      A.StartFromCurrent := True;
      A.StopValue := NewValue;
      A.Start;
      while A.Running do
      begin
        Application.ProcessMessages;
        Sleep(0);
      end;
    finally
      A.DisposeOf;
    end;
  end;

const
  Duration = 0.3;
var
  Tab1, Tab2: TTabItem;
  Layout1, Layout2: TControl;
  LayoutRect: TRectF;
  P, LayoutPos: TPointF;
  SaveDisableGlyphPopulation: Boolean;
begin
  case ATransition of
    TTabTransition.ttSlide:
      begin
        FTransitionRunning := True;
        ForceStyleLookupForChildren(ATab);
        ClipChildren := True;
        SaveDisableGlyphPopulation := TTextLayoutNG.DisableGlyphPopulation;
        if TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle then
          TTextLayoutNG.DisableGlyphPopulation := True;
        try
          LayoutPos := ActiveTab.FContent.LocalToAbsolute(PointF(0, 0));
          LayoutRect := ActiveTab.FContent.BoundsRect;

          Tab1 := ActiveTab;
          Layout1 := ActiveTab.FContent;
          ActiveTab := ATab;
          Tab2 := ActiveTab;
          Layout2 := ActiveTab.FContent;

          SetLength(FTransitionTabs, 2);
          FTransitionTabs[0] := Tab1;
          FTransitionTabs[1] := Tab2;

          Layout1.Visible := True;
          Layout2.Visible := True;
          if ADirection = TTabTransitionDirection.tdNormal then
          begin
            P := Tab1.AbsoluteToLocal(LayoutPos);
            Layout1.SetBounds(P.X, P.Y, LayoutRect.Width, LayoutRect.Height);
            LocalAnimateInt(Layout1, 'Position.X', Round(P.X - LayoutRect.Width), Duration, TAnimationType.atIn, TInterpolationType.itLinear);
            P := Tab2.AbsoluteToLocal(LayoutPos);
            Layout2.SetBounds(P.X + LayoutRect.Width, P.Y, LayoutRect.Width, LayoutRect.Height);
            LocalAnimateIntWait(Layout2, 'Position.X', Round(P.X), Duration, TAnimationType.atIn, TInterpolationType.itLinear);
          end
          else
          begin
            P := Tab1.AbsoluteToLocal(LayoutPos);
            Layout1.SetBounds(P.X, P.Y, LayoutRect.Width, LayoutRect.Height);
            LocalAnimateInt(Layout1, 'Position.X', Round(P.X + LayoutRect.Width), Duration, TAnimationType.atIn, TInterpolationType.itLinear);
            P := Tab2.AbsoluteToLocal(LayoutPos);
            Layout2.SetBounds(P.X - LayoutRect.Width, P.Y, LayoutRect.Width, LayoutRect.Height);
            LocalAnimateIntWait(Layout2, 'Position.X', Round(P.X), Duration, TAnimationType.atIn, TInterpolationType.itLinear);
          end;
        finally
          if TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle then
            TTextLayoutNG.DisableGlyphPopulation := SaveDisableGlyphPopulation;
          SetLength(FTransitionTabs, 0);
          ClipChildren := False;
          FTransitionRunning := False;
          Realign;
        end;
        // Force repaint
        Application.ProcessMessages;
      end
  else
    ActiveTab := ATab;
  end;
end;

procedure TTabControl.SetTabHeight(const Value: Single);
var
  I: Integer;
begin
  if (FTabHeight <> Value) and not FFixedTabHeight then
  begin
    FTabHeight := Value;
    for I := 0 to TabCount - 1 do
      Tabs[I].Height := Value;
    Realign;
  end;
end;

procedure TTabControl.SetFullSize(const Value: Boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    Realign;
  end;
end;

procedure TTabControl.SetTabPosition(const Value: TTabPosition);
var
  I: Integer;
begin
  if FTabPosition <> Value then
  begin
    if FTabPosition = TTabPosition.tpDots then
    begin
      for I := 0 to TabCount - 1 do
        Tabs[I].StyleLookup := '';
    end;
    FTabPosition := Value;
    if FTabPosition = TTabPosition.tpDots then
    begin
      for I := 0 to TabCount - 1 do
        Tabs[I].StyleLookup := 'tabdotstyle';
    end;
    if not (csLoading in ComponentState) then
      ApplyTabsStyleLookup;
    Realign;
  end;
end;

procedure TTabControl.ApplyTabsStyleLookup;
var
  I: Integer;
begin
  for I := 0 to TabCount - 1 do
    Tabs[I].ApplyStyleLookup;
end;

initialization
  RegisterFmxClasses([TTabControl, TTabItem]);
end.

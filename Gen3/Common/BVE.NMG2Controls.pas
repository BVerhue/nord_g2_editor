unit BVE.NMG2Controls;

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

// Set of controls derived from the basis GPU controls

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
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Materials,
  BVE.NMG2GraphTypes,
  BVE.NMG2ControlsGL,
  BVE.NMG2TexturesGL;

type
  TBtnKind = (bkToggle, bkMomentary);

  TBtnTexGL = class(TTexControlGL)
  private
    FImageIDs: string;
    FTexts: string;
    Fsl: TStringList;
    FBtnHeight: single;
    FBtnWidth: single;
  protected
    procedure SetBtnHeight(const aValue: single);
    procedure SetBtnWidth(const aValue: single);
    procedure SetImageIds(const aValue: string);
    procedure SetTexts(const aValue: string);

    procedure UpdateTextureID; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ImageIDs: string read FImageIDs write SetImageIds;
    property Texts: string read FTexts write SetTexts;
    property BtnWidth: single read FBtnWidth write SetBtnWidth;
    property BtnHeight: single read FBtnHeight write SetBtnHeight;
  end;

  TBtnTexMatrixGL = class(TTexMatrixControlGL)
  private
    FImageIDs: string;
    FTexts: string;
    Fsl: TStringList;
    FBtnHeight: single;
    FBtnWidth: single;
    procedure SetBtnHeight(const aValue: single);
    procedure SetBtnWidth(const aValue: single);
  protected
    procedure SetImageIds(const aValue: string); virtual;
    procedure SetTexts(const aValue: string); virtual;
    procedure UpdateTextureID; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ImageIDs: string read FImageIDs write SetImageIds;
    property Texts: string read FTexts write SetTexts;
    property BtnWidth: single read FBtnWidth write SetBtnWidth;
    property BtnHeight: single read FBtnHeight write SetBtnHeight;
  end;

  TBtnTextGL = class(TBtnTexGL)
  private
    FBtnType: TButtonTextType;
    FValue: integer;
  protected
    procedure SetValue(const aValue: integer); virtual;
    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseLeave; override;

  published
    property BtnType: TButtonTextType read FBtnType write FBtnType;
    property Value: integer read FValue write SetValue;
  end;

  TBtnFlatGL = class(TBtnTexGL)
  public
    FValue: integer;
    FMaxValue: integer;
    FMinValue: integer;
  private
    procedure SetMaxValue(const aValue: integer);
    procedure SetMinValue(const aValue: integer);
  protected
    procedure SetValue(const aValue: integer); virtual;
    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property Value: integer read FValue write SetValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property MinValue: integer read FMinValue write SetMinValue;
  end;

  TBtnIncDecGL = class(TBtnTexMatrixGL)
  private
    FValue: integer;
    FMaxValue: integer;
    FMinValue: integer;
    FHorizontal: boolean;
    FOnChange: TNotifyEvent;

    function GetValue: integer;
    procedure SetMaxValue(const aValue: integer);
    procedure SetMinValue(const aValue: integer);
    procedure SetHorizontal(const Value: boolean);
  protected
    procedure SetValue(const aValue: integer); virtual;

    procedure UpdateCellIndex; override;
    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseLeave; override;

  published
    property Value: integer read GetValue write SetValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property MinValue: integer read FMinValue write SetMinValue;
    property Horizontal: boolean read FHorizontal write SetHorizontal;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBtnRadioGL = class(TBtnTexMatrixGL)
  private
    FValue: integer;
    FMaxValue: integer;
    FHorizontal: boolean;
    FUpsideDown: boolean;
    procedure SetUpsideDown(const aValue: boolean);
    procedure SetHorizontal(const Value: boolean);
  protected
    procedure SetValue(const aValue: integer); virtual;
    procedure SetMaxValue(const aValue: integer); virtual;

    procedure UpdateCellIndex; override;
    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property Horizontal: boolean read FHorizontal write SetHorizontal;
    property UpsideDown: boolean read FUpsideDown write SetUpsideDown;
    property Value: integer read FValue write SetValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
  end;

  {TBtnRadioEdit2GL = class(TBtnRadioGL)
  private
    FLabelList: TObjectDictionary<string, TLabelStaticGL>;
  protected
    procedure SetTexts(const aValue: string); override;

    procedure RenderLabels;
    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

    procedure RenderControl(aContext: TContext3D); override;
  end;}

  TBtnRadioEditGL = class(TBtnRadioGL)
  private
    FLabelList: TObjectList<TMatrixTextObject>;
    procedure CreateLabels;
  protected
    procedure SetMaxValue(const aValue: integer); override;
    procedure SetColCount(const Value: integer); override;
    procedure SetRowCount(const Value: integer); override;
    procedure SetTexts(const aValue: string); override;
    procedure SetTextureList(const aValue: TTextureListGL); override;

    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

    procedure RenderControl(aContext: TContext3D); override;
  end;

  TKeyBlackGL = class(TTexControlGL)
  private
    FValue: integer;
    FOnChange: TNotifyEvent;
  protected
    procedure SetValue(const aValue: integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseLeave; override;

    property Value: integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TKeyboardGL = class(TTexMatrixControlGL)
  private
    FValue: integer;
    FKeysBlack: array[0..4] of TKeyBlackGL;
    FOnChange: TNotifyEvent;
    procedure KeyBlackChange(Sender: TObject);
  protected
    procedure SetValue(const aValue: integer); virtual;
    procedure SetTextureList(const aValue: TTextureListGL); override;
    procedure UpdateTextureID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  published
    property Value: integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TKnobGL = class(TTexControlGL)
  private
    FKnobType: TKnobType;
    FKnob: TTexObject;

    FMorphMesh : TArcMeshGL;
    FMorphMat : TColorMaterial;

    FRDial,
    FRKnob,
    FRBtns,
    FRCntr: TRectF;

    FMinValue,
    FMaxValue,
    FValue: single;
    FMouseDownValue: single;
    FMouseDownPoint: TPointF;

    FCenterBtn: TTexControlGL;
    FIncDecBtns: TBtnIncDecGL;
    FShowBtns: boolean;

    FKnobTextureID: string;
    FMorphValue: single;
    FBtnsTextureID: string;

    FOnChange: TNotifyEvent;

    procedure SetMaxValue(const aValue: single);
    procedure SetMinValue(const aValue: single);
    procedure SetKnobTextureID(const aValue: string);

    procedure BtnsClick(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure CntrBtnClick(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure SetShowBtns(const aValue: boolean);
    procedure SetKnobType(const aValue: TKnobType);
    procedure SetMorphValue(const aValue: single);
    procedure SetBtnsTextureID(const Value: string);

    procedure KnobTexCreate(Sender: TObject; var aTiledTexture: TTiledTexture);
  protected
    procedure SetTextureList(const aValue: TTextureListGL); override;
    procedure SetValue(const aValue: single); virtual;

    //procedure CreateVertices; override;
    procedure CreateMorphVertices;
    //procedure CreateKnobVertices;

    procedure UpdateTextureID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;
    function CreateKnobTexture: TTiledTexture;
    function CreateBtnsTexture: TTiledTexture;

    //function GetKnobTexture: TTiledTexture;
    function GetBtnsTexture: TTiledTexture;

    procedure RenderControl(aContext: TContext3D); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property KnobType: TKnobType read FKnobType write SetKnobType;
    property KnobTextureID: string read FKnobTextureID write SetKnobTextureID;
    property BtnsTextureID: string read FBtnsTextureID write SetBtnsTextureID;
    property MinValue: single read FMinValue write SetMinValue;
    property MaxValue: single read FMaxValue write SetMaxValue;
    property Value: single read FValue write SetValue;
    property MorphValue: single read FMorphValue write SetMorphValue;
    property CenterBtn: TTexControlGL read FCenterBtn;
    property IncDecBtns: TBtnIncDecGL read FIncDecBtns;
    property ShowBtns: boolean read FShowBtns write SetShowBtns;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TLabelGL = class(TTexControlGL)
  private
    FText: string;
    FFont: TFont;
    FColor: TAlphaColor;
    FTextAlign: TTextAlign;
    FTexture: TTiledTexture;
    procedure SetFont(const Value: TFont);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetColor(const Value: TAlphaColor);
  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string);

    //procedure UpdateTexture; override;
    procedure UpdateTexture;
    procedure UpdateTextureID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;

  published
    property Caption: string read GetCaption write SetCaption;
    property Color: TAlphaColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
  end;

  TTextEdit2GL = class(TBtnTextGL)
  private
    FLabel: TLabelStaticGL;
  protected
    function GetCaption: string;
    procedure SetCaption(const aValue: string);
    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;
    procedure RenderControl(aContext: TContext3D); override;
  published
    property Caption: string read GetCaption write SetCaption;
  end;

  TTextEditGL = class(TBtnTextGL)
  private
    FLabel: TMatrixTextObject;
  protected
    function GetCaption: string;

    procedure SetCaption(const aValue: string);
    procedure SetTextureList(const aValue: TTextureListGL); override;

    procedure UpdateTextureID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;
    procedure RenderControl(aContext: TContext3D); override;
  published
    property Caption: string read GetCaption write SetCaption;
  end;

  TTextFieldGL = class(TTexControlGL)
  private
    FLabel: TMatrixTextObject;
    FFieldHeight: single;
    FFieldWidth: single;
    procedure SetFieldHeight(const Value: single);
    procedure SetFieldWidth(const Value: single);
  protected
    function GetCaption: string;

    procedure SetCaption(const aValue: string);
    procedure SetTextureList(const aValue: TTextureListGL); override;

    procedure UpdateTextureID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;
    procedure RenderControl(aContext: TContext3D); override;
  published
    property Caption: string read GetCaption write SetCaption;
    property FieldWidth: single read FFieldWidth write SetFieldWidth;
    property FieldHeight: single read FFieldHeight write SetFieldHeight;
  end;

  TGraphGL = class(TTexControlGL)
  private
    FFieldHeight: single;
    FFieldWidth: single;
    procedure SetFieldHeight(const Value: single);
    procedure SetFieldWidth(const Value: single);
  protected
    procedure UpdateTextureID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateTexture: TTiledTexture; override;
    procedure RenderControl(aContext: TContext3D); override;
  published
    property FieldWidth: single read FFieldWidth write SetFieldWidth;
    property FieldHeight: single read FFieldHeight write SetFieldHeight;
  end;

implementation

const
  WHITE_KEY_NRS : array[0..6] of integer = (0, 2, 4, 5, 7, 9, 11);
  BLACK_KEY_NRS : array[0..4] of integer = (1, 3, 6, 8, 10);

{ TBtnTexGL }

constructor TBtnTexGL.Create(AOwner: TComponent);
begin
  inherited;
  FImageIds := '';
  FTexts := '';
  Fsl := TStringList.Create;
  TexCols := 1;
  TexRows := 2;
end;

destructor TBtnTexGL.Destroy;
begin
  Fsl.Free;
  inherited;
end;

procedure TBtnTexGL.SetBtnHeight(const aValue: single);
begin
  if FBtnHeight <> aValue then
  begin
    FBtnHeight := aValue;
    UpdateTextureID;
  end;
end;

procedure TBtnTexGL.SetBtnWidth(const aValue: single);
begin
  if FBtnWidth <> aValue then
  begin
    FBtnWidth := aValue;
    UpdateTextureID;
  end;
end;

procedure TBtnTexGL.SetImageIds(const aValue: string);
begin
  if FImageIDs <> aValue then
  begin
    FImageIDs := aValue;
    Fsl.Delimiter := ';';
    Fsl.StrictDelimiter := True;
    Fsl.DelimitedText := aValue;
    //if assigned(TextureList) then
    //  TextureID := GetBtnTextTexID(Width, Height, FImageIds, FTexts)
    UpdateTextureID;
  end;
end;

procedure TBtnTexGL.SetTexts(const aValue: string);
begin
  if FTexts <> aValue then
  begin
    FTexts := aValue;
    Fsl.Delimiter := ';';
    Fsl.StrictDelimiter := True;
    Fsl.DelimitedText := aValue;
    //if assigned(TextureList) then
    //  TextureID := GetBtnTextTexID(Width, Height, FImageIds, FTexts)
    UpdateTextureID;
  end;
end;

procedure TBtnTexGL.UpdateTextureID;
begin
  //
end;

{ TBtnTexMatrixGL }

constructor TBtnTexMatrixGL.Create(AOwner: TComponent);
begin
  inherited;

  TexCols := 1;
  TexRows := 2;

  FImageIds := '';
  FTexts := '';
  Fsl := TStringList.Create;
end;

destructor TBtnTexMatrixGL.Destroy;
begin
  Fsl.Free;
  inherited;
end;

procedure TBtnTexMatrixGL.SetBtnHeight(const aValue: single);
begin
  if FBtnHeight <> aValue then
  begin
    FBtnHeight := aValue;
    UpdateTextureID;
  end;
end;

procedure TBtnTexMatrixGL.SetBtnWidth(const aValue: single);
begin
  if FBtnWidth <> aValue then
  begin
    FBtnWidth := aValue;
    UpdateTextureID;
  end;
end;

procedure TBtnTexMatrixGL.SetImageIds(const aValue: string);
begin
  if FImageIDs <> aValue then
  begin
    FImageIDs := aValue;
    Fsl.Delimiter := ';';
    Fsl.StrictDelimiter := True;
    Fsl.DelimitedText := aValue;
    UpdateTextureID;
    UpdateCellIndex;
  end;
end;

procedure TBtnTexMatrixGL.SetTexts(const aValue: string);
begin
  if FTexts <> aValue then
  begin
    FTexts := aValue;
    Fsl.Delimiter := ';';
    Fsl.StrictDelimiter := True;
    Fsl.DelimitedText := aValue;
    UpdateTextureID;
    UpdateCellIndex;
  end;
end;

procedure TBtnTexMatrixGL.UpdateTextureID;
begin
  //
end;

{ TBtnTextGL }

constructor TBtnTextGL.Create(AOwner: TComponent);
begin
  inherited;
  FBtnType := bttCheck;
end;

function TBtnTextGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0) and ((FImageIds <> '') or (FTexts <> '')) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtnText(BtnWidth, BtnHeight, FImageIds, FTexts);
  end;
end;

destructor TBtnTextGL.Destroy;
begin
  inherited;
end;

procedure TBtnTextGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then begin
    case FBtnType of
      bttCheck:
        begin
          Value := 1 - Value;
        end;
      bttPush:
        begin
          Value := 1;
        end;
    end;
  end;

  inherited;
end;

procedure TBtnTextGL.MouseLeave;
begin
  case FBtnType of
    bttCheck:
      begin
      end;
    bttPush:
      begin
        Value := 0;
      end;
  end;
end;

procedure TBtnTextGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  case FBtnType of
    bttCheck:
      begin
      end;
    bttPush:
      begin
        Value := 0;
      end;
  end;

  inherited;
end;

procedure TBtnTextGL.SetValue(const aValue: integer);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    ImageIndex := FValue;
  end;
end;

procedure TBtnTextGL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0)
    and ((FImageIds <> '') or (FTexts <> '')) then
  begin
    TexCols := 1;
    TexRows := 2;
    TextureID := GetBtnTextTexID(BtnWidth, BtnHeight, FImageIds, FTexts)
  end;
end;

{ TBtnFlatGL }

constructor TBtnFlatGL.Create(AOwner: TComponent);
begin
  inherited;

  TexCols := 1;
  TexRows := 1;

  FValue := 0;
  FMaxValue := 0;
  FMinValue := 0;
end;

function TBtnFlatGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0) and ((FImageIds <> '') or (FTexts <> '')) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtnFlat(BtnWidth, BtnHeight, FImageIds, FTexts);
  end;
end;

destructor TBtnFlatGL.Destroy;
begin
  inherited;
end;

procedure TBtnFlatGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then
  begin
    if Value = MaxValue then
      Value := MinValue
    else
     Value := value + 1;
  end;

  inherited;
end;

procedure TBtnFlatGL.SetMaxValue(const aValue: integer);
begin
  if FMaxValue <> aValue then
  begin
    FMaxValue := aValue;
    TexRows := aValue + 1;
    if Value > MaxValue then
      Value := MaxValue;
  end;
end;

procedure TBtnFlatGL.SetMinValue(const aValue: integer);
begin
  if FMinValue <> aValue then
  begin
    FMinValue := aValue;
    if Value < MinValue then
      Value := MinValue;
  end;
end;

procedure TBtnFlatGL.SetValue(const aValue: integer);
begin
  if FValue <> aValue then
  begin
    FValue := aValue;
    ImageIndex := FValue;
  end;
end;

procedure TBtnFlatGL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0)
    and ((FImageIds <> '') or (FTexts <> '')) then
  begin
    TexCols := 1;
    TexRows := Fsl.Count;
    TextureID := GetBtnFlatTexID(BtnWidth, BtnHeight, FImageIds, FTexts)
  end;
end;

{ TBtnRadioGL }

constructor TBtnRadioGL.Create(AOwner: TComponent);
begin
  inherited;
  FMaxValue := 0;
  Value := 0;
end;

function TBtnRadioGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0)
    and ((FImageIds <> '') or (FTexts <> '')) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtnRadio(BtnWidth, BtnHeight, ImageIds, FTexts);
  end;
end;

destructor TBtnRadioGL.Destroy;
begin
  inherited;
end;

procedure TBtnRadioGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  i: integer;
begin
  i := IndexFromPoint(X, Y);
  if (i >= 0) and (i < ColCount * RowCount) then
    Value := i;

  inherited;
end;

procedure TBtnRadioGL.SetHorizontal(const Value: boolean);
begin
  FHorizontal := Value;
end;

procedure TBtnRadioGL.SetMaxValue(const aValue: integer);
begin
  if FMaxValue <> aValue then
  begin
    FMaxValue := aValue;
    TexCols := aValue + 1;
    if FHorizontal then
    begin
      ColCount := aValue + 1;
      RowCount := 1;
    end else begin
      ColCount := 1;
      RowCount := aValue + 1;
    end;
  end;
end;

procedure TBtnRadioGL.SetUpsideDown(const aValue: boolean);
begin
  FUpsideDown := aValue;
end;

procedure TBtnRadioGL.SetValue(const aValue: integer);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    UpdateCellIndex;
  end;
end;

procedure TBtnRadioGL.UpdateCellIndex;
var
  i: integer;
begin
  for i := 0 to ColCount * RowCount - 1 do
    if i = FValue then
      CellIndex[i] := 1
    else
      CellIndex[i] := 0;
end;

procedure TBtnRadioGL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0)
    and ((FImageIds <> '') or (FTexts <> '')) then
  begin
    TexCols := Fsl.Count;
    TexRows := 2;
    ColCount := TexCols;
    RowCount := 1;
    TextureID := GetBtnRadioTexID(BtnWidth, BtnHeight, FImageIds, FTexts)
  end;
end;

{ TBtnRadioEdit2GL }

{constructor TBtnRadioEdit2GL.Create(AOwner: TComponent);
begin
  inherited;
  FLabelList := TObjectDictionary<string, TLabelStaticGL>.Create([doOwnsValues]);
  TexCols := 1;
  TexRows := 2;
end;

function TBtnRadioEdit2GL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtnRadioEdit(BtnWidth, BtnHeight);
  end;
end;

destructor TBtnRadioEdit2GL.Destroy;
begin
  FLabelList.Free;
  inherited;
end;

procedure TBtnRadioEdit2GL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  r, c: integer;
  dx, dy: single;
  LabelGL: TLabelStaticGL;
  Text: string;
  Texture: TTiledTexture;
begin
  inherited;

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    SaveMatrix := aContext.CurrentMatrix;
    try
      for r := 0 to RowCount - 1 do begin
        for c := 0 to ColCount - 1 do begin

          if r * ColCount + c < Fsl.Count then
            Text := Fsl[r * ColCount + c]
          else
            Text := '';

          if FLabelList.TryGetValue(Text, LabelGL) then
          begin
            LabelGL.Width := TileWidth;
            LabelGL.Height := TileHeight;

            dx := (TileWidth - LabelGL.Width) / 2;
            dy := (TileHeight - LabelGL.Height) / 2;

            aContext.SetMatrix(
                TMatrix3D.CreateTranslation(
                  Point3D(
                    (TileWidth + Padding) * c + Padding + dx,
                    (TileHeight + Padding) * r + Padding + dy,
                    0)) * SaveMatrix);

            LabelGL.Render(aContext);
          end;
        end;
      end;

    finally
      aContext.SetMatrix(SaveMatrix);
    end;
  end;
end;

procedure TBtnRadioEdit2GL.RenderLabels;
var
  r, c: integer;
  LabelGL: TLabelStaticGL;
  Text: string;
  Texture: TTiledTexture;
begin
  FLabelList.Clear;

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    for r := 0 to RowCount - 1 do begin
      for c := 0 to ColCount - 1 do begin

        if r * ColCount + c < Fsl.Count then
          Text := Fsl[r * ColCount + c]
        else
          Text := '';

        if not FLabelList.ContainsKey(Text) then
        begin

          LabelGL := TLabelStaticGL.Create(
            FONT_FAMILY,
            SCALE_Y * 6,
            TAlphaColorRec.Black,
            TTextAlign.Center,
            Text,
            TileWidth,
            TileHeight);

          FLabelList.Add(Text, LabelGL);
        end;
      end;
    end;
  end;
end;

procedure TBtnRadioEdit2GL.SetTexts(const aValue: string);
begin
  if FTexts <> aValue then
  begin
    FTexts := aValue;

    Fsl.Delimiter := ';';
    Fsl.StrictDelimiter := True;
    Fsl.DelimitedText := aValue;

    //if not assigned(Texture) then
    //  UpdateTexture;
    RenderLabels;
  end;
end;

procedure TBtnRadioEdit2GL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    TextureID := GetBtnRadioEditTexID(BtnWidth, BtnHeight);
    RenderLabels;
  end;
end;}

{ TBtnRadioEditGL }

constructor TBtnRadioEditGL.Create(AOwner: TComponent);
begin
  inherited;
  FLabelList := TObjectList<TMatrixTextObject>.Create(True);
  TexCols := 1;
  TexRows := 2;
end;

procedure TBtnRadioEditGL.CreateLabels;
var
  r, c, i: integer;
  MatrixText: TMatrixTextObject;
  Texture: TTiledTexture;
begin
  Texture := GetTexture;

  FLabelList.Clear;
  for r := 0 to RowCount - 1 do begin
    for c := 0 to ColCount - 1 do begin

      i := r * ColCount + c;

      MatrixText := TMatrixTextObject.Create;
      FLabelList.Add(MatrixText);

      MatrixText.TextureList := TextureList;
      if assigned(Texture) then
      begin
        MatrixText.Width := TileWidth;
        MatrixText.Height := TileHeight;
      end;
      if i < Fsl.Count then
        MatrixText.Text := Fsl[i]
      else
        MatrixText.Text := '';
    end;
  end;
end;

function TBtnRadioEditGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtn(BtnWidth, BtnHeight);
  end;
end;

destructor TBtnRadioEditGL.Destroy;
begin
  FLabelList.Free;
  inherited;
end;

procedure TBtnRadioEditGL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  r, c, i: integer;
  dx, dy: single;
  MatrixText: TMatrixTextObject;
  ClientWidth, ClientHeight, Zoom, Margin: single;
  Texture: TTiledTexture;
begin
  inherited;

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    Zoom := 0.8;
    Margin := 2 * SCALE_X;
    ClientWidth := TileWidth - Margin * 2;
    ClientHeight := TileHeight - Margin * 2;

    SaveMatrix := aContext.CurrentMatrix;
    try
      for r := 0 to RowCount - 1 do begin
        for c := 0 to ColCount - 1 do begin

          i := r * ColCount + c;

          MatrixText := FLabelList[i];

          if (MatrixText.TextWidth * Zoom) > ClientWidth then
            MatrixText.Width := ClientWidth
          else
            MatrixText.Width := MatrixText.TextWidth * Zoom;

          if (MatrixText.TextHeight * Zoom) > ClientHeight then
            MatrixText.Height := ClientHeight
          else
            MatrixText.Height := MatrixText.TextHeight * Zoom;

          dx := (TileWidth - MatrixText.Width) / 2;
          dy := (TileHeight - MatrixText.Height) / 2;

          aContext.SetMatrix(
              TMatrix3D.CreateTranslation(
                Point3D(
                  (TileWidth + Padding) * c + Padding + dx,
                  (TileHeight + Padding) * r + Padding + dy,
                  0)) * SaveMatrix);

          MatrixText.Render(aContext);
        end;
      end;

    finally
      aContext.SetMatrix(SaveMatrix);
    end;
  end;
end;

procedure TBtnRadioEditGL.SetColCount(const Value: integer);
begin
  if Value <> ColCount then
  begin
    inherited;
    CreateLabels;
  end;
end;

procedure TBtnRadioEditGL.SetMaxValue(const aValue: integer);
begin
  if FMaxValue <> aValue then
  begin
    FMaxValue := aValue;
  end;
end;

procedure TBtnRadioEditGL.SetRowCount(const Value: integer);
begin
  if Value <> RowCount then
  begin
    inherited;
    CreateLabels;
  end;
end;

procedure TBtnRadioEditGL.SetTexts(const aValue: string);
var
  r, c, i: integer;
begin
  if FTexts <> aValue then
  begin
    FTexts := aValue;

    Fsl.Delimiter := ';';
    Fsl.StrictDelimiter := True;
    Fsl.DelimitedText := aValue;

    if FLabelList.Count <> ColCount * RowCount then
      CreateLabels;

    for r := 0 to RowCount - 1 do begin
      for c := 0 to ColCount - 1 do begin
        i := (r * ColCount + c);
        if  i < Fsl.Count then
          FLabelList[r * ColCount + c].Text := Fsl[i];
      end;
    end;

    UpdateCellIndex;
    Invalidate;
  end;
end;

procedure TBtnRadioEditGL.SetTextureList(const aValue: TTextureListGL);
var
  r, c, i: integer;
begin
  inherited;

  for r := 0 to RowCount - 1 do begin
    for c := 0 to ColCount - 1 do begin
      i := (r * ColCount + c);
      FLabelList[i].TextureList := aValue;
    end;
  end;
end;

procedure TBtnRadioEditGL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    TextureID := GetBtnTexID(BtnWidth, BtnHeight);
  end;
end;

{ TBtnIncDecGL }

constructor TBtnIncDecGL.Create(AOwner: TComponent);
begin
  inherited;
  TexCols := 2;
  TexRows := 2;
  FValue := 0;
  FMaxValue := 0;
  FMinValue := 0;
end;

function TBtnIncDecGL.CreateTexture: TTiledTexture;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtnIncDec(BtnWidth, BtnHeight, FHorizontal);
  end else
    Result := nil;
end;

destructor TBtnIncDecGL.Destroy;
begin
  inherited;
end;

function TBtnIncDecGL.GetValue: integer;
begin
  Result := FValue;
end;

procedure TBtnIncDecGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  i, Idx: integer;
begin
  if ssLeft in Shift then begin
    Idx := IndexFromPoint(X, Y);
    for i := 0 to ColCount * RowCount - 1 do
      if i = Idx then
        CellIndex[i] := 1
      else
        CellIndex[i] := 0;
  end;

  inherited;
end;

procedure TBtnIncDecGL.MouseLeave;
var
  i : integer;
begin
  for i := 0 to ColCount * RowCount - 1 do
    CellIndex[i] := 0;
end;

procedure TBtnIncDecGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  i : integer;
begin
  for i := 0 to ColCount * RowCount - 1 do
    if CellIndex[i] <> 0 then
    begin
      if CellIndex[0] = 1 then
      begin
        if Value > MinValue then
          Value := Value - 1;
      end;

      if CellIndex[1] = 1 then
      begin
        if Value < MaxValue then
          Value := Value + 1;
      end;

      CellIndex[i] := 0;
    end;

  inherited;
end;

procedure TBtnIncDecGL.SetHorizontal(const Value: boolean);
begin
  FHorizontal := Value;
end;

procedure TBtnIncDecGL.SetMaxValue(const aValue: integer);
begin
  if FMaxValue <> aValue then
  begin
    FMaxValue := aValue;
    if FValue > FMaxValue then
      Value := FMaxValue;
  end;
end;

procedure TBtnIncDecGL.SetMinValue(const aValue: integer);
begin
  if FMinValue <> aValue then
  begin
    FMinValue := aValue;
    if FValue < FMinValue then
      Value := FMinValue;
  end;
end;

procedure TBtnIncDecGL.SetValue(const aValue: integer);
begin
  if FValue <> aValue then
  begin
    FValue := aValue;
    UpdateCellIndex;

    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

procedure TBtnIncDecGL.UpdateCellIndex;
begin
end;

procedure TBtnIncDecGL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    TexCols := 2;
    TexRows := 2;
    TextureID := GetBtnIncDecTexID(BtnWidth, BtnHeight, FHorizontal)
  end;
end;

{ TKnobGL }

// Knob consists of up to 5 elements
// - Knob inidicator (stationary)
// - Knob
// - Morph
// - Knob finetuning buttons
// - Optional center button

constructor TKnobGL.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;

  FKnobType := ktNone;

  FKnob := TTexObject.Create;
  FKnob.OnTexCreate := KnobTexCreate;

  FMorphMesh := TArcMeshGL.Create(
    PointF(
      Width/2, Height/2),
      PointF(Width/2, Height/2),
      0,
      PI);
  FMorphMat := TColorMaterial.Create;
  FMorphMat.Color := TAlphaColorRec.Red;

  FValue := 0;
  FMorphValue := 0;

  GetKnobMetrics(FKnobType, FRDial, FRKnob, FRBtns, FRCntr);

  FCenterBtn := TTexControlGL.Create(self);
  FCenterBtn.OnClk := CntrBtnClick;
  FCenterBtn.TexCols := 1;
  FCenterBtn.TexRows := 2;

  FShowBtns := False;

  FIncDecBtns := TBtnIncDecGL.Create(self);
  FIncDecBtns.ColCount := 2;
  FIncDecBtns.RowCount := 1;
  FIncDecBtns.OnClk := BtnsClick;
  FIncDecBtns.Visible := FShowBtns;
  FIncDecBtns.HitTest := False;
end;

function TKnobGL.CreateBtnsTexture: TTiledTexture;
var
  R: TRectF;
begin
  Result := TTiledTexture.Create;
  R := RectF(0, 0, FRBtns.Width / SCALE_X, FRBtns.Height / SCALE_Y);
  if FKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
    Result.CreateSliderBtns(R, FKnobType = ktHSLider)
  else
    Result.CreateKnobBtns(R);
end;

function TKnobGL.CreateKnobTexture: TTiledTexture;
begin
  Result := TTiledTexture.Create;
  if FKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
    Result.CreateSlider(FRDial, FRKnob, FRBtns, FKnobType = ktHSlider)
  else
    Result.CreateKnob(FRDial, FRKnob);
end;

{procedure TKnobGL.CreateKnobVertices;
begin
  if assigned(FKnobMesh) and assigned(FKnobTexture) then
  begin
    FKnobMesh.CreateMesh(
      FKnobTexture.TileWidth,
      FKnobTexture.TileHeight,
      0, 1, 1);
  end;
end;}

procedure TKnobGL.CreateMorphVertices;
var Range: single;
begin
  if assigned(FMorphMesh) then
  begin
    Range := 2*PI - 55/180*PI;

    FMorphMesh.Center := PointF(FRKnob.Width / 2, FRKnob.Height / 2);
    FMorphMesh.Radius := PointF(FRKnob.Width / 2, FRKnob.Height / 2);
    FMorphMesh.StartAngle := 0;
    FMorphMesh.SweepAngle := Range * MorphValue;
    FMorphMesh.CreateMesh;
  end;
end;

function TKnobGL.CreateTexture: TTiledTexture;
begin
  Result := TTiledTexture.Create;
  if FKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
    Result.CreateSliderIndicator(FRDial, FRKnob, FRBtns, FKnobType = ktHSlider)
  else
    Result.CreateKnobIndicator(FRDial, FRKnob);
end;

destructor TKnobGL.Destroy;
begin
  FMorphMat.Free;
  FMorphMesh.Free;
  FKnob.Free;
  inherited;
end;

function TKnobGL.GetBtnsTexture: TTiledTexture;
begin
  Result := nil;
  if assigned(TextureList) and (TextureID <> '') then
  begin
    if not TextureList.Link(TextureID, Result) then
    begin
      Result := CreateBtnsTexture;
      TextureList.Add(TextureID, Result);
    end;
  end;
end;

procedure TKnobGL.KnobTexCreate(Sender: TObject; var aTiledTexture: TTiledTexture);
begin
  aTiledTexture := TTiledTexture.Create;
  if FKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
    aTiledTexture.CreateSlider(FRDial, FRKnob, FRBtns, FKnobType = ktHSlider)
  else
    aTiledTexture.CreateKnob(FRDial, FRKnob);
end;

procedure TKnobGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if ssLeft in Shift then begin

    FMouseDownValue := Value;
    FMouseDownPoint := PointF(X, Y);

    if PtInRect(FRBtns, FMouseDownPoint) then
    begin
      FIncDecBtns.MouseDown(Button, Shift, X - FRBtns.Left, Y - FRBtns.Top);
    end;
  end;
end;

procedure TKnobGL.MouseEnter;
begin
  inherited;
  ShowBtns := True;
end;

procedure TKnobGL.MouseLeave;
begin
  inherited;

  FIncDecBtns.MouseLeave;
end;

procedure TKnobGL.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Delta : single;
  AbsScale: TPoint3D;
begin
  inherited;

  if ssLeft in Shift then begin
    AbsScale := AbsScale3D;
    if AbsScale.Y = 0 then
      AbsScale.Y := 1.0;

    if FKnobType = ktHSlider then
      Delta := (FMaxValue - FMinValue) * (X - FMouseDownPoint.X) * AbsScale.X / 150
    else
      Delta := (FMaxValue - FMinValue) * (FMouseDownPoint.Y - Y) * AbsScale.Y / 150;

    FMouseDownPoint := PointF(X, Y);

    if Delta + Value > FMaxValue then
      Value := FMaxValue
    else
      if Delta + Value < FMinValue then
        Value := FMinValue
      else
        Value := Value + Delta;
  end;
end;

procedure TKnobGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if PtInRect(FRBtns, FMouseDownPoint) then
  begin
    FIncDecBtns.MouseUp(Button, Shift, X - FRBtns.Left, Y - FRBtns.Top);
    Invalidate;
  end;
end;

procedure TKnobGL.BtnsClick(Sender: TObject; Shift: TShiftState; const aValue: integer);
begin
  if FKnobType in [ktSlider, ktSeqSlider] then
  begin
    if aValue = 1 then
    begin
      if Value > FMinValue then
      begin
        Value := Value - 1;
      end;
    end;

    if aValue = 0 then
    begin
      if Value < FMaxValue then
      begin
        Value := Value + 1;
      end;
    end;

  end else begin
    if aValue = 0 then
    begin
      if Value > FMinValue then
      begin
        Value := Value - 1;
      end;
    end;

    if aValue = 1 then
    begin
      if Value < FMaxValue then
      begin
        Value := Value + 1;
      end;
    end;
  end;
end;

procedure TKnobGL.CntrBtnClick(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin
  Value := (FMaxValue - FMinValue + 1) / 2;
  Invalidate;
end;

procedure TKnobGL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  dx, dy, Angle, Range: single;
begin
  inherited;

  SaveMatrix := aContext.CurrentMatrix;
  try
    if FKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
    begin
      if FKnobType = ktHSlider then
      begin
        //Range := Texture.TileWidth - FKnobTexture.TileWidth - FRBtns.Width;
        Range := TileWidth - FKnob.TileWidth - FRBtns.Width;

        dx := Range;

        if (FMaxValue - FMinValue) <> 0 then
          dx := Range * (Value - FMinValue) / (FMaxValue - FMinValue);

        aContext.SetMatrix(
          TMatrix3D.CreateTranslation(Point3D(dx, 0, 0))
          * SaveMatrix);

      end else begin
        //Range := Texture.TileHeight - FKnobTexture.TileHeight - FRBtns.Height;
        Range := TileHeight - FKnob.TileHeight - FRBtns.Height;

        dy := Range;

        if (FMaxValue - FMinValue) <> 0 then
          dy := dy - Range * (Value - FMinValue) / (FMaxValue - FMinValue);

        aContext.SetMatrix(
          TMatrix3D.CreateTranslation(Point3D(0, dy, 0))
          * SaveMatrix);
      end;

      FKnob.Render(aContext);

      //aContext.DrawTriangles(FKnobMesh.VertexBuffer, FKnobMesh.IndexBuffer, FKnobMat, 1);

    end else begin
      dx := FRKnob.Width / 2;
      dy := FRKnob.Height / 2;

      Range := 360 - 55;

      Angle := 55 / 2;

      if (FMaxValue - FMinValue) <> 0 then
        Angle := Angle + Range * (Value - FMinValue) / (FMaxValue - FMinValue);

      aContext.SetMatrix(
        TMatrix3D.CreateTranslation(Point3D(-dx, -dy, 0))
        * TMatrix3D.CreateRotationZ(Angle * PI / 180)
        * TMatrix3D.CreateTranslation(Point3D(dx + FRKnob.Left, dy + FRKnob.Top, 0))
        * SaveMatrix);

      FKnob.Render(aContext);

      if MorphValue <> 0 then
        aContext.DrawTriangles(FMorphMesh.VertexBuffer, FMorphMesh.IndexBuffer, FMorphMat, 0.5);
    end;

  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

procedure TKnobGL.SetBtnsTextureID(const Value: string);
begin
  FBtnsTextureID := Value;
  FIncDecBtns.TextureID := Value;
end;

procedure TKnobGL.SetKnobTextureID(const aValue: string);
begin
  FKnobTextureID := aValue;
  FKnob.TextureID := aValue;
end;

procedure TKnobGL.SetKnobType(const aValue: TKnobType);
begin
  if FKnobType <> aValue then
  begin
    FKnobType := aValue;
    if FKnobType in [ktSlider, ktSeqSlider, ktHSlider] then
      ShowBtns := True;
    GetKnobMetrics(FKnobType, FRDial, FRKnob, FRBtns, FRCntr);
    UpdateTextureID;
  end;
end;

procedure TKnobGL.SetMaxValue(const aValue: single);
begin
  FMaxValue := aValue;
end;

procedure TKnobGL.SetMinValue(const aValue: single);
begin
  FMinValue := aValue;
end;

procedure TKnobGL.SetMorphValue(const aValue: single);
begin
  if FMorphValue <> aValue then
  begin
    FMorphValue := aValue;
    Invalidate;
  end;
end;

procedure TKnobGL.SetShowBtns(const aValue: boolean);
begin
  if aValue <> FShowBtns then
  begin
    FShowBtns := aValue;
    FIncDecBtns.Visible := aValue;
    Invalidate;
    if assigned(Parent) and (Parent is TControlGL) then
      (Parent as TControlGL).Invalidate
    else
      Invalidate;
  end;
end;

procedure TKnobGL.SetTextureList(const aValue: TTextureListGL);
begin
  FKnob.TextureList := aValue;
  FCenterBtn.TextureList := aValue;
  FIncDecBtns.TextureList := aValue;
  inherited;
end;

procedure TKnobGL.SetValue(const aValue: single);
begin
  if FValue <> aValue then begin

    FValue := aValue;

    {if assigned(Texture) and (FMaxValue - FMinValue <> 0) then begin
      ImageIndex :=
        Round((Texture.Cols * Texture.Rows - 1)
            * (FValue - FMinValue) / (FMaxValue - FMinValue));
    end else begin
      ImageIndex := 0;
    end;}

    Invalidate;

    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

procedure TKnobGL.UpdateTextureID;
begin
  TexCols := 1;
  TexRows := 1;

  TextureID := GetKnobIndicatorTexID(FKnobType);
  KnobTextureID := GetKnobTexID(FKnobType);
  case KnobType of
    ktBig,
    ktMedium,
    ktSmall:
      begin
        FIncDecBtns.TextureID := 'KnobBtns';
      end;
    ktReset,
    ktResetMedium:
      begin
        //FIncDecBtns.TextureID := 'KnobBtns';
        BtnsTextureID := 'KnobBtns';

        FCenterBtn.Parent := Self;
        FCenterBtn.TextureID := 'CenterBtn';
        FCenterBtn.Position := Point3D(
          FRCntr.Left,
          FRCntr.Top,
          0);

      end;
    ktSlider,
    ktSeqSlider:
      begin
        FIncDecBtns.ColCount := 1;
        FIncDecBtns.RowCount := 2;
        //FIncDecBtns.TextureID := 'SliderBtns';
        BtnsTextureID := 'SliderBtns';
      end;
    ktHSlider:
      begin
        FIncDecBtns.ColCount := 2;
        FIncDecBtns.RowCount := 1;
        //FIncDecBtns.TextureID := 'SlidBtnsH';
        FIncDecBtns.BtnWidth := (FRBtns.Width / SCALE_X) / 2;
        FIncDecBtns.BtnHeight := FRBtns.Height / SCALE_Y;
        BtnsTextureID := 'SlidBtnsH';
      end;
  end;

  FIncDecBtns.Parent := Self;
  FIncDecBtns.Position := Point3D(
    FRBtns.Left,
    FRBtns.Top,
    0);
end;

{ TTextEdit2GL }

constructor TTextEdit2GL.Create(AOwner: TComponent);
begin
  inherited;

  FLabel := TLabelStaticGL.Create(
    FONT_FAMILY,
    SCALE_Y * FONTSIZE_Y,
    TAlphaColorRec.Black,
    TTextAlign.Center,
    '',
    Width,
    Height);
end;

function TTextEdit2GL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtn(BtnWidth, BtnHeight);
  end;
end;

destructor TTextEdit2GL.Destroy;
begin
  FreeAndNil(FLabel);
  inherited;
end;

function TTextEdit2GL.GetCaption: string;
begin
  Result := FLabel.Text;
end;

procedure TTextEdit2GL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  dx, dy: single;
  Texture: TTiledTexture;
begin
  inherited;

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    FLabel.Width := TileWidth;
    FLabel.Height := TileHeight;

    SaveMatrix := aContext.CurrentMatrix;
    try
      dx := (TileWidth - FLabel.Width) / 2;
      dy := (TileHeight - FLabel.Height) / 2;

      aContext.SetMatrix(
           TMatrix3D.CreateTranslation(
             Point3D( 1 * SCALE_X + dx, 1 * SCALE_Y + dy, 0)) * SaveMatrix);
       FLabel.Render(aContext);
    finally
      aContext.SetMatrix(SaveMatrix);
    end;
  end;
end;

procedure TTextEdit2GL.SetCaption(const aValue: string);
begin
  if FLabel.Text <> aValue then
  begin
    FLabel.Text := aValue;
  end;
end;

procedure TTextEdit2GL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    TexCols := 1;
    TexRows := 2;
    TextureID := GetBtnTexID(BtnWidth, BtnHeight)
  end;
end;

{ TTextEditGL }

constructor TTextEditGL.Create(AOwner: TComponent);
begin
  inherited;

  FLabel := TMatrixTextObject.Create;
end;

function TTextEditGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateBtn(BtnWidth, BtnHeight);
  end;
end;

destructor TTextEditGL.Destroy;
begin
  FreeAndNil(FLabel);
  inherited;
end;

function TTextEditGL.GetCaption: string;
begin
  Result := FLabel.Text;
end;

procedure TTextEditGL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  ClientWidth, ClientHeight, dx, dy, Zoom, Margin: single;
  Texture: TTiledTexture;
begin
  inherited;

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    Zoom := 1;
    Margin := 1 * SCALE_X;

    ClientWidth := TileWidth - Margin * 2;
    ClientHeight := TileHeight - Margin * 2;

    if (FLabel.TextWidth * Zoom) > ClientWidth then
      FLabel.Width := ClientWidth
    else
      FLabel.Width := FLabel.TextWidth * Zoom;

    if (FLabel.TextHeight * Zoom) > ClientHeight then
      FLabel.Height := ClientHeight
    else
      FLabel.Height := FLabel.TextHeight * Zoom;

    SaveMatrix := aContext.CurrentMatrix;
    try
      dx := (TileWidth - FLabel.Width) / 2;
      dy := (TileHeight - FLabel.Height) / 2;

      aContext.SetMatrix(
           TMatrix3D.CreateTranslation(
             Point3D(dx, dy, 0)) * SaveMatrix);
       FLabel.Render(aContext);
    finally
      aContext.SetMatrix(SaveMatrix);
    end;
  end;
end;

procedure TTextEditGL.SetCaption(const aValue: string);
begin
  if FLabel.Text <> aValue then
  begin
    FLabel.Text := aValue;
    Invalidate;
  end;
end;

procedure TTextEditGL.SetTextureList(const aValue: TTextureListGL);
begin
  inherited;
  FLabel.TextureList := aValue;
end;

procedure TTextEditGL.UpdateTextureID;
begin
  if (BtnWidth <> 0) and (BtnHeight <> 0) then
  begin
    TexCols := 1;
    TexRows := 2;
    TextureID := GetBtnTexID(BtnWidth, BtnHeight)
  end;
end;

{ TTextFieldGL }

constructor TTextFieldGL.Create(AOwner: TComponent);
begin
  inherited;

  FLabel := TMatrixTextObject.Create;
end;

function TTextFieldGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (FieldWidth <> 0) and (FieldHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateTextField(FieldWidth, FieldHeight);
  end;
end;

destructor TTextFieldGL.Destroy;
begin
  FreeAndNil(FLabel);
  inherited;
end;

function TTextFieldGL.GetCaption: string;
begin
  Result := FLabel.Text;
end;

procedure TTextFieldGL.RenderControl(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  ClientWidth, ClientHeight, dx, dy, Zoom, Margin: single;
  Texture: TTiledTexture;
begin
  inherited;

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    Zoom := 1;
    Margin := 1 * SCALE_X;

    ClientWidth := TileWidth - Margin * 2;
    ClientHeight := TileHeight - Margin * 2;

    if (FLabel.TextWidth * Zoom) > ClientWidth then
      FLabel.Width := ClientWidth
    else
      FLabel.Width := FLabel.TextWidth * Zoom;

    if (FLabel.TextHeight * Zoom) > ClientHeight then
      FLabel.Height := ClientHeight
    else
      FLabel.Height := FLabel.TextHeight * Zoom;

    SaveMatrix := aContext.CurrentMatrix;
    try
      dx := (TileWidth - FLabel.Width) / 2;
      dy := (TileHeight - FLabel.Height) / 2;

      aContext.SetMatrix(
           TMatrix3D.CreateTranslation(
             Point3D(dx, dy, 0)) * SaveMatrix);
       FLabel.Render(aContext);
    finally
      aContext.SetMatrix(SaveMatrix);
    end;
  end;
end;

procedure TTextFieldGL.SetCaption(const aValue: string);
begin
  if FLabel.Text <> aValue then
  begin
    FLabel.Text := aValue;
    Invalidate;
  end;
end;

procedure TTextFieldGL.SetFieldHeight(const Value: single);
begin
  if FFieldHeight <> Value then
  begin
    FFieldHeight := Value;
    UpdateTextureID;
  end;
end;

procedure TTextFieldGL.SetFieldWidth(const Value: single);
begin
  if FFieldWidth <> Value then
  begin
    FFieldWidth := Value;
    UpdateTextureID;
  end;
end;

procedure TTextFieldGL.SetTextureList(const aValue: TTextureListGL);
begin
  inherited;
  FLabel.TextureList := aValue;
end;

procedure TTextFieldGL.UpdateTextureID;
begin
  if (FieldWidth <> 0) and (FieldHeight <> 0) then
  begin
    TexCols := 1;
    TexRows := 1;
    TextureID := GetTextFieldTexID(FieldWidth, FieldHeight)
  end;
end;

{ TGraphGL }

constructor TGraphGL.Create(AOwner: TComponent);
begin
  inherited;

end;

function TGraphGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
  if (FieldWidth <> 0) and (FieldHeight <> 0) then
  begin
    Result := TTiledTexture.Create;
    Result.CreateGraph(FieldWidth, FieldHeight);
  end;
end;

destructor TGraphGL.Destroy;
begin

  inherited;
end;

procedure TGraphGL.RenderControl(aContext: TContext3D);
begin
  inherited;

end;

procedure TGraphGL.SetFieldHeight(const Value: single);
begin
  if FFieldHeight <> Value then
  begin
    FFieldHeight := Value;
    UpdateTextureID;
  end;
end;

procedure TGraphGL.SetFieldWidth(const Value: single);
begin
  if FFieldWidth <> Value then
  begin
    FFieldWidth := Value;
    UpdateTextureID;
  end;
end;

procedure TGraphGL.UpdateTextureID;
begin
  if (FieldWidth <> 0) and (FieldHeight <> 0) then
  begin
    TexCols := 1;
    TexRows := 1;
    TextureID := GetTextFieldTexID(FieldWidth, FieldHeight)
  end;
end;

{ TLabelGL }

constructor TLabelGL.Create(AOwner: TComponent);
begin
  inherited;

  FFont := TFont.Create;
  FColor := TAlphaColorRec.Black;
  FText := '';
  FTextAlign := TTextAlign.Leading;
end;

function TLabelGL.CreateTexture: TTiledTexture;
var w, h: single;
begin
  Result := TTiledTexture.Create;
  Result.SetSize(10, 10);

  Result.Canvas.BeginScene;
  try
    Result.Canvas.Font.Assign(FFont);
    w := Result.Canvas.TextWidth(FText);
    h := Result.Canvas.TextHeight(FText);
  finally
    Result.Canvas.EndScene;
  end;

  if w <= 0 then
    w := 10;

  if h <= 0 then
    h := 10;

  Result.SetSize(Round(w), Round(h));
  //Result.TileWidth := Result.Width;
  //Result.TileHeight := Result.Height;

  Result.Canvas.BeginScene;
  try
    Result.Canvas.Clear(0);
    Result.Canvas.Font.Assign(FFont);
    Result.Canvas.Fill.Color := FColor;
    Result.Canvas.FillText(RectF(0, 0, w, h), FText, False, 1, [],
      FTextAlign, TTextAlign.Center);
  finally
    Result.Canvas.EndScene;
  end;
end;

destructor TLabelGL.Destroy;
begin
  if assigned(FTexture) then
    FTexture.DisposeOf;

  FreeAndNil(FFont);
  inherited;
end;

function TLabelGL.GetCaption: string;
begin
  Result := FText;
end;

procedure TLabelGL.SetCaption(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    UpdateTexture;
  end;
end;

procedure TLabelGL.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateTexture;
  end;
end;

procedure TLabelGL.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  UpdateTexture;
end;

procedure TLabelGL.SetTextAlign(const Value: TTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    UpdateTexture;
  end;
end;

procedure TLabelGL.UpdateTexture;
begin
  if assigned(FTexture) then
    FTexture.DisposeOf;

  FTexture := CreateTexture;
end;

procedure TLabelGL.UpdateTextureID;
begin
  // Not kept in a texturelist
end;

{ TKeyBlackGL }

constructor TKeyBlackGL.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TKeyBlackGL.Destroy;
begin
  inherited;
end;

procedure TKeyBlackGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then begin
    Value := 1;
  end;

  inherited;
end;

procedure TKeyBlackGL.MouseLeave;
begin
  Value := 0;
end;

procedure TKeyBlackGL.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
    Value := 1;
end;

procedure TKeyBlackGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  Value := 0;
end;

procedure TKeyBlackGL.SetValue(const aValue: integer);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    ImageIndex := FValue;
    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

{ TKeyboardGL }

constructor TKeyboardGL.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;

  for i := 0 to 4 do
  begin
    FKeysBlack[i] := TKeyBlackGL.Create(self);
    FKeysBlack[i].Parent := Self;
    FKeysBlack[i].ID := i;
    FKeysBlack[i].OnChange := KeyBlackChange;
  end;

  RowCount := 1;
  ColCount := 7;

  FValue := -1;

  UpdateTextureID;
end;

destructor TKeyboardGL.Destroy;
begin
  inherited;
end;

procedure TKeyboardGL.KeyBlackChange(Sender: TObject);
begin
  if Sender is TKeyBlackGL then
    if (Sender as TKeyBlackGL).Value = 0 then
      Value := -1
    else
      Value := BLACK_KEY_NRS[(Sender as TKeyBlackGL).ID];
  Invalidate;
end;

procedure TKeyboardGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  i: integer;
begin
  if ssLeft in Shift then
  begin
    i := IndexFromPoint(X, Y);
    if (i >= 0) and (i < ColCount * RowCount) then
      Value := WHITE_KEY_NRS[i];
  end;

  inherited;
end;

procedure TKeyboardGL.MouseLeave;
begin
  Value := -1;
end;

procedure TKeyboardGL.MouseMove(Shift: TShiftState; X, Y: Single);
var
  i: integer;
begin
  if ssLeft in Shift then
  begin
    i := 0;
    while (i < Length(FKeysBlack)) and (not PtInRect(FKeysBlack[i].BoundsRect, PointF(X, Y))) do
      inc(i);

    if i < Length(FKeysBlack) then
    begin
      Value := BLACK_KEY_NRS[i];
    end else begin
      i := IndexFromPoint(X, Y);
      if (i >= 0) and (i < ColCount * RowCount) then
        Value := WHITE_KEY_NRS[i];
    end;
  end;
end;

procedure TKeyboardGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  Value := -1;
end;

procedure TKeyboardGL.SetTextureList(const aValue: TTextureListGL);
var
  i: integer;
begin
  for i := 0 to 4 do
  begin
    FKeysBlack[i].TextureList := aValue;
  end;

  inherited;

  UpdateTextureID;
end;

procedure TKeyboardGL.SetValue(const aValue: integer);
var
  i: integer;
begin
  if aValue <> FValue then
  begin
    FValue := aValue;
    for i := 0 to (ColCount * RowCount) - 1 do
    begin
      if WHITE_KEY_NRS[i] = aValue then
        CellIndex[i] := 1
      else
        CellIndex[i] := 0;
    end;

    if assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TKeyboardGL.UpdateTextureID;
var
  i: integer;
  ww, bw: single;
begin
  inherited;

  TexCols := 7;
  TexRows := 2;

  TextureID := 'KeyWhite';
  for i := 0 to 4 do
  begin
    FKeysBlack[i].TexCols := 1;
    FKeysBlack[i].TexRows := 2;
    FKeysBlack[i].TextureID := 'KeyBlack';
  end;

  ww := TileWidth;
  bw := FKeysBlack[0].TileWidth;

  for i := 0 to 4 do
  begin
    case i of
    0, 1:
      FKeysBlack[i].Position := Point3D(ww * i + ww - bw/2, 0, 0);
    2, 3, 4:
      FKeysBlack[i].Position := Point3D(ww * (i + 1) + ww - bw/2, 0, 0);
    end;
  end;
  Invalidate;
end;


initialization
  RegisterFmxClasses([TBtnTextGL, TBtnFlatGL, TBtnIncDecGL, TBtnRadioGL, TKnobGL, TKeyboardGL]);

end.

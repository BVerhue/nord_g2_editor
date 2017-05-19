unit Sidepanel;

//  Copyright (c) 2013 Bruno Verhue
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:
//  1. Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. The name of the author may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
//  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
//  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
//  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
//  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
//  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Forms, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics;

type
  TSidepanelState = (spsCollapsed, spsExpanded);

  TSidePanel = class;

  TSidePanelCaption = class(TCustomControl)
  private
    FSidePanel : TSidePanel;
    FCaptionHeight : integer;
    FSidepanelState : TSidepanelState;
  protected
    procedure SetSidePanelState( const aSidePanelState : TSidePanelState);
    procedure Paint; override;
  public
    constructor CreateLinked(aSidePanel: TSidePanel);

    property SidePanelState : TSidePanelState read FSidePanelState write SetSidePanelState;
  end;

  TPage = class(TCustomControl)
  public
    constructor CreateLinked(aSidePanel: TSidePanel);
  end;

  TSidePanel = class(TCustomControl)
  private
    FCaptionPanel : TSidePanelCaption;
    FExpandedWidth : integer;
    FDragging : Boolean;
    FLastPos : TPoint;
    FOnCollapse : TNotifyEvent;
    FOnExpand : TNotifyEvent;
    FClientPanel : TPage;
  protected
    procedure Resize; override;

    procedure CaptionClick(Sender: TObject);

    function GetCaption : string;
    procedure SetCaption( const aValue : string);
    function GetCaptionHeight : integer;
    procedure SetCaptionHeight( const aValue : integer);
    function GetSidePanelState : TSidePanelState;
    procedure SetSidePanelState( const aValue : TSidePanelState);
    function GetCaptionColor : TColor;
    procedure SetCaptionColor( const aValue : TColor);

    procedure DoResize;

    procedure MouseDown(Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildParent:TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property Align;
    property Caption : string read GetCaption write SetCaption;
    property SidePanelState : TSidePanelState read GetSidePanelState write SetSidePanelState;
    property CaptionHeight : integer read GetCaptionHeight write SetCaptionHeight;
    property ExpandedWidth : integer read FExpandedWidth write FExpandedWidth;
    property CaptionColor : TColor read GetCaptionColor write SetCaptionColor;
    property Color;
    property Font;
    property OnCollapse : TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand : TNotifyEvent read FOnExpand write FOnExpand;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NM G2', [TSidePanel]);
end;

{ TSidePanel }

constructor TSidePanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csOpaque, csDoubleClicks, csGestures];

  Color := clBtnFace;

  FExpandedWidth := 100;
  Width := FExpandedWidth;
  Height := 100;

  FCaptionPanel := TSidePanelCaption.CreateLinked(self);
  FClientPanel := TPage.CreateLinked(self);

  FCaptionPanel.Caption := Caption;
  FCaptionPanel.OnClick := CaptionClick;
end;

procedure TSidePanel.DoResize;
begin
  //if not(assigned(FCaptionPanel) and assigned(FClientPanel)) then
  //  exit;

  FCaptionPanel.Top := 0;
  if SidepanelState = spsExpanded then begin
    FCaptionPanel.SetBounds(0,0,ClientWidth, 16);
    FClientPanel.SetBounds(1,17,ClientWidth-2,ClientHeight-18);
  end else begin
    FCaptionPanel.SetBounds(0,0,ClientWidth, ClientHeight);
    FClientPanel.SetBounds(0,0,0,0);
  end;
end;

function TSidePanel.GetCaption: string;
begin
  Result := FCaptionPanel.Caption;
end;

procedure TSidePanel.SetCaption(const aValue: string);
begin
  if aValue <> FCaptionPanel.Caption then begin
    FCaptionPanel.Caption := aValue;
    FCaptionPanel.Invalidate;
  end;
end;

function TSidePanel.GetSidePanelState: TSidePanelState;
begin
  Result := FCaptionPanel.SidePanelState;
end;

procedure TSidePanel.SetSidePanelState(const aValue: TSidePanelState);
var i : integer;
begin
  if aValue <> FCaptionPanel.SidePanelState then begin

    case FCaptionPanel.SidepanelState of
      spsExpanded :
        begin
          FCaptionPanel.SidepanelState := spsCollapsed;

          DoResize;

          FExpandedWidth := Width;
          Width := FCaptionPanel.FCaptionHeight;

          if assigned(FOnCollapse) then
            FOnCollapse(self);
        end;
      spsCollapsed :
        begin
          Width := FExpandedWidth;
          FCaptionPanel.SidepanelState := spsExpanded;

          DoResize;

          if assigned(FOnExpand) then
            FOnExpand(self);
        end;
    end;
  end;
end;

function TSidePanel.GetCaptionHeight: integer;
begin
  Result := FCaptionPanel.FCaptionHeight;
end;

function TSidePanel.GetChildParent: TComponent;
begin
  Result := FClientPanel;
end;

procedure TSidePanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FClientPanel.GetChildren(Proc, Root);
end;

procedure TSidePanel.SetCaptionHeight(const aValue: integer);
begin
  if aValue <> FCaptionPanel.FCaptionHeight then begin
    FCaptionPanel.FCaptionHeight := aValue;
    Invalidate;
  end;
end;

function TSidePanel.GetCaptionColor: TColor;
begin
  Result := FCaptionPanel.Color;
end;

procedure TSidePanel.SetCaptionColor(const aValue: TColor);
begin
  if aValue <> FCaptionPanel.Color then begin
    FCaptionPanel.Color := aValue;
    FCaptionPanel.Invalidate;
  end;
end;

procedure TSidePanel.CaptionClick(Sender: TObject);
begin
   if SidepanelState = spsExpanded then
     SidePanelState := spsCollapsed
   else
     SidePanelState := spsExpanded;
end;

procedure TSidePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FCaptionPanel.SidePanelState = spsExpanded then begin
    if Align = alLeft then begin
      if (Button = mbLeft) and ((Width - x ) < 10) then begin
        FDragging := True;
        FLastPos := Point(x, y);
        MouseCapture := true;
        Screen.cursor := crSizeWE;
      end else
        inherited;
    end else
      if (Button = mbLeft) and ((Width - x ) < 10) and
        ((Height - y ) < 10) then begin
        FDragging := True;
        FLastPos := Point(x, y);
        MouseCapture := true;
        Screen.cursor := crSizeNWSE;
      end else
        inherited;
  end else
    inherited;
end;

procedure TSidePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
begin
  if FDragging then begin
    r := BoundsRect;
    SetBounds( r.left, r.top, r.right - r.left + X - FlastPos.X,
    r.bottom - r.top + Y - Flastpos.Y );
    FLastPos := Point( x, y );
  end else begin

    inherited;
    Cursor := crDefault;

    if FCaptionPanel.SidePanelState = spsExpanded then begin
      if Align = alLeft then begin
        if (Width - x ) < 10 then begin
          Cursor := crSizeWE
        end else
          if ((Width - x ) < 10) and ((Height - y ) < 10) then
            Cursor := crSizeNWSE;
      end;
    end;

  end;
end;

procedure TSidePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
  begin
    FDragging := False;
    MouseCapture := false;
    Screen.Cursor := crDefault;
  end else
    inherited;
end;

procedure TSidePanel.Resize;
begin
  DoResize;
  inherited;
end;

{ TPage }

constructor TPage.CreateLinked(aSidePanel: TSidePanel);
begin
  inherited Create(aSidePanel);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csOpaque, csReplicatable, csGestures];
  Parent := aSidePanel;
end;

{ TSidePanelCaption }

constructor TSidePanelCaption.CreateLinked(aSidePanel: TSidePanel);
begin
  inherited Create(aSidePanel);
  FSidePanel := aSidePanel;
  Parent := aSidePanel;

  FSidepanelState := spsExpanded;
  FCaptionHeight := 16;

  Color := clBtnFace;

  Width := FSidePanel.Width;
  Height := FCaptionHeight;

  Constraints.MaxHeight := Height;
end;

procedure TSidePanelCaption.Paint;

  procedure DrawArrowLeft( aLeft, aTop, aSize : integer);
  //var Points : array[0..2] of TPoint;
  var old_font : TFont;
  begin
    {Points[0].X := aLeft;         Points[0].Y := aTop + aSize div 2;
    Points[1].X := aLeft + aSize; Points[1].Y := aTop;
    Points[2].X := aLeft + aSize; Points[2].Y := aTop + aSize;
    Canvas.Polygon( Points);}
    old_font := TFont.Create;
    try
      old_font.Assign(Canvas.Font);
      Canvas.Font.Name := 'Marlett';
      Canvas.Font.Size := aSize;
      Canvas.TextOut( aLeft, aTop, '3' );
      Canvas.Font.Assign(old_font);
    finally
      old_font.Free;
    end;
  end;

  procedure DrawArrowRight( aLeft, aTop, aSize : integer);
  //var Points : array[0..2] of TPoint;
  var old_font : TFont;
  begin
    {Points[0].X := aLeft;         Points[0].Y := aTop;
    Points[1].X := aLeft + aSize; Points[1].Y := aTop + aSize div 2;
    Points[2].X := aLeft;         Points[2].Y := aTop + aSize;
    Canvas.Polygon( Points);}
    old_font := TFont.Create;
    try
      old_font.Assign(Canvas.Font);
      Canvas.Font.Name := 'Marlett';
      Canvas.Font.Size := aSize;
      Canvas.TextOut( aLeft, aTop, '4' );
      Canvas.Font.Assign(old_font);
    finally
      old_font.Free;
    end;
  end;

  procedure TextAngle( aLeft, aTop : integer; aAngle : integer; aText : string);
  var old_font : TFont;
      lf : TLogFont;
      tf : TFont;
  begin
    old_font := TFont.Create;
    try
      old_font.Assign(Canvas.Font);
      with Canvas do begin
        //Font.Name := 'Arial';
        //Font.Size := 24;
        tf := TFont.Create;
        try
          tf.Assign(FSidePanel.Font) ;
          GetObject(tf.Handle, sizeof(lf), @lf) ;
          lf.lfEscapement := aAngle;
          lf.lfOrientation := aAngle;
          tf.Handle := CreateFontIndirect(lf) ;
          Font.Assign(tf) ;
        finally
          tf.Free;
        end;
        TextOut(aLeft, aTop, aText) ;
      end;
      Canvas.Font.Assign(old_font);
    finally
      old_font.Free;
    end;
  end;

begin
  inherited;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle( ClientRect);

  case FSidepanelState of
    spsCollapsed :
      begin
        DrawArrowRight( 2, 1, 10);
        Canvas.Brush.Style := bsClear;
        //TextAngle( 0, FSidePanel.Canvas.TextWidth(Caption) + 4 + 10, 900, Caption);
        TextAngle( 0, FSidePanel.Height{ - FSidePanel.Canvas.TextWidth(Caption)} - 8, 900, Caption);
      end;
    spsExpanded :
      begin
        DrawArrowLeft( Width - 12, 1, 10);
        Canvas.Brush.Style := bsClear;
        TextAngle( 4, 1, 0, Caption);
      end;
  end;
end;

procedure TSidePanelCaption.SetSidePanelState(
  const aSidePanelState: TSidePanelState);
begin
  if FSidePanelState <> aSidePanelState then begin
    FSidePanelState := aSidePanelState;
    case FSidePanelState of
      spsCollapsed :
        begin
          Constraints.MaxHeight := 0;
          Align := alClient;
        end;
      spsExpanded :
        begin
          Align := alTop;
          Height := FCaptionHeight;
          Constraints.MaxHeight := Height;
        end;
    end;
    Invalidate;
  end;
end;

end.

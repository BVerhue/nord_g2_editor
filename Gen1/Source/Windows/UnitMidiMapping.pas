unit UnitMidiMapping;

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

//  ////////////////////////////////////////////////////////////////////////////
//
//  Unit to map midi to editor UI elements, for use of editor with controler
//
//  ////////////////////////////////////////////////////////////////////////////

// http://stackoverflow.com/questions/10835355/in-place-editing-of-a-subitem-in-a-tlistview

interface

uses
{$IFDEF G2_VER220_up}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Contnrs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Menus,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Contnrs, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, Menus,
{$ENDIF}
  DOM, XMLRead, XMLWrite,
  MMSystem, MidiType, MidiIn, MidiOut,
  g2_types, g2_database, g2_file, g2_midi, JawsCtrls;


Const
  USER_EDITLISTVIEW = WM_USER + 666;

type
  TListItemMidiEditorAssignment = class(TMidiEditorAssignment)
  public
    constructor Create( aG2 : TG2Midi);
    destructor Destroy; override;

    procedure WriteValues( aListItem : TListItem);
    procedure ReadValues( aListItem : TListItem);
  end;

  TfrmMidiMapping = class(TForm)
    lvEditorMidiAssignments: DListView;
    puCtrlAssignMidi: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvEditorMidiAssignmentsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    FMidiEditorAssignmentList : TMidiEditorAssignmentList;
    FLastMidiEvent            : TMyMidiEvent;
    FCtrlMidiOutputList       : TMidiOutputList;
    FCurrentControl           : TMidiAwareControl;
    FCurrentIndex             : integer;

    ListViewEditor: TEdit;
    LItem: TListitem;

    procedure UserEditListView( Var Message: TMessage ); message USER_EDITLISTVIEW;
    procedure ListViewEditorExit(Sender: TObject);
  public
    { Public declarations }
    procedure   LoadIniXML;
    function    GetMidiCtrlAssignmentControl( aControlPath : string): TControl;

    procedure   UpdateCtrlMidiOutputList;
    procedure   ProcessCtrlMidiMessage( aCtrlMidiInput : TMidiInput);
    procedure   DoCtrlMidiInput(Sender: TObject);

    procedure   UpdateList;

    procedure   PopupMenu( aControl : TMidiAwareControl; X, Y : integer);
    procedure   AssignCtrlMidi(Sender: TObject);
    procedure   DeassignCtrlMidi(Sender: TObject);

    procedure   AddMidiEditorAssignment( aControl : TMidiAwareControl;  aMidiChannel, aMidiNote : byte; aMidiCC : byte; aControlIndex : byte);

    property    MidiEditorAssignmentList : TMidiEditorAssignmentList read FMidiEditorAssignmentList;
  end;

var
  frmMidiMapping: TfrmMidiMapping;

implementation

{$R *.dfm}

uses UnitSettings, CommCtrl, UnitG2Editor;

const
  EDIT_COLUMN = -1; //Index of the column to Edit

procedure TfrmMidiMapping.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmG2Main.SetMidiBoxVisible( False);
end;

procedure TfrmMidiMapping.FormCreate(Sender: TObject);
begin
  //create the TEdit and assign the OnExit event

  ListViewEditor := TEdit.Create(Self);
  ListViewEditor.Parent:= lvEditorMidiAssignments;
  ListViewEditor.OnExit := ListViewEditorExit;
  ListViewEditor.Visible := False;

  FCtrlMidiOutputList := TMidiOutputList.Create( False);

  FMidiEditorAssignmentList := TMidiEditorAssignmentList.Create( True);

  FLastMidiEvent := TMyMidiEvent.Create;
  LoadIniXML;
end;

procedure TfrmMidiMapping.FormDestroy(Sender: TObject);
begin
  FLastMidiEvent.Free;

  FMidiEditorAssignmentList.Free;

  FCtrlMidiOutputList.Free;
end;

procedure TfrmMidiMapping.FormShow(Sender: TObject);
begin
  UpdateList;
  frmG2Main.SetMidiBoxVisible( True);
end;

procedure TfrmMidiMapping.lvEditorMidiAssignmentsClick(Sender: TObject);
var
  LPoint: TPoint;
  LVHitTestInfo: TLVHitTestInfo;
begin
  LPoint:= lvEditorMidiAssignments.ScreenToClient(Mouse.CursorPos);
  ZeroMemory( @LVHitTestInfo, SizeOf(LVHitTestInfo));
  LVHitTestInfo.pt := LPoint;
  //Check if the click was made in the column to edit
  If (lvEditorMidiAssignments.perform( LVM_SUBITEMHITTEST, 0, LPARAM(@LVHitTestInfo))<>-1) and ( LVHitTestInfo.iSubItem = EDIT_COLUMN ) Then
    PostMessage( self.Handle, USER_EDITLISTVIEW, LVHitTestInfo.iItem, 0 )
  else
    ListViewEditor.Visible:=False; //hide the TEdit
end;

procedure TfrmMidiMapping.ListViewEditorExit(Sender: TObject);
begin
  If Assigned(LItem) Then
  Begin
    //assign the vslue of the TEdit to the Subitem
    LItem.SubItems[ EDIT_COLUMN-1 ] := ListViewEditor.Text;
    LItem := nil;
  End;
end;

procedure TfrmMidiMapping.UserEditListView(var Message: TMessage);
var
  LRect: TRect;
begin
  LRect.Top := EDIT_COLUMN;
  LRect.Left:= LVIR_BOUNDS;
  lvEditorMidiAssignments.Perform( LVM_GETSUBITEMRECT, Message.wparam,  LPARAM(@LRect) );
  MapWindowPoints( lvEditorMidiAssignments.Handle, ListViewEditor.Parent.Handle, LRect, 2 );
  //get the current Item to edit
  LItem := lvEditorMidiAssignments.Items[ Message.wparam ];
  //set the text of the Edit
  ListViewEditor.Text := LItem.Subitems[ EDIT_COLUMN-1];
  //set the bounds of the TEdit
  ListViewEditor.BoundsRect := LRect;
  //Show the TEdit
  ListViewEditor.Visible:=True;
end;


procedure TfrmMidiMapping.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    Control : TControl;
    i, j : integer;
    MidiEditorAssignment : TListItemMidiEditorAssignment;
    FormSettingsNode : TXMLFormSettingsType;
    CtrlMidiAssignmentListNode : TDOMNode;
    CtrlMidiAssignmentList : TXMLCtrlMidiassignmentListType;
    CtrlMidiassignmentNode : TXMLCtrlMidiassignmentType;
    G2 : TG2Midi;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  G2 := frmG2Main.FirstG2;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      CtrlMidiAssignmentListNode := RootNode.FindNode('CTRL_MIDI_ASSIGNMENT_LIST');
      if assigned(CtrlMidiAssignmentListNode) then begin

        CtrlMidiAssignmentList := TXMLCtrlMidiassignmentListType.Create( CtrlMidiAssignmentListNode);
        if assigned(CtrlMidiAssignmentList) then begin
          try
            for i := 0 to CtrlMidiAssignmentList.Count - 1 do begin
              CtrlMidiAssignmentNode := CtrlMidiAssignmentList[i];
              if assigned( CtrlMidiassignmentNode) then begin
                MidiEditorAssignment := TListItemMidiEditorAssignment.Create( G2);
                MidiEditorAssignment.Channel := CtrlMidiassignmentNode.Channel;
                MidiEditorAssignment.Note := CtrlMidiassignmentNode.Note;
                MidiEditorAssignment.ControlIndex := CtrlMidiassignmentNode.ControlIndex;
                MidiEditorAssignment.CC := CtrlMidiassignmentNode.CC;
                MidiEditorAssignment.MinValue := CtrlMidiassignmentNode.MinValue;
                MidiEditorAssignment.MaxVAlue := CtrlMidiassignmentNode.MaxValue;
                MidiEditorAssignment.ControlPath := CtrlMidiassignmentNode.ControlPath;
                MidiEditorAssignment.MidiOutputList := FCtrlMidiOutputList;
                Control := GetMidiCtrlAssignmentControl( MidiEditorAssignment.ControlPath);
                if assigned(Control) then begin
                  MidiEditorAssignment.Control := Control as TMidiAwareControl;
                  frmMidiMapping.MidiEditorAssignmentList.Add(MidiEditorAssignment)
                end else
                  MidiEditorAssignment.Free;
              end;
            end;
          finally
            CtrlMidiAssignmentList.Free;
          end;
        end;
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('MidiMappingForm'));
      if assigned(FormSettingsNode) then begin
        Left := FormSettingsNode.PosX;
        Top := FormSettingsNode.PosY;
        Width := FormSettingsNode.SizeX;
        Height := FormSettingsNode.SizeY;
        Visible := FormSettingsNode.Visible;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

function TfrmMidiMapping.GetMidiCtrlAssignmentControl( aControlPath : string): TControl;
var p : integer;
    i : integer;
    FrmName, CtrlName : string;

    procedure GetControl( ParentControl : TWinControl);
    var j : integer;
    begin

      p := pos('/', aControlPath);

      if p > 0 then begin
        CtrlName := copy(aControlPath, 1, p - 1);
        aControlPath := copy(aControlPath, p + 1, (Length(aControlPath) - p));
      end else begin
        CtrlName := aControlPath;
        aControlPath := '';
      end;

      j := 0;
      while (j < ParentControl.ControlCount) and not(ParentControl.Controls[j].Name = CtrlName) do
        inc(j);

      if (j < ParentControl.ControlCount) then begin
        if aControlPath = '' then
          Result := ParentControl.Controls[j]
        else begin
          if ParentControl.Controls[j] is TWinControl then
            GetControl( ParentControl.Controls[j] as TWinControl);
        end;
      end;
    end;

begin
  // Search the control in the application and connect to assignmentinfo

  // For this to work, frmMidiMapping must be the last form that's created

  Result := nil;

  // aControlPath Form/Control/Control...

  p := pos('/', aControlPath);

  if p = 1 then
    aControlPath := copy(aControlPath, 2, (Length(aControlPath) - 1));

  p := pos('/', aControlPath);

  FrmName := copy(aControlPath, 1, p - 1);
  aControlPath := copy(aControlPath, p + 1, (Length(aControlPath) - p));

  i := 0;
  while (i < Application.ComponentCount) and not((Application.Components[i] is TForm)
                      and ((Application.Components[i] as TForm).Name = FrmName)) do
    inc(i);

  if (i < Application.ComponentCount) then begin
    GetControl((Application.Components[i] as TForm));
    if assigned(Result) and (Result is TMidiAwareControl) then begin
      (Result as TMidiAwareControl).MidiEditorAssignmentList := FMidiEditorAssignmentList;
    end;
  end;
end;

procedure TfrmMidiMapping.AddMidiEditorAssignment( aControl: TMidiAwareControl; aMidiChannel, aMidiNote, aMidiCC, aControlIndex: byte);
var i : integer;
    MidiEditorAssignment : TListItemMidiEditorAssignment;
    G2 : TG2Midi;
begin
  if FMidiEditorAssignmentList.Count >= MAX_CTRL_MIDI_ASSIGNMENTS then
    raise Exception.Create('Max number of midi ctrl assignments reached.');

  G2 := frmG2Main.FirstG2;

  if aMidiNote <> 0 then begin
    MidiEditorAssignment := FMidiEditorAssignmentList.FindControlNote( aControl, aMidiChannel, aMidiNote) as TListItemMidiEditorAssignment;
    if not assigned(MidiEditorAssignment) then begin
      MidiEditorAssignment := TListItemMidiEditorAssignment.Create(G2);
      with MidiEditorAssignment do begin
        Channel        := aMidiChannel;
        Note           := aMidiNote;
        ControlIndex   := aControlIndex;
        CC             := aMidiCC;
        MinValue       := 0;
        MaxValue       := 127;
        Control        := aControl;
        MidiOutputList := FCtrlMidiOutputList;
        ControlPath    := aControl.GetCntrlPath;
      end;
      FMidiEditorAssignmentList.Add( MidiEditorAssignment);
      aControl.MidiEditorAssignmentList := FMidiEditorAssignmentList;
    end;
  end else
    if aMidiCC <> 0 then begin
      MidiEditorAssignment := FMidiEditorAssignmentList.FindControlCC( aControl, aMidiChannel, aMidiCC) as TListItemMidiEditorAssignment;
      if not assigned(MidiEditorAssignment) then begin
        MidiEditorAssignment := TListItemMidiEditorAssignment.Create(G2);
        with MidiEditorAssignment do begin
          Channel        := aMidiChannel;
          Note           := aMidiNote;
          ControlIndex   := aControlIndex;
          CC             := aMidiCC;
          MinValue       := 0;
          MaxValue       := 127;
          Control        := aControl;
          MidiOutputList := FCtrlMidiOutputList;
          ControlPath    := aControl.GetCntrlPath;
        end;
        FMidiEditorAssignmentList.Add( MidiEditorAssignment);
        aControl.MidiEditorAssignmentList := FMidiEditorAssignmentList;
      end;
    end;
end;

procedure TfrmMidiMapping.UpdateCtrlMidiOutputList;
var i, j : integer;
begin
  // Build midi out device list for midi aware controls

  // Close and delete unchecked devices
  i := 0;
  while (i < FCtrlMidiOutputList.Count) do begin
    j := 0;
    while (j < frmSettings.clbCtrlMidiOutDevices.Items.Count) and not(frmSettings.clbCtrlMidiOutDevices.Items.Objects[j] = FCtrlMidiOutputList.Items[i]) do
      inc(j);

    if not(j < frmSettings.clbCtrlMidiOutDevices.Items.Count) then begin
      FCtrlMidiOutputList.Delete(i);
    end else begin
      // Found, but check if the device is selected
      if not frmSettings.clbCtrlMidiOutDevices.Checked[j] then begin
        FCtrlMidiOutputList.Delete(i);
      end else
        inc(i);
    end;
  end;

  // Add new checked and enabled devices
  for i := 0 to frmSettings.clbCtrlMidiOutDevices.Items.Count - 1 do begin
    if (frmSettings.clbCtrlMidiOutDevices.Checked[i])
        and (frmSettings.clbCtrlMidiOutDevices.ItemEnabled[i])
        and assigned(frmSettings.clbCtrlMidiOutDevices.Items.Objects[i])
        and (FCtrlMidiOutputList.Find( TMidiOutDevice(frmSettings.clbCtrlMidiOutDevices.Items.Objects[i]).MidiOutput) = -1) then begin
      j := FCtrlMidiOutputList.Add( TMidiOutDevice(frmSettings.clbCtrlMidiOutDevices.Items.Objects[i]).MidiOutput);
    end;
  end;
end;

procedure TfrmMidiMapping.UpdateList;
var i : integer;
    ListItem : TListItem;
begin
  lvEditorMidiAssignments.Clear;
  for i := 0 to FMidiEditorAssignmentList.Count - 1 do begin
    ListItem := lvEditorMidiAssignments.Items.Add;

    (FMidiEditorAssignmentList[i] as TListItemMidiEditorAssignment).WriteValues( ListItem);
    ListItem.Data := FMidiEditorAssignmentList[i];
  end;
end;

procedure TfrmMidiMapping.DoCtrlMidiInput(Sender: TObject);
begin
  if Sender is TMidiInput then
    ProcessCtrlMidiMessage( Sender as TMidiInput);
end;

procedure TfrmMidiMapping.PopupMenu( aControl: TMidiAwareControl; X, Y: integer);
var i : integer;
    MidiEditorAssignment : TMidiEditorAssignment;
    MidiMessage, MidiValue1, Channel : byte;
    miCtrlMidiAssignment : TMenuItem;
    AlreadyAssigned : boolean;
begin
  if not assigned(aControl) then
    exit;

  FCurrentControl := aControl;
  if aControl.IndexedControl then
    FCurrentIndex := aControl.GetValue
  else
    FCurrentIndex := 0;

  AlreadyAssigned := False;
  MidiMessage := FLastMidiEvent.MidiMessage shr 4;
  MidiValue1 := FLastMidiEvent.Data1;
  Channel := FLastMidiEvent.MidiMessage and $0F;

  puCtrlAssignMidi.Items.Clear;
  for i := 0 to FMidiEditorAssignmentList.Count - 1 do begin
    if (FMidiEditorAssignmentList[i].Control = aControl) and (FMidiEditorAssignmentList[i].ControlIndex = FCurrentIndex) then begin
      miCtrlMidiAssignment := TMenuItem.Create(self);
      miCtrlMidiAssignment.Tag := i;
      if (FMidiEditorAssignmentList[i].Note <> 0) then begin
        miCtrlMidiAssignment.Caption := 'Deassign Ctrl Midi N' + IntToStr(FMidiEditorAssignmentList[i].Note);
        if ((MidiMessage = $09) or ((MidiMessage = $08))) and (FMidiEditorAssignmentList[i].Note = MidiValue1) then
          AlreadyAssigned := True;
      end else begin
        miCtrlMidiAssignment.Caption := 'Deassign Ctrl Midi C' + IntToStr(FMidiEditorAssignmentList[i].CC);
        if (MidiMessage = $0B) and (FMidiEditorAssignmentList[i].CC = MidiValue1) then
          AlreadyAssigned := True;
      end;
      miCtrlMidiAssignment.OnClick := DeassignCtrlMidi;
      puCtrlAssignMidi.Items.Add(miCtrlMidiAssignment);
    end;
  end;

  if not AlreadyAssigned then begin
    if MidiMessage = $09 then begin
      MidiEditorAssignment := FMidiEditorAssignmentList.FindControlNote( aControl, Channel, MidiValue1);
      if not assigned(MidiEditorAssignment) then begin
        miCtrlMidiAssignment := TMenuItem.Create(self);
        miCtrlMidiAssignment.Caption := 'Assign Ctrl to Note ' + IntToStr( FLastMidiEvent.Data1);
        miCtrlMidiAssignment.OnClick := AssignCtrlMidi;
        puCtrlAssignMidi.Items.Add(miCtrlMidiAssignment);
      end;
    end else
      if MidiMessage = $0B then begin
        MidiEditorAssignment := FMidiEditorAssignmentList.FindControlCC( aControl, Channel, MidiValue1);
        if not assigned(MidiEditorAssignment) then begin
        miCtrlMidiAssignment := TMenuItem.Create(self);
          miCtrlMidiAssignment.Caption := 'Assign Ctrl to CC ' + IntToStr( FLastMidiEvent.Data1);
          miCtrlMidiAssignment.OnClick := AssignCtrlMidi;
          puCtrlAssignMidi.Items.Add(miCtrlMidiAssignment);
        end;
      end;
  end;

  if puCtrlAssignMidi.Items.Count > 0 then
    puCtrlAssignMidi.Popup( X, Y);
end;

procedure TfrmMidiMapping.AssignCtrlMidi(Sender: TObject);
var MidiMessage, Channel : byte;
begin
  MidiMessage := FLastMidiEvent.MidiMessage shr 4;
  Channel := FLastMidiEvent.MidiMessage and $0F;

  // Is it a note or a cc?
  if MidiMessage = $09 then begin
    // Note (= digital : on/off)
    AddMidiEditorAssignment( FCurrentControl,
                             Channel,
                             FLastMidiEvent.Data1,
                             0,
                             FCurrentIndex);
  end else begin
    if MidiMessage = $0B  then begin
      // CC (= range : 0..127)
      AddMidiEditorAssignment( FCurrentControl,
                               Channel,
                               0,
                               FLastMidiEvent.Data1,
                               FCurrentIndex);
    end;
  end;
end;

procedure TfrmMidiMapping.DeassignCtrlMidi(Sender: TObject);
var i, j : integer;
begin
  if Sender is TMenuItem then begin
    i := (Sender as TMenuItem).Tag;
    if (i>=0) and (i< FMidiEditorAssignmentList.Count) then begin

      // Other midi assignments to this control?
      j := 0;
      while (j<FMidiEditorAssignmentList.Count) and
                  not((FMidiEditorAssignmentList.Items[i].Control = FMidiEditorAssignmentList.Items[j].Control) and (j<>i)) do
        inc(j);

      if not(j<FMidiEditorAssignmentList.Count) then
        FMidiEditorAssignmentList.Items[i].Control.MidiEditorAssignmentList := nil;

      FMidiEditorAssignmentList.Delete( i);
    end;
  end;
end;

procedure TfrmMidiMapping.ProcessCtrlMidiMessage( aCtrlMidiInput : TMidiInput);
var	thisEvent : TMyMidiEvent;
    MidiMessage, Channel : byte;
    i, m : integer;
    Param : TG2FileParameter;
begin
  while (aCtrlMidiInput.MessageCount > 0) do begin

    { Get the event as an object }
    thisEvent := aCtrlMidiInput.GetMidiEvent;
    try
      if thisEvent.Sysex = nil then begin
        FLastMidiEvent.MidiMessage := thisEvent.MidiMessage;
        FLastMidiEvent.Data1 := thisEvent.Data1;
        FLastMidiEvent.Data2 := thisEvent.Data2;
        FLastMidiEvent.Time := thisEvent.Time;

        MidiMessage := FLastMidiEvent.MidiMessage shr 4;
        Channel := FLastMidiEvent.MidiMessage and $0F;

        if MidiMessage = $09 then begin
          i := 0;
          while (i<FMidiEditorAssignmentList.Count) do begin
            if FMidiEditorAssignmentList[i].Note = thisEvent.Data1 then begin
              FMidiEditorAssignmentList[i].Control.SetValueByCtrlMidi( FMidiEditorAssignmentList[i], FLastMidiEvent);
            end;
            inc(i);
          end;

        end else
          if MidiMessage = $08 then begin
            i := 0;
            while (i<FMidiEditorAssignmentList.Count) do begin
              if FMidiEditorAssignmentList[i].Note = thisEvent.Data1 then begin
                FMidiEditorAssignmentList[i].Control.SetValueByCtrlMidi( FMidiEditorAssignmentList[i], FLastMidiEvent);
              end;
              inc(i);
            end;

          end else
            if MidiMessage = $0B then begin

              m := aCtrlMidiInput.GetMidiMessageInBufferCount( thisEvent);

              i := 0;
              while (i<FMidiEditorAssignmentList.Count) do begin
                if FMidiEditorAssignmentList[i].CC = thisEvent.Data1 then begin
                  Param := FMidiEditorAssignmentList[i].Control.GetParameter;
                  if assigned(Param) then begin
                    if Param.ParamType = ptParam then
                      FMidiEditorAssignmentList[i].Control.SetValueByCtrlMidi( FMidiEditorAssignmentList[i], FLastMidiEvent)
                    else
                      // Limit number of midi messages for non-responseless usb messages
                      if m <= 2 then
                       FMidiEditorAssignmentList[i].Control.SetValueByCtrlMidi( FMidiEditorAssignmentList[i], FLastMidiEvent);
                  end else
                    FMidiEditorAssignmentList[i].Control.SetValueByCtrlMidi( FMidiEditorAssignmentList[i], FLastMidiEvent)
                end;
                inc(i);
              end;
            end;
      end;
    finally
      thisEvent.Free;
    end;
  end;
end;

{ TListItemMidiEditorAssignment }

constructor TListItemMidiEditorAssignment.Create( aG2 : TG2Midi);
begin
  inherited;
end;

destructor TListItemMidiEditorAssignment.Destroy;
begin
  inherited;
end;

procedure TListItemMidiEditorAssignment.ReadValues(aListItem: TListItem);
begin
  while aListItem.SubItems.Count < 6 do
    aListItem.SubItems.Add('');

  ControlPath := aListItem.Caption;
  ControlIndex := StrToInt(aListItem.SubItems[0]);
  Channel := StrToInt(aListItem.SubItems[1]);
  Note := StrToInt(aListItem.SubItems[2]);
  CC := StrToInt(aListItem.SubItems[3]);
  MinValue := StrToInt(aListItem.SubItems[4]);
  MaxValue := StrToInt(aListItem.SubItems[5]);
end;

procedure TListItemMidiEditorAssignment.WriteValues(aListItem: TListItem);
var i : integer;
begin
  while aListItem.SubItems.Count < 6 do
    aListItem.SubItems.Add('');

  aListItem.Caption := ControlPath;
  aListItem.SubItems[0] := IntToStr(ControlIndex);
  aListItem.SubItems[1] := IntToStr(Channel);
  aListItem.SubItems[2] := IntToStr(Note);
  aListItem.SubItems[3] := IntToStr(CC);
  aListItem.SubItems[4] := IntToStr(MinValue);
  aListItem.SubItems[5] := IntToStr(MaxValue);
end;

end.

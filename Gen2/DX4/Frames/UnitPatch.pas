unit UnitPatch;
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.ListBox,
  BVE.NMG2File, BVE.NMG2ControlsFMX, BVE.NMG2GraphFMX,
  UnitAppSettings;
type
  {TTreeViewItemExpanded = class(TTreeViewItem)
  private
    FOnChangeExpanded: TNotifyEvent;
  protected
    procedure ApplyStyle;override;
    procedure DoChangeExpanded(Sender: TObject);
  published
    property OnChangeExpanded: TNotifyEvent read FOnChangeExpanded write FOnChangeExpanded;
  end;}

  TframePatch = class(TFrame)
    Rectangle3: TRectangle;
    btLoadPatch: TG2BtnText;
    btSavePatch: TG2BtnText;
    btLoadPerf: TG2BtnText;
    btSavePerf: TG2BtnText;
    btInitPatch: TG2BtnText;
    btRefresh: TG2BtnText;
    Layout1: TLayout;
    lDirs: TLayout;
    lbFiles: TListBox;
    procedure btLoadPatchChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSavePatchChangeValue(Sender: TObject; const aValue: Integer);
    procedure btLoadPerfChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSavePerfChangeValue(Sender: TObject; const aValue: Integer);
    //procedure tvFilesChange(Sender: TObject);
    procedure btInitPatchChangeValue(Sender: TObject; const aValue: Integer);
    procedure btRefreshChangeValue(Sender: TObject; const aValue: Integer);
    //procedure tvFilesDblClick(Sender: TObject);
    procedure lbFilesChange(Sender: TObject);
    procedure lbFilesDblClick(Sender: TObject);
    procedure lbFilesKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    [Weak] FSynth : TG2GraphFMX;
    [Weak] FFrameAppSettings : TframeAppSettings;
    FRootDir : string;
    FBtnHeight : single;
    FDirs : TStringList;
    FLocateFile : string;
    //function GetNodeDir( aNode : TTreeViewItem): string;
    //function GetNodeFilename( aNode : TTreeViewItem) : string;
    procedure SetSynth(const Value: TG2GraphFMX);
  public
    constructor Create( AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFileStream(aFilename: string);
    procedure SavePatch(const aFilename : string);
    procedure SavePerf(const aFilename : string);
    function CurrentDir : string;
    procedure AddDir( aDir : string);
    procedure DeleteFromDir( aDir : string);
    procedure DeleteAllDirs;
    procedure OnDirBtnClick( Sender : TObject);
    procedure ReadDir;
    procedure DownDir( aDir : string);
    procedure UpDir( aDir : string);
    //procedure TreeNodeExpand( Sender : TObject);
    //procedure ReadDir( aNode : TFMXObject; aPath : string);
    procedure UpdateFiles;
    //procedure RefreshFiles( aNode : TTreeViewItem);
    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);
    property frameAppSettings : TframeAppSettings read FframeAppSettings write FframeAppSettings;
    property Synth : TG2GraphFMX read FSynth write SetSynth;
  end;
implementation
uses
  FMX.Ani;
{$R *.fmx}
constructor TframePatch.Create(AOwner: TComponent);
begin
  inherited;
  FBtnHeight := 22;

  FDirs := TStringList.Create;
  FRootDir := '';

  //FRootDir := 'C:';
  //AddDir( FRootDir);
  //ReadDir;

  //tvFiles.AniCalculations.Animation := True;
  //tvFiles.AniCalculations.BoundsAnimation := True;
  //tvFiles.AniCalculations.TouchTracking := [ttVertical];
end;

destructor TframePatch.Destroy;
begin
  FDirs.Free;
  inherited;
end;

procedure TframePatch.AddDir(aDir: string);
var Btn : TButton;
begin
  Btn := TButton.Create(self);
  Btn.StyledSettings := [];
  Btn.Font.Family := 'Roboto';
  Btn.Font.Size := 12;
  Btn.Text := aDir;
  Btn.OnClick := OnDirBtnClick;
  Btn.Parent := lDirs;
  Btn.Height := FBtnHeight;
  Btn.Position.Y := FDirs.Count * FBtnHeight;
  Btn.Align := TAlignLayout.alTop;
  FDirs.AddObject( aDir, Btn);
  lDirs.Height := FDirs.Count * FBtnHeight;
end;

procedure TframePatch.DeleteAllDirs;
begin
  FLocateFile := '';
  while FDirs.Count > 0 do begin
    FLocateFile := FDirs[ FDirs.Count - 1];
    FDirs.Objects[FDirs.Count - 1].Free;
    FDirs.Delete( FDirs.Count - 1);
  end;
  lDirs.Height := FDirs.Count * FBtnHeight;
end;

procedure TframePatch.DeleteFromDir(aDir: string);
var i : integer;
begin
  FLocateFile := '';
  i := FDirs.IndexOf( aDir);
  while FDirs.Count > (i + 1) do begin
    FLocateFile := FDirs[ FDirs.Count - 1];
    FDirs.Objects[FDirs.Count - 1].Free;
    FDirs.Delete( FDirs.Count - 1);
  end;
  lDirs.Height := FDirs.Count * FBtnHeight;
end;

procedure TframePatch.DownDir(aDir: string);
begin
  AddDir( aDir);
  ReadDir;
  if lbFiles.Count > 0 then
    lbFiles.ItemIndex := 0;
end;

procedure TframePatch.UpDir( aDir: string);
begin
  DeleteFromDir( aDir);
  ReadDir;
end;

procedure TframePatch.OnDirBtnClick(Sender: TObject);
begin
  if Sender is TButton then
    UpDir((Sender as TButton).Text);
end;

function TframePatch.CurrentDir: string;
var i : integer;
begin
  Result := '';
  for i := 0 to FDirs.Count - 1 do
    Result := Result + FDirs[i] + System.IOUtils.TPath.DirectorySeparatorChar;
end;

procedure TframePatch.ReadDir;
var SearchOption : TSearchOption;
    Predicate : TDirectory.TFilterPredicate;
    ListBoxItem : TListBoxItem;
    Path : string;
    Files : TStringDynArray;
    i : integer;
begin
  Path := CurrentDir;

  if trim(Path) = '' then
    exit;


  SearchOption := TSearchOption.soTopDirectoryOnly;

  Predicate := function(const Path: string; const SearchRec: TSearchRec): Boolean
                     begin
                       Result := (SearchRec.Attr and faHidden)=0;
                     end;

  lbFiles.BeginUpdate;
  try
    lbFiles.Items.Clear;

    Files := System.IOUtils.TDirectory.GetDirectories( Path, '*', SearchOption, Predicate);
    for i := 0 to Length(Files)-1 do begin
      ListBoxItem := TListBoxItem.Create(lbFiles);
      ListBoxItem.StyledSettings := [];
      ListBoxItem.Font.Family := 'Roboto';
      ListBoxItem.Font.Size := 12;
      ListBoxItem.Text := ExtractFilename(Files[i]);

      // (aNone=0, aMore=1, aDetail=2, aCheckmark=3)
      ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(1);
      ListBoxItem.Tag := 1;
      lbFiles.AddObject(ListBoxItem);
    end;

    Files := System.IOUtils.TDirectory.GetFiles( Path, '*', Predicate);
    for i := 0 to Length(Files)-1 do begin
      ListBoxItem := TListBoxItem.Create(lbFiles);
      ListBoxItem.StyledSettings := [];
      ListBoxItem.Font.Family := 'Roboto';
      ListBoxItem.Font.Size := 12;
      ListBoxItem.Text := ExtractFilename(Files[i]);
      // (aNone=0, aMore=1, aDetail=2, aCheckmark=3)
      ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(0);
      ListBoxItem.Tag := 0;
      lbFiles.AddObject(ListBoxItem);
    end;
  finally
    lbFiles.EndUpdate;
  end;

  if FLocateFile <> '' then begin
    i := lbFiles.Items.IndexOf( FLocateFile);
    if i <> - 1 then
      lbFiles.ItemIndex := i;
  end;
end;

procedure TframePatch.UpdateFiles;
begin
  if FRootDir <> FframeAppSettings.PatchDir then begin
    DeleteAllDirs;
    FRootDir := FframeAppSettings.PatchDir;
    if System.IOUtils.TDirectory.Exists(FRootDir) then begin
      AddDir( FRootDir);
      ReadDir;
    end;
  end else
    ReadDir;
end;


procedure TframePatch.lbFilesChange(Sender: TObject);
var Item : TListBoxItem;
    Ext : string;
begin
  Item := lbFiles.Selected;
  if assigned(Item) and (Item.Tag = 0) then begin
    Ext := Lowercase( ExtractFileExt(Item.Text));
    if (Ext = '.prf2') then begin
      btLoadPerf.State := csDefault;
    end else begin
      btLoadPerf.State := csDisabled;
    end;
    if (Ext = '.pch2') then begin
      btLoadPatch.State := csDefault;
    end else begin
      btLoadPatch.State := csDisabled;
    end;
  end;
end;

procedure TframePatch.lbFilesDblClick(Sender: TObject);
var Item : TListBoxItem;
    filename : string;
begin
  Item := lbFiles.Selected;

  if assigned(Item) then begin
    case Item.Tag of
    0 : begin
          filename := CurrentDir + Item.Text;
          LoadFilestream(filename);
        end;
    1 : begin
          if lbFiles.ItemIndex <> -1 then
            DownDir( lbFiles.Items[ lbFiles.ItemIndex]);
        end;
    end;
  end;
end;


procedure TframePatch.lbFilesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
  vkLeft :
    begin
      if FDirs.Count >= 2 then
        UpDir( FDirs[ FDirs.Count - 2]);
      Key := 0;
    end;
  vkRight :
    begin
      if assigned(lbFiles.Selected) and (lbFiles.Selected.Tag = 1) then begin
        DownDir( lbFiles.Items[ lbFiles.ItemIndex]);
      end;
      Key := 0;
    end;
  vkReturn :
    begin
      lbFilesDblClick(self);
    end;
  end;
end;

{procedure TframePatch.ReadDir(aNode: TFMXObject; aPath: string);
var sr : TSearchRec;
    ChildNode : TTreeViewItemExpanded;
    Ext : string;
    i : integer;
    sl : TStringList;
    ItemsDeleted : boolean;
    function FindChild( aName : string): TTreeViewItem;
    var j : integer;
    begin
      if aNode is TTreeView then begin
        j := 0;
        while (j<(aNode as TTreeView).Count) and not((aNode as TTreeView).Items[j].Text = sr.Name) do
          inc(j);
         if j<(aNode as TTreeView).Count then
           Result := (aNode as TTreeView).Items[j]
         else
           Result := nil;
      end else begin
        j := 0;
        while (j<(aNode as TTreeViewItem).Count) and not((aNode as TTreeViewItem).Items[j].Text = sr.Name) do
          inc(j);
         if j<(aNode as TTreeViewItem).Count then
           Result := (aNode as TTreeViewItem).Items[j]
         else
           Result := nil;
      end;
    end;
begin
  if aPath.Length = 0 then
    exit;
  if assigned(aNode) and (aNode is TTreeViewItemExpanded)
     and ((aNode as TTreeViewItemExpanded).ParentItem <> nil)
     and not((aNode as TTreeViewItemExpanded).ParentItem.IsExpanded) then
    exit;
  sl := TStringList.Create;
  try
    if aPath.Chars[aPath.Length - 1] <> PathDelim then
      aPath := aPath + PathDelim;
    if FindFirst( aPath + '*.*', faAnyFile, sr) = 0 then begin
      // Remove what doesn't exits anymore, make quick list
      repeat
        if (sr.Attr and faDirectory) = 0 then begin
          Ext := Lowercase(ExtractFileExt(sr.Name));
          if (Ext = '.prf2') or (Ext = '.pch2') then begin
            sl.Add(sr.Name);
          end;
        end else begin
          if (sr.Name = '.') or (sr.Name = '..') then begin
          end else begin
            sl.Add(sr.Name);
          end;
        end;
      until (FindNext(sr) <> 0);
      FindClose(sr);
      if aNode is TTreeview then begin
        ItemsDeleted := False;
        i := 0;
        while (i < (aNode as TTreeView).Count) do begin
          ChildNode := (aNode as TTreeView).Items[i] as TTreeViewItemExpanded;
          if sl.IndexOf(ChildNode.Text) = -1 then begin
            (aNode as TTreeView).RemoveObject(ChildNode);
            ChildNode.DisposeOf;
            ItemsDeleted := True;
          end else
            inc(i);
        end;
        if ItemsDeleted then
          (aNode as TTreeView).RealignContent;
      end else begin
        ItemsDeleted := False;
        i := 0;
        while (i < (aNode as TTreeViewItem).Count) do begin
          ChildNode := (aNode as TTreeViewItem).Items[i] as TTreeViewItemExpanded;;
          if sl.IndexOf(ChildNode.Text) = -1 then begin
            (aNode as TTreeViewItem).RemoveObject(ChildNode);
            ChildNode.DisposeOf;
            ItemsDeleted := True;
          end else
            inc(i);
        end;
        if ItemsDeleted then
          (aNode as TTreeViewItem).TreeView.RealignContent;
      end;
    end;
    if FindFirst( aPath + '*.*', faAnyFile, sr) = 0 then begin
      repeat
        if (sr.Attr and faDirectory) = 0 then begin
          Ext := Lowercase(ExtractFileExt(sr.Name));
          if (Ext = '.prf2') or (Ext = '.pch2') then begin
            ChildNode := FindChild(sr.Name) as TTreeViewItemExpanded;
            if not assigned(ChildNode) then begin
              ChildNode := TTreeViewItemExpanded.Create(self);
              ChildNode.Parent := aNode;
              ChildNode.Text := sr.Name;
              ChildNode.TagObject := TObject(sr.Attr);
            end;
          end;
        end else begin
          if (sr.Name = '.') or (sr.Name = '..') then begin
          end else begin
            ChildNode := FindChild(sr.Name) as TTreeViewItemExpanded;
            if not assigned(ChildNode) then begin
              ChildNode := TTreeViewItemExpanded.Create(self);
              ChildNode.Parent := aNode;
              ChildNode.Text := sr.Name;
              ChildNode.OnChangeExpanded := TreeNodeExpand;
              ChildNode.TagObject := TObject(sr.Attr);
            end;
            ReadDir( ChildNode, aPath + sr.Name);
          end;
        end;
      until (FindNext(sr) <> 0);
      FindClose(sr);
    end;
  finally
    sl.Free;
  end;
end;
function TframePatch.GetNodeDir( aNode : TTreeViewItem): string;
var Attr : integer;
    BasePath : string;
begin
  Result := '';
  while assigned(aNode) do begin
    Attr := integer(aNode.TagObject);
    if (Attr and faDirectory) = faDirectory then
      Result := aNode.Text + PathDelim + Result;
    aNode := aNode.ParentItem;
  end;
  BasePath := FframeAppSettings.PatchDir;
  if BasePath.Chars[BasePath.Length - 1] <> PathDelim then
    BasePath := BasePath + PathDelim;
  Result := BasePath + Result;
end;
function TframePatch.GetNodeFilename( aNode : TTreeViewItem): string;
var Attr : integer;
    BasePath : string;
begin
  Result := '';
  if not assigned(aNode)  then
    exit;
  Result := GetNodeDir(aNode) + aNode.Text;
end;

procedure TframePatch.TreeNodeExpand(Sender: TObject);
begin
  if tvFiles.IsUpdating then
    Exit;

  if not (Sender as TTreeViewItem).IsExpanded then
    exit;
  tvFiles.BeginUpdate;
  try
    ReadDir( Sender as TTreeViewItem, GetNodeDir( Sender as TTreeViewItem));
  finally
    tvFiles.EndUpdate;
    tvFiles.Repaint;
  end;
end;
procedure TframePatch.UpdateFiles;
begin
  tvFiles.BeginUpdate;
  try
    ReadDir( tvFiles, FframeAppSettings.PatchDir);
  finally
    tvFiles.EndUpdate;
    tvFiles.Repaint;
  end;
end;
procedure TframePatch.RefreshFiles( aNode : TTreeViewItem);
var Attr : integer;
begin
  if not assigned(aNode) then
    UpdateFiles
  else begin
    tvFiles.BeginUpdate;
    try
      Attr := integer(aNode.TagObject);
      if (Attr and faDirectory) = faDirectory then begin
        ReadDir( aNode, GetNodeDir(aNode))
      end else begin
        if aNode.ParentItem <> nil then
          ReadDir( aNode.ParentItem, GetNodeDir(aNode))
        else
          ReadDir( tvFiles, FframeAppSettings.PatchDir);
      end;
    finally
      tvFiles.EndUpdate;
      tvFiles.Repaint;
    end;
  end;
end;}
procedure TframePatch.LoadFileStream(aFilename: string);
var G2FileDataStream : TG2FileDataStream;
    DataName : string;
    i : integer;
    FileStream : TFileStream;
    Lines : TStrings;
    b : byte;
begin
  if not assigned(Synth) then
    exit;
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    aFilename := ExtractFilename( aFileName);
    // Name patch max size = 16, if shorter end with 0
    DataName := '';
    i := 1;
    while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
      DataName := DataName +Char(byte(aFileName[i]));
      inc(i);
    end;
    Lines := nil;
    if assigned(Synth.LogLines) then
      Lines := Synth.LogLines;
    // Check first byte
    FileStream.Read( b, 1);
    FileStream.Position := 0;
    case b of
      $56 : G2FileDataStream := TG2FileDataStream.LoadFileData( self, FileStream, Lines);
      $F0 : G2FileDataStream := TG2FileDataStream.LoadMidiData( self, FileStream, Lines);
      else
        raise Exception.Create('Unknown file data.');
    end;
    if G2FileDataStream is TG2FilePerformance then begin
      (Synth.Performance as TG2GraphPerformanceFMX).SendSetPerformanceMessage( DataName, G2FileDataStream as TG2FilePerformance);
    end else
      if G2FileDataStream is TG2FilePatch then
        Synth.SelectedSlot.SendSetPatchMessage( DataName, G2FileDataStream as TG2FilePatch)
      else
        raise Exception.Create('Unknown data type');
  finally
    FileStream.Free;
  end;
end;
procedure TframePatch.SavePatch(const aFilename : string);
var WriteStream : TFileStream;
begin
  if not assigned(Synth) then
    exit;
{$IFDEF FPC}
  if FileExistsUTF8(aFileName) { *Converted from FileExists*  } then begin
{$ELSE}
  if FileExists(aFileName) then begin
{$ENDIF}
    if MessageDlg('Overwrite existing file?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
      exit;
{$IFDEF FPC}
    DeleteFileUTF8(aFileName); { *Converted from DeleteFile*  }
{$ELSE}
    DeleteFile(aFileName);
{$ENDIF}
  end;
  WriteStream := TFileStream.Create(aFileName, fmCreate);
  try
    Synth.SelectedSlot.Patch.SaveToFile(WriteStream);
  finally
    WriteStream.Free;
  end;
  //RefreshFiles(tvFiles.Selected);
  ReadDir;
end;
procedure TframePatch.SavePerf(const aFilename : string);
var WriteStream : TFileStream;
begin
  if not assigned(Synth) then
    exit;
{$IFDEF FPC}
  if FileExistsUTF8(aFileName) { *Converted from FileExists*  } then begin
{$ELSE}
  if FileExists(aFileName) then begin
{$ENDIF}
    if MessageDlg('Overwrite existing file?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
      exit;
{$IFDEF FPC}
    DeleteFileUTF8(aFileName); { *Converted from DeleteFile*  }
{$ELSE}
    DeleteFile(aFileName);
{$ENDIF}
  end;
  WriteStream := TFileStream.Create(aFileName, fmCreate);
  try
    Synth.Performance.SaveToFile(WriteStream);
  finally
    WriteStream.Free;
  end;
  //RefreshFiles(tvFiles.Selected);

  ReadDir;

end;
procedure TframePatch.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;
procedure TframePatch.SetSynth(const Value: TG2GraphFMX);
begin
  FSynth := Value;
end;
{

rocedure TframePatch.tvFilesChange(Sender: TObject);
var Item : TTreeViewItem;
    Ext : string;
begin
  Item := tvFiles.Selected;
  if assigned(Item) then begin
    Ext := Lowercase(ExtractFileExt(Item.Text));
    //if Ext = '.pchlist' then begin
    //  btLoadBank.State := csDefault
    //end else
    //  btLoadBank.State := csDisabled;
    if (Ext = '.prf2') then begin
      btLoadPerf.State := csDefault;
    end else begin
      btLoadPerf.State := csDisabled;
    end;
    if (Ext = '.pch2') then begin
      btLoadPatch.State := csDefault;
    end else begin
      btLoadPatch.State := csDisabled;
    end;
  end;
end;}
{procedure TframePatch.tvFilesDblClick(Sender: TObject);
var filename : string;
begin
  filename := GetNodeFilename(tvFiles.Selected);
  if filename <> '' then
    LoadFilestream(filename);
end;}
procedure TframePatch.btInitPatchChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then begin
    FSynth.SelectedSlot.Patch.Init;
    FSynth.SelectedSlot.SendSetPatchMessage('No name', FSynth.SelectedSlot.Patch);
  end;
end;
procedure TframePatch.btLoadPatchChangeValue(Sender: TObject;
  const aValue: Integer);
var filename : string;
    Item : TListBoxItem;
begin
  if aValue = 0 then begin
    //filename := GetNodeFilename( tvFiles.Selected);
    //if filename <> '' then
    Item := lbFiles.Selected;
    if assigned(Item) and (Item.Tag = 0) then begin
      filename := CurrentDir + Item.Text;
      LoadFilestream(filename);
    end;
  end;
end;
procedure TframePatch.btLoadPerfChangeValue(Sender: TObject;
  const aValue: Integer);
var filename : string;
    Item : TListBoxItem;
begin
  if aValue = 0 then begin
    //filename := GetNodeFilename( tvFiles.Selected);
    Item := lbFiles.Selected;
    if assigned(Item) and (Item.Tag = 0) then begin
      filename := CurrentDir + Item.Text;
      LoadFilestream(filename);
    end;
  end;
end;
procedure TframePatch.btRefreshChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then begin
    UpdateFiles;
  end;
end;
procedure TframePatch.btSavePatchChangeValue(Sender: TObject;
  const aValue: Integer);
var filename : string;
begin
  if not assigned(Synth) then
    exit;
  if aValue = 0 then begin
    //filename := GetNodeDir(tvFiles.Selected) + Synth.SelectedSlot.PatchName + '.pch2';
    filename := CurrentDir + Synth.SelectedSlot.PatchName + '.pch2';
    SavePatch(filename);
  end;
end;
procedure TframePatch.btSavePerfChangeValue(Sender: TObject;
  const aValue: Integer);
var filename : string;
begin
  if not assigned(Synth) then
    exit;
  if aValue = 0 then begin
    //filename := GetNodeDir(tvFiles.Selected) + Synth.Performance.PerformanceName + '.prf2';
    filename := CurrentDir + Synth.Performance.PerformanceName + '.prf2';
    SavePerf(filename);
  end;
end;
// -----------------------------------------------------------------------------
//
// TTreeViewItemExpanded
// Trick for missing onexpand event: http://monkeystyler.com/blog
//
// -----------------------------------------------------------------------------
{procedure TTreeViewItemExpanded.ApplyStyle;
var Ani: TFloatAnimation;
  O: TFMXObject;
begin
  inherited;
  O := FindStyleResource('button');
  if Assigned(O) then
  begin
    Ani := TFloatAnimation.Create(O);
    Ani.Parent := O;
    Ani.Stored := False;
    Ani.StartValue := 0.999999999999;
    Ani.StopValue := 1;
    Ani.PropertyName := 'Opacity';
    Ani.Trigger := 'IsExpanded=true';
    Ani.TriggerInverse := 'IsExpanded=false';
    Ani.Duration := 0;
    Ani.OnFinish := DoChangeExpanded;
  end;
end;
procedure TTreeViewItemExpanded.DoChangeExpanded(Sender: TObject);
begin
  if Assigned(OnChangeExpanded) then
    OnChangeExpanded(Self);
end;}
end.

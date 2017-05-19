unit UnitG2EditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Math,
  FMX.Layouts, FMX.Objects, FMX.Menus, g2_types, g2_file, FMX.ExtCtrls,
  G2FMXGraph;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SmallScrollBar1: TSmallScrollBar;
    LayoutZoomFX: TLayout;
    Splitter1: TSplitter;
    ScrollboxVA: TScrollBox;
    LayoutZoomVA: TLayout;
    ScrollBoxFX: TScrollBox;
    LayoutVA: TG2FMXPatchArea;
    LayoutFX: TG2FMXPatchArea;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SmallScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FG2 : TG2Graph;
    FPatch : TG2GraphPatch;
    procedure InitPatchLocations;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


// === Form ====================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
  FG2 := TG2Graph.Create(self);
  FG2.PatchAreaVA := LayoutVA;
  FG2.PatchAreaFX := LayoutFX;
  FG2.LoadModuleDefs('');
  FG2.IsServer := True;
  FPatch := FG2.Performance.Slot[0].Patch as TG2GraphPatch;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FG2.Free;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var aPatch : TG2FilePatch;
    PatchName : string;
    i : integer;
    FileStream : TFileStream;
    aFileName : string;
begin
  if OpenDialog1.Execute then begin
    aFileName := OpenDialog1.FileName;

    FileStream := TFileStream.Create( aFileName, fmOpenRead);
    try
      // Take the patchname from the filename
      aFilename := ExtractFilename( aFileName);

      // Name patch max size = 16, if shorter end with 0
      PatchName := '';
      i := 1;
      while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
        PatchName := PatchName + aFileName[i];
        inc(i);
      end;

      {aPatch := TG2FilePatch.Create(FG2USB);
      try
        if aPatch.LoadFromFile(FileStream, FG2USB.LogLines.Lines) then
          USBUploadPatch(PatchName, aPatch);
      finally
        aPatch.Free;
      end;}
      FPatch.Init;
      FPatch.LoadFromFile(FileStream, nil);
      FPatch.Visible := True;

      InitPatchLocations;
    finally
      FileStream.Free;
    end;
  end;
end;


procedure TForm1.InitPatchLocations;
var i, max_col, max_row : integer;
begin
  max_col := 0;
  max_row := 0;
  for i := 0 to FPatch.ModuleCount[ord(ltVA)] - 1 do begin
    if FPatch.ModuleList[ord(ltVA)][i].Row > max_row then
      max_row := FPatch.ModuleList[ord(ltVA)][i].Row;

    if FPatch.ModuleList[ord(ltVA)][i].Col > max_col then
      max_col := FPatch.ModuleList[ord(ltVA)][i].Col;
  end;

  LayoutZoomVA.Position.X := 0;
  LayoutZoomVA.Position.Y := 0;
  LayoutVA.Position.X := 0;
  LayoutVA.Position.Y := 0;
  LayoutVA.Width := (max_col + 2) * UNITS_COL;
  LayoutVA.Height := (max_row + 6) * UNITS_ROW;
  LayoutZoomVA.Width := LayoutVa.Width * SmallScrollbar1.Value;
  LayoutZoomVA.Height := LayoutVa.Height * SmallScrollbar1.Value;

  max_col := 0;
  max_row := 0;
  for i := 0 to FPatch.ModuleCount[ord(ltFX)] - 1 do begin
    if FPatch.ModuleList[ord(ltFX)][i].Row > max_row then
      max_row := FPatch.ModuleList[ord(ltFX)][i].Row;

    if FPatch.ModuleList[ord(ltFX)][i].Col > max_col then
      max_col := FPatch.ModuleList[ord(ltFX)][i].Col;
  end;

  LayoutZoomFX.Position.X := 0;
  LayoutZoomFX.Position.Y := 0;
  LayoutFX.Position.X := 0;
  LayoutFX.Position.Y := 0;
  LayoutFX.Width := (max_col + 2) * UNITS_COL;
  LayoutFX.Height := (max_row + 6) * UNITS_ROW;
  LayoutZoomFX.Width := LayoutFX.Width * SmallScrollbar1.Value;
  LayoutZoomFX.Height := LayoutFX.Height * SmallScrollbar1.Value;

end;

procedure TForm1.SmallScrollBar1Change(Sender: TObject);
begin
  LayoutVa.Scale.X := SmallScrollbar1.Value;
  LayoutVa.Scale.Y := SmallScrollbar1.Value;
  LayoutZoomVA.Width := LayoutVa.Width * SmallScrollbar1.Value;
  LayoutZoomVA.Height := LayoutVa.Height * SmallScrollbar1.Value;

  LayoutFX.Scale.X := SmallScrollbar1.Value;
  LayoutFX.Scale.Y := SmallScrollbar1.Value;
  LayoutZoomFX.Width := LayoutVa.Width * SmallScrollbar1.Value;
  LayoutZoomFX.Height := LayoutVa.Height * SmallScrollbar1.Value;

end;

end.

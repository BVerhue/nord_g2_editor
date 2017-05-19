unit UnitFormGenTex;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  BVE.NMG2TexturesGL;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPathImg: string;
    FTextureList: TTextureListGL;

    procedure GenerateTextures;
  end;

var
  Form1: TForm1;

implementation
uses
  System.Zip,
  BVE.NMG2ControlsG2;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  GenerateTextures;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  FPathImg := '..\..\..\Img\';
{$ELSE}
  FPathImg := System.IOUtils.TPath.GetDocumentsPath + '/';
{$ENDIF}
end;

procedure TForm1.GenerateTextures;
begin
  FTextureList := TTextureListGL.Create(nil);
  try
    Memo1.Lines.Add('Creating textures...');
    Memo1.Lines.Add('');

    FTextureList.MaxSize := 100000000;
    CreateTextures(FTextureList, '', Memo1.Lines);
    CreateModulePreviews(FTextureList, Memo1.Lines);

    Memo1.Lines.Add('');
    Memo1.Lines.Add('Saving textures to ' + FPathImg );

    FTextureList.SaveTextures(FPathImg);
  finally
    FTextureList.Free;
  end;
end;

end.

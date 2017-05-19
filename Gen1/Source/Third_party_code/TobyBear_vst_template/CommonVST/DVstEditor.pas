unit DVstEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DsgnIntf, StdCtrls, ExtCtrls, Buttons, DVSTHost;

type
  TVSTHostEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): AnsiString; override;
    function GetVerbCount: Integer; override;
  end;

implementation

procedure TVSTHostEditor.Edit;
begin
  ExecuteVerb(0);
end;

function TVSTHostEditor.GetVerb(Index: Integer): AnsiString;
begin
  case Index of
    0: Result := 'Load PlugIn';
  end;
end;

function TVSTHostEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TVSTHostEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: begin
        ShowMessage('ToDo');
       end;
  end;
end;

end.

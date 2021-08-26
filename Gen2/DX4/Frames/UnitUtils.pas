unit UnitUtils;

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

{$I ..\Common\CompilerSettings.Inc}

interface

{$IFDEF MSWINDOWS}
uses
  Winapi.ShellAPI,
  Winapi.Windows;
{$ENDIF}
{$IFDEF POSIX}
uses
  Posix.Stdlib;
{$ENDIF}

type
  TUtils = class
    class procedure Open(sCommand: string);
  end;

implementation

class procedure TUtils.Open(sCommand: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(sCommand), '', '', SW_SHOWNORMAL);
  {$ENDIF}
  {$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(sCommand)));
  {$ENDIF}
end;

procedure ConvertXML;
{var m, ci, co, p, pm : integer;
    sl : TStringList;
    ModuleDef : TXMLModuleDefType;
    ConnectorType : TXMLConnectorType;
    ParamType : TXMLParamType;
    ParamDef : TXMLParamDefType;
  function allowedchar( aValue : string): string;
  var sb : TStringBuilder;
      i : integer;
  begin
    sb := TStringBuilder.Create;
    try
      for i := 0 to aValue.Length - 1 do
        if aValue.Chars[i] = '&' then
          sb.Append('and')
        else
          sb.Append(aValue.Chars[i]);
      Result := sb.ToString;
    finally
      sb.Free;
    end;
  end;}
begin
{  sl := TStringList.Create;
  try
    sl.Add('<?xml version="1.0"?>');
    sl.Add('<ModuleDefList xmlns="http://www.yourtargetnamespace.com" version="0.3">');
    for m := 0 to FG2.FModuleDeFlist.Count - 1 do  begin
      ModuleDef := FG2.FModuleDeFlist.ModuleDef[m] as TXMLModuleDefType;
      sl.Add('<ModuleDef');
      sl.Add('  ' + 'moduleType="' + IntToStr(ModuleDef.ModuleType) + '"');
      sl.Add('  ' + 'isLed="' + IntToStr(ModuleDef.IsLed) + '"');
      sl.Add('  ' + 'uprate="' + IntToStr(ModuleDef.Uprate) + '"');
      sl.Add('  ' + 'page="' + ModuleDef.Page + '"');
      sl.Add('  ' + 'pageIndex="' + IntToStr(ModuleDef.PageIndex) + '"');
      sl.Add('  ' + 'shortName="' + AllowedChar(ModuleDef.ShortName) + '"');
      sl.Add('  ' + 'longName="' + AllowedChar(ModuleDef.LongName) + '"');
      sl.Add('  ' + 'height="' + IntToStr(ModuleDef.Height) + '">');
      sl.Add('  ' + '<Inputs>');
      if assigned( ModuleDef.Inputs) and (ModuleDef.Inputs.Count > 0) then begin
        for ci := 0 to ModuleDef.Inputs.Count - 1 do begin
          ConnectorType := ModuleDef.Inputs[ci];
          sl.Add('  ' + '  ' + '<Connector');
          sl.Add('  ' + '  ' + '  ' + 'name="' + ConnectorType.Name + '"');
          sl.Add('  ' + '  ' + '  ' + 'type="' + ConnectorType.Type_ + '"/>');
        end;
      end;
      sl.Add('  ' + '</Inputs>');
      sl.Add('  ' + '<Outputs>');
      if assigned(ModuleDef.Outputs) and (ModuleDef.Outputs.Count > 0) then begin
        for co := 0 to ModuleDef.Outputs.Count - 1 do begin
          ConnectorType := ModuleDef.Outputs[co];
          sl.Add('  ' + '  ' + '<Connector');
          sl.Add('  ' + '  ' + '  ' + 'name="' + ConnectorType.Name + '"');
          sl.Add('  ' + '  ' + '  ' + 'type="' + ConnectorType.Type_ + '"/>');
        end;
      end;
      sl.Add('  ' + '</Outputs>');
      sl.Add('  ' + '<Params>');
      if assigned(ModuleDef.Params) and (ModuleDef.Params.Count > 0) then begin
        for p := 0 to ModuleDef.Params.Count - 1 do begin
          ParamType := ModuleDef.Params[p];
          sl.Add('  ' + '  ' + '<Param');
          sl.Add('  ' + '  ' + '  ' + 'name="' + ParamType.Name + '"');
          sl.Add('  ' + '  ' + '  ' + 'defaultValue="' + IntToStr(ParamType.DefaultValue) + '"');
          sl.Add('  ' + '  ' + '  ' + 'id="' + IntToStr(ParamType.Id) + '"');
          sl.Add('  ' + '  ' + '  ' + 'defaultKnob="' + IntToStr(ParamType.DefaultKnob) + '"');
          sl.Add('  ' + '  ' + '  ' + 'buttonParam="' + IntToStr(ParamType.ButtonParamIndex) + '"');
          sl.Add('  ' + '  ' + '  ' + 'paramLabel="' + ParamType.ParamLabel + '"/>');
        end;
      end;
      sl.Add('  ' + '</Params>');
      sl.Add('  ' + '<Modes>');
      if assigned(ModuleDef.Modes) and (ModuleDef.Modes.Count > 0) then begin
        for pm := 0 to ModuleDef.Modes.Count - 1 do begin
          ParamType := ModuleDef.Modes[pm];
          sl.Add('  ' + '  ' + '<Param');
          sl.Add('  ' + '  ' + '  ' + 'name="' + ParamType.Name + '"');
          sl.Add('  ' + '  ' + '  ' + 'defaultValue="' + IntToStr(ParamType.DefaultValue) + '"');
          sl.Add('  ' + '  ' + '  ' + 'id="' + IntToStr(ParamType.Id) + '"');
          sl.Add('  ' + '  ' + '  ' + 'defaultKnob="' + IntToStr(ParamType.DefaultKnob) + '"');
          sl.Add('  ' + '  ' + '  ' + 'buttonParam="' + IntToStr(ParamType.ButtonParamIndex) + '"');
          sl.Add('  ' + '  ' + '  ' + 'paramLabel="' + ParamType.ParamLabel + '"/>');
        end;
      end;
      sl.Add('  ' + '</Modes>');
      sl.Add('</ModuleDef>');
    end;
    sl.Add('</ModuleDefList>');
    sl.SaveToFile('ModuleDef_v3.xml');
    sl.Clear;
    sl.Add('<?xml version="1.0"?>');
    sl.Add('<ParamDefList xmlns="http://www.yourtargetnamespace.com" version="0.3">');
    for m := 0 to FG2.FParamDefList.Count - 1 do begin
      ParamDef := FG2.FParamDefList.ParamDef[m];
      sl.Add('  ' + '<ParamDef');
      sl.Add('  ' + '  ' + 'id="' + IntToStr(ParamDef.Id) + '"');
      sl.Add('  ' + '  ' + 'paramType="' + IntToStr(ParamDef.ParamType) + '"');
      sl.Add('  ' + '  ' + 'rangeType="' + ParamDef.RangeType + '"');
      sl.Add('  ' + '  ' + 'lowValue="' + IntToStr(ParamDef.LowValue) + '"');
      sl.Add('  ' + '  ' + 'highValue="' + IntToStr(ParamDef.HighValue) + '"');
      sl.Add('  ' + '  ' + 'defaultValue="' + IntToStr(ParamDef.DefaultValue) + '"');
      sl.Add('  ' + '  ' + 'definitions="' + ParamDef.Definitions + '"');
      sl.Add('  ' + '  ' + 'comments="' + ParamDef.Comments + '"');
      sl.Add('  ' + '  ' + 'buttonText="' + ParamDef.ButtonText + '"/>');
    end;
    sl.Add('</ParamDefList>');
    sl.SaveToFile('ParamDef_v3.xml');

  finally
    sl.Free;
  end;}
end;

end.

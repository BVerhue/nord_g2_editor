unit BVE.NMG2ControlsFMXIDE;

interface
uses
  System.SysUtils, System.Classes, FMX.Forms, System.UITypes,
  DesignEditors, DesignIntf,
  BVE.NMG2ControlsFMX;

procedure Register;

implementation


procedure Register;
begin
  RegisterClasses([TG2BtnArray,TG2BtnIncDec]);
  RegisterComponents('NMG2', [TBufferedLayout,
                              TArrayLayout,
                              TG2StateStyleList,
                              TG2Scope,
                              TG2LedGreen,
                              TG2Label,
                              TG2TextField,
                              TG2BtnText,
                              TG2BtnTextEdit,
                              TG2BtnFlat,
                              TG2BtnRadio,
                              TG2BtnRadioEdit,
                              TG2BtnIncDec,
                              TG2PartSelector,
                              TG2Knob,
                              TG2Connector,
                              TG2Cable,
                              TG2KeyBoard,
                              TG2Module]);
end;

end.

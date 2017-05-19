unit JawsCtrls;

// Jaws Accessibility Controls
//
// Reclass of some windowed controls, so Jaws will recognize them
// For some reason or another DEdit is recognized but TEdit isn't

interface
uses
  Classes, StdCtrls, ComCtrls;

type
  DEdit = class(TEdit);
  DListView = class(TListView);
  DCombobox = class(TCombobox);
  DUpDown = class(TUpDown);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NM G2', [DEdit, DCombobox, DListView, DUpDown]);
end;


end.

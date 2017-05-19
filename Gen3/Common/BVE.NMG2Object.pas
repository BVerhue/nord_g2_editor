unit BVE.NMG2Object;

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

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.math,
  System.Character,
  System.SyncObjs,
  BVE.NMG2Types,
  BVE.NMG2FileIntf;

type
  // The ui objects are connected to the G2 objects through the obeserverlist.
  // This is a list because a parameter for example can be connected to
  // multiple knobs in the ui. The G2 object is the subject and the ui object
  // is the observer.
  // Before an ui object is destroyed it must have removed itself from the
  // observerlist.
  TG2ObservedObject = class(TInterfacedObject, IG2Subject)
  private
  public
    FObserverList: TList<IG2Observer>;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterObserver(aObserver: IG2Observer);
    procedure RemoveObserver(aObserver: IG2Observer);
    procedure NotifyObservers(aG2Event: TG2Event; const aG2Object: IG2Object);
    procedure NotifyDestroy;
  end;

implementation

{ TG2ObservedObject }

constructor TG2ObservedObject.Create;
begin
  inherited;
  FObserverList := TList<IG2Observer>.Create;
end;

destructor TG2ObservedObject.Destroy;
begin
  FreeAndNil(FObserverList);
  inherited;
end;

procedure TG2ObservedObject.NotifyDestroy;
var
  Observer: IG2Observer;
  TempList: TList<IG2Observer>;
begin
  // The observed objects will unregister themselves, so make a templist to
  // have a static list to iterate over
  TempList := TList<IG2Observer>.Create;
  try
    for Observer in FObserverList do
      TempList.Add(Observer);

    for Observer in TempList do
      Observer.Update(evtDestroy, self as IG2Subject);

  finally
    TempList.Free;
  end;
end;

procedure TG2ObservedObject.NotifyObservers(aG2Event: TG2Event;
  const aG2Object: IG2Object);
var
  Observer: IG2Observer;
begin
  for Observer in FObserverList do
    Observer.Update(aG2Event, aG2Object);
end;

procedure TG2ObservedObject.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add(aObserver);
end;

procedure TG2ObservedObject.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove(aObserver);
end;

end.

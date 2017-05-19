unit BVE.NMG2CablePhysics;

// http://charly-studio.com/blog/html5-rope-simulation-verlet-integration-and-relaxation/

interface
uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections;

type
  TCableNode = class
    X,
    Y,
    OldX,
    OldY: single;
    IsPinned: boolean;
  end;

  TCableNodeList = class(TObjectList<TCableNode>)
  private
    FFPS: integer;
    FGravity: single;
    FPixelsPerMeter: single;
    FLength: single;
    FRelaxationIterations: integer;

    procedure ApplyUnitaryVerletIntegration(aNode: TCableNode; aEllapsedTime, aGravity, aPixelPerMeter: single);
    procedure ApplyUnitaryDistanceRelaxation(aNode, aFromNode: TCableNode; aTargetLength: single);
  public
    constructor Create(aStartPoint, aEndPoint: TPointF; const aNodeCount: integer);
    destructor Destroy; override;
    procedure Iterate;
  end;

implementation

{ TCableNodeList }

procedure TCableNodeList.ApplyUnitaryDistanceRelaxation(aNode,
  aFromNode: TCableNode; aTargetLength: single);
var dx, dy, dst: single;
begin
  dx := aNode.X - aFromNode.X;
  dy := aNode.Y - aFromNode.Y;
  dst := Sqrt(dx * dx + dy * dy);

  if (dst > aTargetLength) and (dst <>0) then begin
    aNode.X := aNode.X - (dst - aTargetLength) * (dx / dst) * 0.5;
    aNode.Y := aNode.Y - (dst - aTargetLength) * (dy / dst) * 0.5;
  end;
end;

procedure TCableNodeList.ApplyUnitaryVerletIntegration(aNode: TCableNode;
  aEllapsedTime, aGravity, aPixelPerMeter: single);
begin
  aNode.X := 2*aNode.X - aNode.OldX; // No accelaration here
  aNode.Y := 2*aNode.Y - aNode.OldY + aGravity * aEllapsedTime * aPixelPerMeter;
end;

constructor TCableNodeList.Create(aStartPoint, aEndPoint: TPointF; const aNodeCount: integer);
var i: integer;
    Node: TCableNode;
    dx, dy: single;
begin
  inherited Create(True);

  FFPS := 30;
  FGravity := 9.81;
  FPixelsPerMeter := 800;
  FRelaxationIterations := 50;

  dx := aEndPoint.X - aStartPoint.X;
  dy := aEndPoint.Y - aStartPoint.Y;
  FLength := Sqrt(dx * dx + dy * dy);

  if aNodeCount > 0 then begin

    for i := 0 to aNodeCount - 1 do begin
      Node := TCableNode.Create;
      Node.X := aStartPoint.X + dx / aNodeCount * i;
      Node.Y := aStartPoint.Y + dy / aNodeCount * i;
      Node.OldX := Node.X;
      Node.OldY := Node.Y;
      Node.IsPinned := False;
      Add(Node);
    end;
    Items[0].IsPinned := True;
    Items[aNodeCount-1].IsPinned := True;
  end;
end;

destructor TCableNodeList.Destroy;
begin
  inherited;
end;

procedure TCableNodeList.Iterate;
var EllapsedTime: single;
    ItemLength: single;
    i, Iterations: integer;
    old_x, old_y: single;
    Node, FromNode: TCableNode;
begin
  if FFPS = 0 then
    exit;

  if Count = 0 then
    exit;

  EllapsedTime := 1 / FFPS;
  ItemLength := FLength / Count;

  // Apply verlet integration
  for i := 0 to Count - 1 do begin
    old_x := Items[i].X;
    old_y := Items[i].Y;

    if not Items[i].IsPinned then
      ApplyUnitaryVerletIntegration(Items[i], EllapsedTime, FGravity, FPixelsPerMeter);

    Items[i].OldX := old_x;
    Items[i].OldY := old_y;
  end;

  // Apply relaxation
  for Iterations := 0 to FRelaxationIterations - 1 do begin
    // From Left to right
    for i := 0 to Count - 1 do begin
      Node := Items[i];
      if not Node.IsPinned then
        if i > 0 then begin
          FromNode := Items[i-1];
          ApplyUnitaryDistanceRelaxation(Node, FromNode, ItemLength);
        end;
    end;

    // From right to left
    for i := 0 to Count - 1 do begin
      Node := Items[Count - 1 - i];
      if not Node.IsPinned then
        if i > 0 then begin
          FromNode := Items[Count - i];
          ApplyUnitaryDistanceRelaxation(Node, FromNode, ItemLength);
        end;
    end;
  end;
end;

end.

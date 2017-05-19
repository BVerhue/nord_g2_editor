unit graph_util_fmx;


interface
uses
  System.Types,
  FMX.Types, FMX.Layouts, FMX.Objects;

const
  polyImgSin :  array[0..51] of single =
( 0, 0.551871418952942,
  0.0798164233565331, 0.764491200447083,
  0.166582986712456, 0.932944774627686,
  0.265593677759171, 1,
  0.364604324102402, 0.932944774627686,
  0.451370894908905, 0.764491200447083,
  0.530952632427216, 0.552496671676636,
  0.608637034893036, 0.353392124176025,
  0.677943050861359, 0.2157082259655,
  0.734406352043152, 0.177468329668045,
  0.791102409362793, 0.215865880250931,
  0.860042572021484, 0.349710255861282,
  0.935932874679565, 0.551871418952942,
  1, 0.448128581047058,
  0.920183598995209, 0.235508814454079,
  0.833416998386383, 0.0670552253723145,
  0.734406352043152, 0,
  0.635162889957428, 0.067212887108326,
  0.548762202262878, 0.238857120275497,
  0.46735492348671, 0.447503328323364,
  0.391229897737503, 0.650289714336395,
  0.322289735078812, 0.784134089946747,
  0.265593677759171, 0.822531640529633,
  0.208897605538368, 0.784134089946747,
  0.139957427978516, 0.650289714336395,
  0.064067117869854, 0.448128581047058);

 polyImgTri : array[0..11] of single =
( 0.0532005913555622, 1,
  0.5, 0.22064146399498,
  0.946799397468567, 1,
  1, 0.872157096862793,
  0.5, 0,
  0, 0.872157096862793);

 polyImgSaw : array[0..11] of single =
( 0.0817152112722397, 0.924997508525848,
  0.0817152112722397, 0.246683895587921,
  0.967974662780762, 1,
  1, 0.849995017051697,
  0, 0,
  0, 0.924997508525848);

 polyImgPulse : array[0..23] of single =
( 0.0797342211008072, 0.5,
  0.0797342211008072, 0.16901408135891,
  0.460132896900177, 0.16901408135891,
  0.460132896900177, 1,
  1, 1,
  1, 0.5,
  0.920265793800354, 0.5,
  0.920265793800354, 0.830985903739929,
  0.539867103099823, 0.830985903739929,
  0.539867103099823, 0,
  0, 0,
  0, 0.5);

 polyImgPulse25 : array[0..19] of single =
( 0.0830449834465981, 0.915492951869965,
  0.0830449834465981, 0.16901408135891,
  0.239619374275208, 0.16901408135891,
  0.239619374275208, 1,
  1, 1,
  1, 0.830985903739929,
  0.322664350271225, 0.830985903739929,
  0.322664350271225, 0,
  0, 0,
  0, 0.915492951869965);

 polyImgPulse10 : array[0..19] of single =
( 0.0830449834465981, 0.915492951869965,
  0.0830449834465981, 0.16901408135891,
  0.119809687137604, 0.16901408135891,
  0.119809687137604, 1,
  1, 1,
  1, 0.830985903739929,
  0.202854678034782, 0.830985903739929,
  0.202854678034782, 0,
  0, 0,
  0, 0.915492951869965);

 polyImgOnOff : array[0..67] of single =
( 0.617000222206116, 0.148863524198532,
  0.724714696407318, 0.220813930034637,
  0.793899118900299, 0.314062058925629,
  0.828903794288635, 0.426043599843979,
  0.825657665729523, 0.543735444545746,
  0.784538328647614, 0.65345025062561,
  0.710327804088593, 0.742428481578827,
  0.611656665802002, 0.800322115421295,
  0.500000059604645, 0.820398271083832,
  0.38834348320961, 0.80032217502594,
  0.289672285318375, 0.742428541183472,
  0.215461745858192, 0.6534503698349,
  0.1743423640728, 0.543735563755035,
  0.171096220612526, 0.426043689250946,
  0.206100821495056, 0.314062148332596,
  0.275285273790359, 0.220814004540443,
  0.382999688386917, 0.148863568902016,
  0.290895104408264, 8.74068177836307E-8,
  0.158388331532478, 0.0885111764073372,
  0.0532140843570232, 0.230267077684402,
  0, 0.400501668453217,
  0.00493479333817959, 0.579416930675507,
  0.0674445554614067, 0.746205568313599,
  0.180259540677071, 0.881470322608948,
  0.330259591341019, 0.969480216503143,
  0.50000011920929, 1,
  0.669740557670593, 0.969480216503143,
  0.81974059343338, 0.881470203399658,
  0.932555556297302, 0.746205449104309,
  0.995065271854401, 0.579416811466217,
  1, 0.400501519441605,
  0.946785867214203, 0.230266943573952,
  0.841611564159393, 0.0885110348463058,
  0.709104776382446, 0);

type
  TPolyImg = (piSin, piSaw);

  procedure DrawPolyOnOff( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
  procedure DrawPolySin( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
  procedure DrawPolyTri( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
  procedure DrawPolySaw( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
  procedure DrawPolyPulse( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
  procedure DrawPolyPulse25( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);

  procedure DrawImage( aParamID, aParamValue : integer; aCanvas : TCanvas; aBitmap : TBitmap; aSrceRect, aDestRect : TRectF; aMarginHorz, aMarginVert : single; aOpacity : single);

implementation

function SubRect( aRect : TRectF; aMargin : single): TRectF;
begin
  Result.Left := aRect.Left + aMargin;
  Result.Width := aRect.Width - aMargin*2;
  Result.Top := aRect.Top + aMargin;
  Result.Height := aRect.Height - aMargin*2;
end;

procedure DrawPolyOnOff( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R, R2 : TRectF;
    d, d2 : single;
begin
  d2 := aMargin + 0.6;

  R := SubRect( aDestRect, aMargin + 0.6);

  SetLength( Poly, 34);
  for i := 0 to 33 do begin
    Poly[i].X := R.Left + polyImgOnOff[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgOnOff[i*2+1] * R.Height;
  end;
  aCanvas.FillPolygon(Poly, aOpacity);

  d := 0.6;
  R2.Left := R.Width/2 + R.Left - d;
  R2.Right := R2.Left + d*2;
  R2.Top := aDestRect.Top + aMargin;
  R2.Height := 3;

  aCanvas.FillRect( R2, 0, 0, allCorners, aOpacity);
end;

procedure DrawPolySin( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R : TRectF;
begin
  R := SubRect( aDestRect, aMargin);

  SetLength( Poly, 26);
  for i := 0 to 25 do begin
    Poly[i].X := R.Left + polyImgSin[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgSin[i*2+1] * R.Height;
  end;

  aCanvas.FillPolygon(Poly, aOpacity);
end;

procedure DrawPolySaw( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R : TRectF;
begin
  R := SubRect( aDestRect, aMargin);

  SetLength( Poly, 6);
  for i := 0 to 5 do begin
    Poly[i].X := R.Left + polyImgSaw[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgSaw[i*2+1] * R.Height;
  end;

  aCanvas.FillPolygon(Poly, aOpacity);
end;

procedure DrawPolyTri( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R : TRectF;
begin
  R := SubRect( aDestRect, aMargin);

  SetLength( Poly, 6);
  for i := 0 to 5 do begin
    Poly[i].X := R.Left + polyImgTri[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgTri[i*2+1] * R.Height;
  end;

  aCanvas.FillPolygon(Poly, aOpacity);
end;

procedure DrawPolyPulse( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R : TRectF;
begin
  R := SubRect( aDestRect, aMargin);

  SetLength( Poly, 12);
  for i := 0 to 11 do begin
    Poly[i].X := R.Left + polyImgPulse[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgPulse[i*2+1] * R.Height;
  end;

  aCanvas.FillPolygon(Poly, aOpacity);
end;

procedure DrawPolyPulse25( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R : TRectF;
begin
  R := SubRect( aDestRect, aMargin);

  SetLength( Poly, 10);
  for i := 0 to 9 do begin
    Poly[i].X := R.Left + polyImgPulse25[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgPulse25[i*2+1] * R.Height;
  end;

  aCanvas.FillPolygon(Poly, aOpacity);
end;

procedure DrawPolyPulse10( aCanvas : TCanvas; aDestRect : TRectF; aMargin : single; aOpacity : single);
var Poly : TPolygon;
    i : integer;
    R : TRectF;
begin
  R := SubRect( aDestRect, aMargin);

  SetLength( Poly, 10);
  for i := 0 to 9 do begin
    Poly[i].X := R.Left + polyImgPulse10[i*2] * R.Width;
    Poly[i].Y := R.Top +  polyImgPulse10[i*2+1] * R.Height;
  end;

  aCanvas.FillPolygon(Poly, aOpacity);
end;

procedure DrawImage( aParamID, aParamValue : integer; aCanvas : TCanvas; aBitmap : TBitmap; aSrceRect, aDestRect : TRectF; aMarginHorz, aMarginVert : single; aOpacity : single);

  procedure DrawBitmap;
  begin
    aCanvas.DrawBitmap( aBitmap, aSrceRect, aDestRect, aOpacity);
  end;

begin
  case aParamID of
   33 : begin // EnvNR
          DrawBitmap;
        end;
   34 : begin // EnvShape_3
          DrawBitmap;
        end;
   75 : begin // LfoA_WaveForm
          DrawBitmap;
        end;
   76 : begin // LfoB_WaveForm
          DrawBitmap;
        end;
   81 : begin // LfoShpA__WaveForm
          DrawBitmap;
        end;
  106 : begin // OffOn
          DrawPolyOnOff( aCanvas, aDestRect, 2, aOpacity);
        end;
  118 : begin // OscA_WaveForm
          case aParamValue of
            0 : DrawPolySin( aCanvas, aDestRect, 2, aOpacity);
            1 : DrawPolyTri( aCanvas, aDestRect, 2, aOpacity);
            2 : DrawPolySaw( aCanvas, aDestRect, 2, aOpacity);
            3 : DrawPolyPulse( aCanvas, aDestRect, 2, aOpacity);
            4 : DrawPolyPulse25( aCanvas, aDestRect, 2, aOpacity);
            5 : DrawPolyPulse10( aCanvas, aDestRect, 2, aOpacity);
            else
              DrawBitmap;
          end;
        end;
  119 : begin // OscBWaveForm
          DrawBitmap;
        end;
  120 : begin // OscShpA_WaveForm
          DrawBitmap;
        end;
  121 : begin // OscWaveForm_1
          DrawBitmap;
        end;
  122 : begin // OscWaveForm_2
          DrawBitmap;
        end;
  133 : begin // OscWaveForm_3
          DrawBitmap;
        end;
    else
      DrawBitmap;
  end;
end;

end.

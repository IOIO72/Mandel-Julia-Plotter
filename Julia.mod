MODULE Julia;

(* Optionen für optimalen Code: $R- $F- $V- $P- *)

(*

 Mandel-/Julia Plotter v0.6 (09.03.1991)

 Besonderheiten (v0.6):

 - Achsen-/Punktspiegelung
 - 32 Farben
 - Mausunterstützte Zoomfunktion
 - Mausunterstützte Parameterübergabe
 - FFP Version

 geplant für die Version 1.0:

 - eigene IFF-Speicherroutine mit Kommentar für die Werte
 - Laderoutine, um weiter arbeiten zu können
 - Fortsetzungsmöglichkeit beim Abbruch
 - Intuition Benutzeroberfläche (kein Tastaturmenü mehr)
 - Achsen-/Punktspiegelung auch bei ungleichen Grenzwerten
 - Orbital-Darstellung
 - 3D-Darstellung
 - Unterstützung von 4096 Farben

*)



FROM	SYSTEM		IMPORT	FFP,ADR,ADDRESS;
FROM	FFPConversions	IMPORT	RealToStr;
FROM	Str		IMPORT	Concat;
FROM	Arts		IMPORT	DetectCtrlC;
FROM	InOut		IMPORT	WriteString, Read, ReadInt, WriteLn, Write,
				WriteInt;
FROM	FFPInOut	IMPORT	ReadReal, WriteReal;
FROM	Intuition	IMPORT	WindowFlagSet, ScreenFlagSet, Window, Screen,
				NewScreen, NewWindow, OpenWindow, OpenScreen,
				CloseWindow, CloseScreen, WindowPtr, ScreenPtr,
				IDCMPFlagSet, windowSizing, windowDrag, rmbTrap,
				Gadgets, customBitMap, customScreen,
				IntuiMessagePtr, IntuiMessage, mouseButtons,
				SetWindowTitles;
(* FROM	InputEvent	IMPORT	lButton, rButton; *)
FROM	Graphics	IMPORT	Text, WritePixel, SetRast, SetRGB4, SetAPen,
				Move, Draw, SetDrMd, ViewModeSet, RastPortPtr,
				DrawModeSet, complement, dm0;
FROM	Exec		IMPORT	GetMsg, ReplyMsg;

CONST	AnzFarben	=	32; (* 2^5 *)

TYPE	REAL = FFP;	(*

			"FFP" nur damit das Rechnen schneller wird
			Die Rechengenauigkeit ist zwar etwas schlechter,
			aber der Unterschied in der fertigen Grafik ist
			nicht sichtbar!

			"FFP" steht für Fast-Floating-Point und ist im AMIGA-
			ROM vorhanden => kürzere & schnellere Programme

			*)

	CTyp = RECORD
	  Rt : REAL;
	  It : REAL;
	END;

	STRING = ARRAY[0..79] OF CHAR;	(* in Modula2 gibt es kein String *)

VAR	err	  : BOOLEAN;
	winptr	  : WindowPtr;
	scrptr	  : ScreenPtr;
	RP	  : RastPortPtr;
	zliun,
	zreob,c   : CTyp;
	g	  : INTEGER;
	ch,h	  : CHAR;
	myclass   : IDCMPFlagSet;
	myaddress : ADDRESS;
	mycode	  : CARDINAL;		(* Berreich von +0 bis +65535 *)

PROCEDURE Info;
VAR ch : CHAR;
BEGIN
  Write(CHR(12));
  WriteString('Mandel-/Julia Plotter by ALPHA-DISK © 1991');WriteLn;WriteLn;
  WriteString('internal version 0.6')			   ;WriteLn;WriteLn;
  WriteString('written in Modula 2 (M2Amiga v3.3d) by TPH / KZ')   ;WriteLn;
  Read(ch)
END Info;

PROCEDURE schreibMenu;
BEGIN
  Write(CHR(12));
  WriteString(' I = Info')					;WriteLn;
  WriteString(' W = Werte ausgeben	(R = Initialisieren)')	;WriteLn;
  WriteString(' B = Bildgrenzen')				;WriteLn;
  WriteString(' P = Parameter c')				;WriteLn;
  WriteString(' C = Parameter c aus der Mandelbrotmenge')	;WriteLn;
  WriteString(' G = Genauigkeit')				;WriteLn;
  WriteString(' J = Julia		(g(z)=z²+c)')		;WriteLn;
  WriteString(' M = Mandelbrot		(g(c)=(c²+c)²+c)')	;WriteLn;
  WriteString(' Z = Zoomen')					;WriteLn;
  WriteString(' Q = Ende')				;WriteLn;WriteLn;
  WriteString('Ihre Wahl: ');
END schreibMenu;


PROCEDURE SetzeFarben;			(* Initialisiere Farbregister 0-31 *)
VAR i,f : INTEGER;
BEGIN
  SetRGB4(ADR(scrptr^.viewPort),0,3,0,5);
  FOR i:=1 TO 31 DO
    f:=i MOD 2;
    SetRGB4(ADR(scrptr^.viewPort),i,i,0,f);
  END;
END SetzeFarben;


PROCEDURE SetPos (x ,y : INTEGER);	(* Grafik-Cursor plazieren *)
BEGIN
  Move(RP,x,y);
END SetPos;


PROCEDURE WriteS (s : ADDRESS; n : INTEGER);	(* Text ausgeben *)
BEGIN
  Text(RP,s,n);
END WriteS;

PROCEDURE GetSize(winptr : WindowPtr; VAR x, y : INTEGER); (* GetMaxX/Y *)
BEGIN
  x:=winptr^.width-20;
  y:=winptr^.height-20;
END GetSize;

PROCEDURE InitGraph;			(* Screen und Window öffnen *)
VAR Name	: ARRAY[0..40] OF CHAR;
    Scr		: NewScreen;
    Win		: NewWindow;
BEGIN
  Name:="Julia-/Mandel Plotter von ALPHA-DISK";
  WITH Scr DO
    leftEdge:=0;topEdge:=0;
    width:=320;height:=256;
    depth:=5;
    detailPen:=0;blockPen:=1;
    viewModes:=ViewModeSet{};
    type:=customScreen;
    font:=NIL;
    defaultTitle:=ADR(Name);
    gadgets:=NIL;
    customBitMap:=NIL;
  END;
  scrptr:=OpenScreen(Scr);
					(* Window *)

  WITH Win DO
    leftEdge:=0;topEdge:=0;
    width:=50;height:=60;
    detailPen:=0;blockPen:=1;
    idcmpFlags:=IDCMPFlagSet{mouseButtons};
    flags:=WindowFlagSet{windowSizing, windowDrag, rmbTrap};
    firstGadget:=NIL;
    checkMark:=NIL;
    title:=ADR(Name);
    screen:=scrptr;
    bitMap:=NIL;
    minWidth:=20;minHeight:=40;
    maxWidth:=320;maxHeight:=256;
    type:=customScreen;
  END;
  winptr:=OpenWindow(Win);
  RP:=winptr^.rPort;
END InitGraph;

PROCEDURE CloseGraph;			(* Screen und Window schließen *)
BEGIN
  CloseWindow(winptr);
  CloseScreen(scrptr);
END CloseGraph;


PROCEDURE cls;				(* clrscr *)
BEGIN
  SetRast(RP,0);
END cls;


(* Intuition-Message ermitteln *)

PROCEDURE GetIMsg (window : WindowPtr; VAR getclass : IDCMPFlagSet;
		   VAR getcode : CARDINAL; VAR getadr : ADDRESS);
VAR IMsg : POINTER TO IntuiMessage;
BEGIN
  getclass:=IDCMPFlagSet{};getcode:=0;
  getadr  :=NIL;
  IMsg    :=GetMsg(window^.userPort);
  WHILE IMsg # NIL DO
    getclass:= IMsg^.class;
    getcode := IMsg^.code;
    getadr  := IMsg^.iAddress;
    ReplyMsg(IMsg);
    IMsg    := GetMsg(window^.userPort);
  END; (* WHILE *)
END GetIMsg;


PROCEDURE Maus(winptr : WindowPtr):BOOLEAN;	(* Maustasten abfragen *)
VAR bool : BOOLEAN;
BEGIN
  bool:=FALSE;
  GetIMsg(winptr,myclass,mycode,myaddress);
  IF (mouseButtons IN myclass) THEN bool:=TRUE END;
  RETURN bool;
END Maus;


(* alle Variabeln initialisieren *)

PROCEDURE Initialisiere(VAR zliun, zreob, c : CTyp; VAR g : INTEGER);
BEGIN
  zliun.Rt	:=	-1.8;
  zliun.It	:=	-1.8;
  zreob.Rt	:=	 1.8;
  zreob.It	:=	 1.8;
  c.Rt		:=	-0.74543;
  c.It		:=	 0.11301;
  g		:=	100;
END Initialisiere;


(* Realkoordinaten <-> Bildschirmkoordinaten *)

PROCEDURE Delta(winptr : WindowPtr; zliun, zreob : CTyp;
		VAR Deltax, Deltay : REAL);
VAR h1,h2 : REAL;
    Bildhoehe, Bildbreite : INTEGER;
BEGIN
  GetSize(winptr,Bildbreite,Bildhoehe);
  h1:=REAL(Bildbreite);			(* Konvertierung von INTEGER in REAL *)
  h2:=REAL(Bildhoehe);			(* " *)
  Deltax:=(zreob.Rt-zliun.Rt) / h1;
  Deltay:=(zreob.It-zliun.It) / h2;
END Delta;


(* Zoomen per Eingabe *)

PROCEDURE liesBildGrenzen(VAR zliun, zreob : CTyp);
VAR re : REAL;
BEGIN
  Write(CHR(12));

  WriteLn;

  WriteString('Realteil     für links-unten (');
  WriteReal(zliun.Rt,8,4);
  WriteString('): ');
  ReadReal(re);IF re # 99.0 THEN zliun.Rt:=re END;

  WriteString('Imaginärteil für links-unten (');
  WriteReal(zliun.It,8,4);
  WriteString('): ');
  ReadReal(re);IF re # 99.0 THEN zliun.It:=re END;

  WriteString('Realteil     für rechts-oben (');
  WriteReal(zreob.Rt,8,4);
  WriteString('): ');
  ReadReal(re);IF re # 99.0 THEN zreob.Rt:=re END;

  WriteString('Imaginärteil für rechts-oben (');
  WriteReal(zreob.It,8,4);
  WriteString('): ');
  ReadReal(re);IF re # 99.0 THEN zreob.It:=re END;

  IF zreob.Rt < zliun.Rt THEN
    re:=zreob.Rt;
    zreob.Rt:=zliun.Rt;
    zliun.Rt:=re;
  END;

  IF zreob.Rt = zliun.Rt THEN zreob.Rt:=zreob.Rt+0.1 END;

  IF zreob.It < zliun.It THEN
    re:=zreob.It;
    zreob.Rt:=zliun.It;
    zliun.It:=re;
  END;

  IF zreob.It = zliun.It THEN zreob.It:=zreob.It+0.1 END;

END liesBildGrenzen;


(* Parameter per Eingabe *)

PROCEDURE liesParameter(VAR c : CTyp);
VAR re : REAL;
BEGIN
  Write(CHR(12));

  WriteString('Realteil     für den Parameter c (');
  WriteReal(c.Rt,8,4);
  WriteString('): ');
  ReadReal(re);IF re # 99.0 THEN c.Rt:=re END;

  WriteString('Imaginärteil für den Parameter c (');
  WriteReal(c.It,8,4);
  WriteString('): ');
  ReadReal(re);IF re # 99.0 THEN c.It:=re END;

END liesParameter;


(* Parameter & Grenzen ausgeben *)

PROCEDURE schreibParameter(c : CTyp);
VAR cRtstr, cItstr, cstr : STRING;

PROCEDURE WandleKomp(comp : CTyp; VAR strRt, strIt : STRING; n : INTEGER);
BEGIN
  RealToStr(comp.Rt,strRt,n,n-1,FALSE,err);
  RealToStr(comp.It,strIt,n,n-1,FALSE,err);
END WandleKomp;

BEGIN
  SetAPen(RP,1);
  WandleKomp(c,cRtstr,cItstr,10);
  cstr:='c = ';
  Concat(cstr,cRtstr);
  Concat(cstr,' + ' );
  Concat(cstr,cItstr);
  Concat(cstr,' i'  );
  SetWindowTitles(winptr,ADR(cstr),ADR(cstr));

  WandleKomp(zliun,cRtstr,cItstr,5);
  cstr:=cRtstr;
  Concat(cstr,' + ');
  Concat(cstr,cItstr);
  Concat(cstr,' i «-» ');

  WandleKomp(zreob,cRtstr,cItstr,5);
  Concat(cstr,cRtstr);
  Concat(cstr,' + ');
  Concat(cstr,cItstr);
  Concat(cstr,' i');

  SetPos(2,2*8);
  WriteS(ADR(cstr),35);

END schreibParameter;


(* Parameter per Maus *)

PROCEDURE Parameter(winptr : WindowPtr; cliun, creob : CTyp; VAR c : CTyp);
VAR dx,dy : REAL;

BEGIN
  REPEAT UNTIL Maus(winptr)=FALSE;
  Delta(winptr,cliun,creob,dx,dy);
  REPEAT
    c.Rt:= -(creob.Rt - REAL( winptr^.mouseX -  4) * dx);
    c.It:=   creob.It + REAL(-winptr^.mouseY + 20) * dy ;
    schreibParameter(c);
  UNTIL Maus(winptr)=TRUE;
END Parameter;


(* Zoomen per Maus *)

PROCEDURE Zoomen(winptr : WindowPtr; zliun, zreob : CTyp;
		 VAR Nzliun, Nzreob : CTyp);
VAR x,y : INTEGER;
    re  : REAL;
    ch,h: CHAR;
BEGIN

  REPEAT
    REPEAT UNTIL Maus(winptr)=FALSE;
    Parameter(winptr,zliun,zreob,Nzliun);
    x:=winptr^.mouseX;
    y:=winptr^.mouseY;
    REPEAT UNTIL Maus(winptr)=FALSE;
    Parameter(winptr,zliun,zreob,Nzreob);

    SetDrMd(RP,DrawModeSet{complement});
    Move(RP,x,y);
    Draw(RP,x,winptr^.mouseY);
    Draw(RP,winptr^.mouseX,winptr^.mouseY);
    Draw(RP,winptr^.mouseX,y);
    Draw(RP,x,y);
    SetDrMd(RP,DrawModeSet{dm0});

    WriteString('Übernehmen ? ');Read(ch);Read(h);

  UNTIL CAP(ch)='J';

  Nzreob.Rt:=-Nzreob.Rt;
  Nzliun.Rt:=-Nzliun.Rt;

  IF Nzreob.Rt < Nzliun.Rt THEN
    re:=Nzreob.Rt;
    Nzreob.Rt:=Nzliun.Rt;
    Nzliun.Rt:=re;
  END;

  IF Nzreob.Rt = Nzliun.Rt THEN Nzreob.Rt:=Nzreob.Rt+0.1 END;

  IF Nzreob.It < Nzliun.It THEN
    re:=Nzreob.It;
    Nzreob.Rt:=Nzliun.It;
    Nzliun.It:=re;
  END;

  IF Nzreob.It = Nzliun.It THEN Nzreob.It:=Nzreob.It+0.1 END;

END Zoomen;


(* Iterationstiefe *)

PROCEDURE liesGenauigkeit(VAR g : INTEGER);
VAR i : INTEGER;
BEGIN
  Write(CHR(12));

  WriteString('Genauigkeit (');
  WriteInt(g,3);
  WriteString('): ');
  ReadInt(i);WriteLn;
  IF i > 0 THEN g:=i END;

END liesGenauigkeit;


(* Alle Werte außer dem Parameter ausgeben *)

PROCEDURE GibWerteAus;
VAR rtstr, itstr, cstr : STRING;
  PROCEDURE SchreibKomp;
  BEGIN
    Concat(cstr,rtstr);
    Concat(cstr,' + ');
    Concat(cstr,itstr);
    Concat(cstr,' i' );
    WriteString(cstr);WriteLn;
  END SchreibKomp;
BEGIN
  RealToStr(zliun.Rt,rtstr,10,6,FALSE,err);
  RealToStr(zliun.It,itstr,10,6,FALSE,err);
  Write(CHR(12));
  cstr:='Links unten = '; SchreibKomp;
  RealToStr(zreob.Rt,rtstr,10,6,FALSE,err);
  RealToStr(zreob.It,itstr,10,6,FALSE,err);
  cstr:='Rechts oben = '; SchreibKomp;
  WriteLn;
  WriteString ('Genauigkeit = ');
  WriteInt    (g,1);
  WriteLn;
END GibWerteAus;


(* Juliamenge zeichnen *)

PROCEDURE zeichneJuliaMenge (zliun, zreob, c : CTyp; g : INTEGER);
VAR PStartx, PStarty, Bildbreite, Bildhoehe, EndeY, Farbe : INTEGER;
    a, b, x, y, x1, y1, xq, yq, Deltax, Deltay, h, h1, h2 : REAL;
    Spalte, Zeile, Zaehler : INTEGER;
    PunktSp : BOOLEAN;
BEGIN
  Delta(winptr,zliun,zreob,Deltax,Deltay);
  GetSize(winptr,Bildbreite,Bildhoehe);
  PStartx:= 4;
  PStarty:=20;
  a:=c.Rt;
  b:=c.It;
  x:=zreob.Rt;

  PunktSp:=FALSE;
  EndeY:=PStartx+Bildbreite;
  IF (ABS(zreob.Rt) = ABS(zliun.Rt)) AND (ABS(zreob.It) = ABS(zliun.It)) THEN
    EndeY:=(PStartx+Bildbreite) DIV 2 + 2;
    PunktSp:=TRUE
  END;

  FOR Spalte:=PStartx TO EndeY DO
    y:=zreob.It;
    FOR Zeile:=PStarty TO PStarty+Bildhoehe DO
      x1:=x;
      y1:=y; Zaehler:=0;
      REPEAT
	INC(Zaehler);
	xq:=x1*x1;yq:=y1*y1;
	y1:=2.0*x1*y1+b; x1:=xq-yq+a;
      UNTIL (xq+yq > 4.0) OR (Zaehler > g);

      Farbe:=Zaehler MOD AnzFarben;

      SetAPen(RP,Farbe);

      IF Zaehler < g THEN
	err:=WritePixel(RP,Spalte,Zeile);
	IF PunktSp = TRUE THEN
	  err:=WritePixel(RP,Bildbreite+8-Spalte,Bildhoehe+40-Zeile)
	END;
      END;
      y:=y-Deltay;
    END;
    IF Maus(winptr)=TRUE THEN Spalte:=EndeY END;
    x:=x-Deltax;
  END;
END zeichneJuliaMenge;


(* Mandelbrot zeichnen *)

PROCEDURE Mandelbrot(cliun, creob : CTyp; g : INTEGER);
VAR PStarta, PStartb, Bildbreite, Bildhoehe, Farbe  : INTEGER;
    r, i, cr, ci, zr, zi, dx, dy, h1, h2 : REAL;
    Spalte, Zeile, it, Ende : INTEGER;
    AchsenSp : BOOLEAN;
BEGIN
  Delta(winptr,cliun,creob,dx,dy);
  GetSize(winptr,Bildbreite,Bildhoehe);
  PStarta:= 4;
  PStartb:=20;
  ci:=creob.It;

  AchsenSp:=TRUE;
  Ende:=(PStartb+Bildhoehe) DIV 2 + 10;
  IF ABS(creob.It) # ABS(cliun.It) THEN
    Ende:=(PStartb+Bildhoehe);
    AchsenSp:=FALSE;
  END;

  FOR Zeile:=PStartb TO Ende DO
    cr:=creob.Rt;
    FOR Spalte:=PStarta TO PStarta+Bildbreite DO
      it:=0;
      zr:=0.0;
      zi:=0.0;
      REPEAT
	INC(it);
	i:=zi; r:=zr;
	zi:=2.0*i*r-ci;
	zr:=r*r-i*i-cr;
      UNTIL (zr*zr+zi*zi > 4.0) OR (it > g);

      Farbe:=it MOD AnzFarben;

      SetAPen(RP,Farbe);

      IF it < g THEN
        err:=WritePixel(RP,Spalte,Zeile);
        IF AchsenSp=TRUE THEN
          err:=WritePixel(RP,Spalte,Bildhoehe+40-Zeile);
        END;
      END;
      cr:=cr-dx;
    END;
    IF Maus(winptr)=TRUE THEN Zeile:=Ende END;
    ci:=ci-dy;
  END
END Mandelbrot;


(* Hauptprogramm *)

BEGIN
  Info;
  InitGraph;
  DetectCtrlC(FALSE);			(* bei Ctrl-C nicht abbrechen ... *)
  SetzeFarben;
  Initialisiere(zliun, zreob, c, g);
  REPEAT
    schreibMenu;
    Read(ch);Read(h);
    CASE CAP(ch) OF
      'R' : Initialisiere(zliun, zreob, c, g);		|
      'W' : GibWerteAus; Read(h);			|
      'B' : liesBildGrenzen(zliun,zreob);		|
      'P' : liesParameter(c);				|
      'G' : liesGenauigkeit(g);				|
      'I' : Info;					|
      'Z' : Zoomen(winptr,zliun,zreob,zliun,zreob);	|
      'C' : Parameter(winptr,zliun,zreob,c);		|
      'J' : GibWerteAus;WriteLn;
            WriteString('Zeichne... Bitte warten! (Abbruch mit Maus)');WriteLn;
	    cls;
	    schreibParameter(c);
	    zeichneJuliaMenge(zliun,zreob,c,g);		|
      'M' : GibWerteAus;WriteLn;
      	    WriteString('Zeichne... Bitte warten! (Abbruch mit Maus)');WriteLn;
	    WriteLn;WriteString('Mandelbrot');WriteLn;WriteLn;
	    cls;
	    Mandelbrot(zliun,zreob,g);			|
    ELSE END;
  UNTIL CAP(ch)='Q';
  CloseGraph;
END Julia.


 Unit NgmGraph;

    Interface

        Uses
          Crt,Dos,Graph,NgmMath,Mouse;

        Type
          Point =Record
            x,y : LongInt;
          End;

          PalR = Array[0..256] of Record
                                    R,G,B : Byte;
                                  End;

          WinType=Record
                    x,y : Byte;
                    Win : Array[1..81,1..52] of Word;
                  End;
          MveMatrix=Array[1..1024] of Byte;
          IcoMatrix=Array[1..1024] of Byte;

          PBotao=Object
            x1,y1,x2,y2 : Word;
            Ico : IcoMatrix;
            Event : Byte;
            OnLine : String;
            Procedure Init(xx1,yy1,xx2,yy2 : Word; Ev : Byte; Var OnL : String;Var I : IcoMatrix);
            Procedure Aperte;
          End;

        Const
          PalColor : Array[0..15] of Byte =
(Black,Red,Green,Brown,Blue,Magenta,Cyan,DarkGray,LightGray,LightRed,LightGreen,Yellow,LightBlue,LightMagenta,LightCyan,White);

        Var
          CenterX,CenterY : Integer;
          Angl,Tilt       : Integer;
          CosA,SinA,CosB,SinB,
          CosACosB,SinASinB,
          CosASinB,SinACosB  : Real;
          PerspectivePlot : Boolean;
          Mx,My,Mz,ds : Real;
          TimeStart,TimeStop : Real;
          OldPage : Word;
          NgMError : Byte;


        Procedure InitG(Vesa : Boolean);
        Function Min(n1,n2 : Integer) : Integer;
        Function Max(n1,n2 : Integer) : Integer;
        Procedure LineWR(P1,P2 : Point; Raio : Real; Var P : Point;Var Sen,Coss : Real);
        Procedure Plot16M(x,y : Word; R,G,B : Byte; M : LongInt);
        Procedure Plot64K(x,y : Word; Color : Word);
        Procedure InitPloting(Ang,Tlt,cx,cy : Integer);
        Procedure InitPerspective(Perspective : Boolean; x,y,z,m : Real);
        Procedure MapCoodinates(x,y,z : Real; Var Px,Py : Integer);
        Procedure Line3D(x1,y1,z1,x2,y2,z2 : Real);
        Procedure Botao(x1,y1,x2,y2 : Word; Out : Boolean; Tam : Byte);
        Procedure NReadChar(x,y : Word; Var S : Char; Var Fore,Back,Blink : Byte);
        Function GetPalavra(x,y : Word) : String;
        Procedure DrawMve(x,y : integer;Var MveM : MveMatrix);
        Procedure GetMve(NMve : Byte; Var MveM : MveMatrix; Path : String);
        Procedure DrawIco(x,y : integer; IcoM : IcoMatrix);
        Procedure GetIco(Var IcoM : IcoMatrix; Path : String);
        Procedure GetBMP16(Lx,Ly : Word; Path : String);
        Procedure GetBMP256(Px,Py : Word; Path : String);
        Procedure GetBMP256t(Px,Py : Word; Path : String);
        Procedure SetPal(HUE : PalR);
        Procedure RolaWUp(N,x1,y1,x2,y2 : Byte);
        Procedure RolaWDown(N,x1,y1,x2,y2 : Byte);
        Procedure Beep(Freq,Dela : Word);
        Procedure WriteWindow(x,y : Byte; W : WinType);
        Procedure GetWindow(x1,y1,x2,y2 : Byte; Var W : WinType);
        Procedure TimerON;
        Procedure TimerOFF(Var  Time : Real);
        Procedure Retire(Var S : String; Cha : Char);
        Procedure SetMode(Mode : Word);
        Procedure Plot(x,y : Word; Color : Byte; MaxX : LongInt);
        Procedure Plot3D(x,y,z : Real; Color : Byte);
        Procedure PlotPolar3D(a1,a2,r : Real; Color : Byte);
        Procedure Pal;
        Procedure LinePolar(r1,a1,r2,a2 : Real; CenX,CenY : Word);
        Procedure CallPointer(P : Pointer);
        Procedure GetDll(NDll : Byte; Var DllM : IcoMatrix; Path : String);
        Procedure BB(x1,y1,x2,y2 : Word; Color : Byte);
         procedure espera_retraco;

  Implementation

     procedure espera_retraco;
       const porta=$3da;
       var lido : byte;
       begin
         while ((lido and 8)<>0) do
           lido:=port[porta];

         while(not((lido and 8)<>0)) do
           lido:=port[porta];

       end;

     Function Min(n1,n2 : Integer) : Integer;
       Begin
         If (n1<n2) Then
           Min:=n1
         Else
           Min:=n2;
       End;

     Function Max(n1,n2 : Integer) : Integer;
       Begin
         If (n1>n2) Then
           Max:=n1
         Else
           Max:=n2;
       End;

     Procedure BB(x1,y1,x2,y2 : Word; Color : Byte);
       Begin
         SetFillStyle(SolidFill,Color);
         Bar(x1,y1,x2,y2);
       End;

      Procedure PBotao.Aperte;
        Var
          Im : Pointer;
        Begin
          MHide;
          GetMem(Im,ImageSize(x1,y1,x2,y2));
          GetImage(x1,y1,x2,y2,Im^);
          BB(x1,y1,x2,y2,0);
          PutImage(x1+2,y1+2,Im^,NormalPut);
          Delay(100);
          BB(x1+2,y1+2,x2+2,y2+2,0);
          PutImage(x1,y1,Im^,NormalPut);
          FreeMem(Im,ImageSize(x1,y1,x2,y2));
          MShow;
        End;

      Procedure PBotao.Init(xx1,yy1,xx2,yy2 : Word; Ev : Byte; Var OnL : String;Var I : IcoMatrix);
        Begin
         x1:=xx1; x2:=xx2; y1:=yy1; y2:=yy2;
         Event:=Ev;
         OnLine:=OnL;
         Botao(xx1,yy1,xx2,yy2,TRUE,2);
         Ico:=I;
         DrawIco(xx1+3,yy1+3,Ico);
        End;





      Procedure GetDll(NDll : Byte; Var DllM : IcoMatrix; Path : String);
        Var
          trash : word;
          cc,a : integer;
          b : Byte;
          F : File of Byte;
        Begin
          cc:=1;
          Assign(f,Path);
          Reset(f);
          seek(f,8248+(NDll*1048));

          For trash:=1 to 104 Do
            Read(f,b);

          For a:= 1 to 512 Do
            Begin
              Read(f,b);
              DllM[cc]:= b shr 4;
              DllM[cc+1]:=(b Shl 4) Shr 4;
              cc:=cc+2;
            End;
          Close(f);
        End;

    Procedure LineWR(P1,P2 : Point; Raio : Real; Var P : Point;Var Sen,Coss : Real);
      Var
        Ds,Dc,D,D1,D2 : Real;
      Begin
        Ds:=(P2.y-P1.y);
        Dc:=(P2.x-P1.x);
        D1:=(P1.x-P2.x)*(P1.x-P2.x);
        D2:=(P1.y-P2.y)*(P1.y-P2.y);
        D:=Sqrt(d1+d2);
        Sen:=Ds/(D+1E-20);
        Coss:=Dc/(D+1E-20);
        P.x:=Round(P1.x+Raio*Coss);
        P.y:=Round(P1.y+Raio*Sen);
      End;

    Procedure CallPointer(P : Pointer);
      Begin
        Asm
          PUSHF
          CALL DWORD PTR [bp+04]
        End
      End;


    Procedure PlotPolar3D(a1,a2,r : Real; Color : Byte);
      Var
        x,y,z : Real;
      Begin
        x:=R * Sin(a1) * Cos(a2);
        y:=R * Sin(a1) * Sin(a2);
        z:=R * Cos(a1);

        Plot3D(x,y,z,Color);
      End;


    Procedure LinePolar(r1,a1,r2,a2 : Real; CenX,CenY : Word);
      Var
        x1,y1,x2,y2 : Integer;
      Begin
        x1:=Round(CenX+r1*Cos(a1));
        y1:=Round(CenY-r1*Sin(a1));

        x2:=Round(CenX+r2*Cos(a2));
        y2:=Round(CenY-r2*Sin(a2));

        Line(x1,y1,x2,y2);
      End;




    Procedure InitG(Vesa : Boolean);
      Var
        Gd,Gm : Integer;
      Begin
        Gd:=Detect;
        If Vesa Then
          Begin
           Gd := InstallUserDriver('VESA16', nil);
           Gm:=1;
          End;
{        Else
          Begin
           Gd := InstallUserDriver('EGAVGA', nil);
          End; }

        InitGraph(Gd,Gm, 'd:\lng\tp70\bgi');
      End;

    Procedure Pal;
      Var
        H : PalR;
        t,t2 : Word;
      Begin
        For t:=0 to 255 Do
          Begin
            H[t].R:=t DIV 4;
            H[t].G:=t DIV 4;
            H[t].B:=t DIV 4;
          End;
        SetPal(H);
      End;


    Procedure Retire(Var S : String; Cha : Char);
      Var
        t,tt : Byte;
        st : string;
      Begin
        tt:=1;
        st:='';
        For t:=1 to Length(S) Do
          If S[t] <> Cha Then
            Begin
              st[tt]:=S[t];
              tt:=tt+1;
            End;

        st[0]:=Chr(tt);

        s:=st;

        s:=Copy(s,1,Length(s)-1);

      End;

      Procedure TimerON;
        Var
          h,m,s,s100 : Word;
        Begin
          GetTime(h,m,s,s100);
          TimeStart:=(h*3600)+(m*60)+s+(s100/100);
        End;

      Procedure TimerOFF(Var  Time : Real);
        Var
          h,m,s,s100 : Word;
        Begin
          GetTime(h,m,s,s100);
          TimeStop:=(h*3600)+(m*60)+s+(s100/100);
          Time:=TimeStop-TimeStart;
        End;

      Procedure InitPloting(Ang,Tlt,cx,cy : Integer);
        Begin
          CenterX:=Cx;
          CenterY:=Cy;
          Angl := Ang;
          Tilt := Tlt;
          CosA:=CosD(Ang);
          SinA:=SinD(Ang);
          CosB:=CosD(Tlt);
          SinB:=SinD(Tlt);
          CosACosB:=CosA * CosB;
          SinASinB:=SinA * SinB;
          CosASinB:=CosA * SinB;
          SinACosB:=SinA * CosB;
        End;

      Procedure InitPerspective(Perspective : Boolean; x,y,z,m : Real);
        Begin
          PerspectivePlot:=Perspective;
          Mx:=x;
          My:=y;
          Mz:=z;
          ds:=m;
        End;


      Procedure MapCoodinates(x,y,z : Real; Var Px,Py : Integer);
        Var
          Xt,Yt,Zt : Real;
        Begin
          Xt:=(Mx + x * CosA - y * SinA);
          Yt:=(My + x * SinASinb + y * CosASinB + Z * CosB);
          If PerspectivePlot Then
            Begin
              Zt:=(Mz + x * SinACosB + y * CosACosB + z * SinB);
              Px:=CenterX + Round(ds * Xt /Zt);
              Py:=CenterY + Round(ds * Yt /Zt);
            End
          Else
            Begin
              Px:=CenterX + Round(Xt);
              Py:=CenterY + Round(Yt);
            End;
        End;

      Procedure Line3D(x1,y1,z1,x2,y2,z2 : Real);
        Var
          xx1,yy1,xx2,yy2 : integer;
        Begin
          MapCoodinates(x1,y1,z1,xx1,yy1);
          MapCoodinates(x2,y2,z2,xx2,yy2);
          Line(xx1,yy1,xx2,yy2);

        End;

      Procedure Plot3D(x,y,z : Real; Color : Byte);
        Var
          xx,yy : Integer;
        Begin
          MapCoodinates(x,y,z,xx,yy);
          PutPixel(xx,yy,color);
        End;

      Procedure NReadChar(x,y : Word; Var S : Char;
                  Var Fore,Back,Blink : Byte);
         Begin
           If X>80 Then X:=80;
           If Y>25 Then X:=25;
           X:=X-1;
           Y:=Y-1;
           S:=Chr(Mem[$B800:2*(80*y+x)]);
           X:=X+1;
           Y:=Y+1;
           Fore:=(Mem[$B800:2*(80*y+x)] Shl 4) Shr 4;
           Back:=(Mem[$B800:2*(80*y+x)] Shl 1) Shr 5;
           Blink:=Mem[$B800:2*(80*y+x)] Shr 7;
         End;


      Function GetPalavra(x,y : Word) : String;
        Var
          STemp1 : Char;
          Stemp2 : String;
          Fo,Ba,Bl : Byte;
          xtemp : Word;
        Begin
          If Chr(Mem[$B800:2*(80*y+x)])=' ' Then exit;

          Xtemp:=x;
          STemp2:='';
          Repeat
            NReadChar(x,y,Stemp1,Fo,Ba,Bl);
            STemp2:=STemp2+STemp1;
            x:=x+1;
          Until (STemp1=' ') or (X > 80);
          x:=xtemp-1;
          Repeat
            NReadChar(x,y,Stemp1,Fo,Ba,Bl);
            STemp2:=STemp1+Stemp2;
            x:=x-1
          Until (STemp1=' ') or (X<1);
          While Stemp2[1] = ' ' Do
            Stemp2:=Copy(Stemp2,2,Length(Stemp2)-1);
          GetPalavra:=Stemp2;
        End;

      Procedure GetMve(NMve : Byte; Var MveM : MveMatrix; Path : String);
        Var
          trash : word;
          cc,a : integer;
          b : Byte;
          F : File of Byte;
        Begin
          {$I-}
          cc:=1;
          Assign(f,Path);
          Reset(f);
          seek(f,4896+(NMve*752));

          For trash:=1 to 104 Do
            Read(f,b);

          For a:= 1 to 512 Do
            Begin
              Read(f,b);
              MveM[cc]:= b shr 4;
              MveM[cc+1]:=(b Shl 4) Shr 4;
              cc:=cc+2;
            End;
          Close(f);
          {$I+}
          If DosError<>0 Then NgMError:=128;
        End;


      Procedure DrawMve(x,y : integer;Var MveM : MveMatrix);
        Var
          cc,a,aa : integer;
        Begin
          cc:=1;
          For a:= 32 DownTo 1 Do
            For aa:= 1 to 32 Do
              Begin
                PutPixel(x+aa,y+a,MveM[cc]);
                cc:=cc+1;
              End;
        End;


      Procedure GetIco(Var IcoM : IcoMatrix; Path : String);
        Var
          cc,a : integer;
          b : Byte;
          F : File of Byte;
        Begin
          {$I-}
          cc:=1;
          Assign(f,Path);
          Reset(f);
          seek(f,126);
          For a:= 1 to 512 Do
            Begin
              Read(f,b);
              icoM[cc]:=(b AND 240) shr 4;
 {               b shr 4;  }
              icoM[cc+1]:=b AND 15;
{                 (b Shl 4) Shr 4;   }
              cc:=cc+2;
            End;
          Close(F);
          {$I+}
          If DosError<>0 Then NgMError:=128;
        End;


      Procedure DrawIco(x,y : integer; IcoM : IcoMatrix);
        Var
          cc,a,aa : integer;
        Begin
          cc:=1;
          For a:= 32 DownTo 1 Do
            For aa:= 1 to 32 Do
              Begin
                PutPixel(x+aa,y+a,PalColor[icoM[cc]]);
                cc:=cc+1;
              End;
        End;

      Procedure GetBMP16(Lx,Ly : Word; Path : String);
        Var
          xx,yy,xxx,trash : integer;
          H1,H2,Head : Byte;
          Scan,S1,S2,R8 : Byte;
          x1,y1,x2,y2 : Byte;
          x,y : Word;
          F : File of Byte;
        Begin
          xxx:=1;
          {$I-}
          Assign(f,Path);
          Reset(f);
          {$I+}
          if ioresult<>0 then exit;

          Seek(f,10);
          Read(f,H1);
          Read(f,H2);
          Head:=H2;
          Head:=(Head Shl 8) + H1;

          Seek(f,18);
          Read(f,x1);
          Read(f,x2);
          x:=x2;
          x:=(x Shl 8) + x1;

          If Odd(x) Then x:=x-1;

          If x MOD 8<>0 then
            R8:=(8-(x MOD 8)) DIV 2
          Else
            R8:=0;

{            R8:=0; }

          Seek(f,22);
          Read(f,y1);
          Read(f,y2);
          y:=y2;
          y:=(y Shl 8) + y1;

          Seek(f,Head);

          For yy:=y DownTo 1 Do
            Begin
              For xx:=1 to x DIV 2 Do
                Begin
                  Read(f,Scan);
                  S1:=Scan Shr 4;
                  S2:=(Scan Shl 4) Shr 4;
                  PutPixel(lx+xxx,ly+yy,S1);
                  PutPixel(lx+xxx+1,ly+yy,S2);
                  xxx:=xxx+2;
                End;
              xxx:=1;
              For trash:=1 to r8 Do
                 Read(f,Scan);
            End;
       End;

  Procedure GetBMP256(Px,Py : Word; Path : String);
    Type
       Bl = Array[1..$2FFF] of Byte;
    Var
      t,c,x,y : Word;
      Cont : Word;
      Block : Bl;
      Lx,Ly : LongInt;
      PalBMP : Array[1..1024] of Byte;
      F : File;
      FBegin : LongInt;
      P : PalR;
    Begin
      Assign(F,Path);
      {$I-}
      Reset(F,1);
      {$I+}
      If IOResult<>0 Then Exit;

      Seek(F,18);
      BlockRead(F,Lx,4);
      Seek(F,22);
      BlockRead(F,Ly,4);

      Seek(F,54);
      BlockRead(F,PalBMP,1024);

      Seek(F,10);
      BlockRead(F,FBegin,4);

      Seek(F,FBegin);
      x:=1;
      y:=Ly;


      For t:=0 to 256 Do
        Begin
          P[t].R:=0;
          P[t].G:=0;
          P[t].B:=0;
        End;
      SetPal(P);



      If (Lx MOD 2)<>0 Then Lx:=Lx+1;

      While NOT Eof(F) do
        Begin
          BlockRead(F,Block,$2FFF,Cont);
          If Cont<>0 Then
            For t:= 1 to Cont do
              Begin
                PutPixel(Px+x,Py+y,16+Block[t]);
                x:=x+1;
                If x>Lx Then
                  Begin
                    x:=1;
                    y:=y-1;
                  End;
              End;
        End;

      c:=1;
      For t:=0 to 256 do
        Begin
          P[t].R:=PalBMP[c+2] SHR 2;
          P[t].G:=PalBMP[c+1] SHR 2;
          P[t].B:=PalBMP[c] SHR 2;
          c:=c+4;
        End;
      SetPal(P);

      Close(F);

    End;

  Procedure GetBMP256t(Px,Py : Word; Path : String);
    Type
       Bl = Array[1..$2FFF] of Byte;
    Var
      t,tt,c,x,y : Word;
      Cont : Word;
      Block : Bl;
      Lx,Ly : LongInt;
      PalBMP : Array[1..1024] of Byte;
      F : File;
      FBegin : LongInt;
      P : PalR;
    Begin
      {$I-}
      Assign(F,Path);
      Reset(F,1);
      {$I+}
      If IOResult<>0 Then Exit;


      Seek(F,18);
      BlockRead(F,Lx,4);
      Seek(F,22);
      BlockRead(F,Ly,4);

      Seek(F,54);
{      BlockRead(F,PalBMP,1024);

      Seek(F,10);
      BlockRead(F,FBegin,4);

      Seek(F,FBegin); }
      x:=1;
      y:=Ly;


{      For t:=0 to 256 Do
        Begin
          P[t].R:=0;
          P[t].G:=0;
          P[t].B:=0;
        End;
      SetPal(P);  }

{      c:=1;
      For t:=0 to 256 do
        Begin
          P[t].R:=PalBMP[c+2] SHR 2;
          P[t].G:=PalBMP[c+1] SHR 2;
          P[t].B:=PalBMP[c] SHR 2;
          c:=c+4;
        End;
      SetPal(P); }


   If (Lx MOD 2)<>0 Then Lx:=Lx+1;




      While NOT Eof(F) do
        Begin
          tt:=1;
          BlockRead(F,Block,$2FFF+1,Cont);
          If Cont<>0 Then
            For t:= 1 to Round(Cont/3)  do
              Begin
                Plot16M(Px+x,Py+y,Block[tt],Block[tt+1],Block[tt+2],640);
                tt:=tt+3;
                x:=x+1;
                If x>Lx Then
                  Begin
                    x:=1;
                    y:=y-1;
                  End;
              End;
        End;

      c:=1;
      For t:=0 to 256 do
        Begin
          P[t].R:=PalBMP[c+2] SHR 2;
          P[t].G:=PalBMP[c+3] SHR 2;
          P[t].B:=PalBMP[c+1] SHR 2;
          c:=c+4;
        End;
      SetPal(P);

      Close(F);

    End;



   Procedure SetPal(HUE : PalR);
     Var
       Reg : Registers;
     Begin
       With Reg Do
         Begin
           AH := $10;
           AL := $12;
           BX := 16;
           CX := 256-16;
           ES := Seg(HUE);
           DX := Ofs(HUE);
         End;
       Intr($10,Reg);
     End;

     Procedure RolaWUp(N,x1,y1,x2,y2 : Byte);
       Var
         R : Registers;
       Begin
         With R Do
           Begin
             AH := 6;
             AL := N;
             BH := 1;
             CL := x1;
             CH := y1;
             DL := x2;
             DH := y2;
           End;
         Intr($10,R);
       End;

     Procedure RolaWDown(N,x1,y1,x2,y2 : Byte);
       Var
         R : Registers;
       Begin
         With R Do
           Begin
             AH := 7;
             AL := N;
             BH := 1;
             CL := x1;
             CH := y1;
             DL := x2;
             DH := y2;
           End;
         Intr($10,R);
       End;

     Procedure Beep(Freq,Dela : Word);
       Begin
         Sound(Freq);
         Delay(Dela);
         NoSound;
       End;

    Procedure Botao(x1,y1,x2,y2 : Word; Out : Boolean; Tam : Byte);
      Var
        t : Byte;
      Begin
        If Out Then
          Begin
            SetColor(White);
            For t:=0 to Tam Do
              Begin
                Line(x1+t,y2-t,x1+t,y1+t);
                Line(x1+t,y1+t,x2-t,y1+t);
              End;
            SetColor(DarkGray);
            For t:=0 to Tam Do
              Begin
                Line(x2-t,y1+t,x2-t,y2-t);
                Line(x2-t,y2-t,x1+t,y2-t);
              End;
            SetFillStyle(1,LightGray);
            Bar(x1+Tam,y1+Tam,x2-Tam,y2-Tam);
          End
        Else
          Begin
            SetColor(DarkGray);
            For t:=0 to Tam Do
              Begin
                Line(x1+t,y2-t,x1+t,y1+t);
                Line(x1+t,y1+t,x2-t,y1+t);
              End;
            SetColor(White);
            For t:=0 to Tam Do
              Begin
                Line(x2-t,y1+t,x2-t,y2-t);
                Line(x2-t,y2-t,x1+t,y2-t);
              End;
            SetFillStyle(1,0);
            Bar(x1+Tam,y1+Tam,x2-Tam,y2-Tam);
          End;
      End;


    Procedure GetWindow(x1,y1,x2,y2 : Byte; Var W : WinType);
       Var
         trash,trash2,c : Word;
       Begin
         c:=160*y1+(2*x1);
         W.x:=Abs(x2-x1);
         W.y:=Abs(y2-y1);
         For trash2:= 1 to W.y Do
           Begin
             For trash:= 1 to W.x Do
               Begin
                 W.Win[trash,trash2]:=0;
                 W.Win[trash,trash2]:=(Mem[$B800:c+1] shl 8)+Mem[$B800:c];
                 c:=c+2;
               End;
             c:=c+(2*(80-x2))+(2*x1);
           End;
       End;


    Procedure WriteWindow(x,y : Byte; W : WinType);
       Var
         trash,trash2,c : Word;
       Begin
         c:=(160*y)+2*x;
         For trash2:= 1 to W.y Do
           Begin
             For trash:= 1 to W.x Do
               Begin
                 Mem[$B800:c]:=Lo(W.Win[trash,trash2]);
                 Mem[$B800:c+1]:=Hi(W.Win[trash,trash2]);
                 c:=c+2;
               End;
             c:=c+2*(80-(x+W.x))+2*x;
           End;
       End;

    Procedure Plot(x,y : Word; Color : Byte; MaxX : LongInt);
      Var
        L : LongInt;
        Page,Desloc : Word;
      Begin
        L:=((y-1)*MaxX)+x-1;
        Page:=L DIV $10000;
        Desloc:=Word(L);
        If Page<>OldPage Then
          Port[$3D8]:=Page;
        Mem[$A000:Desloc]:=Color;
        OldPage:=Page;
      End;

    Procedure SetMode(Mode : Word);
      Var
        R : Registers;
      Begin
        R.AH:=0;
        R.AL:=Mode;
        Intr($10,R);
      End;

    Procedure Plot16M(x,y : Word; R,G,B : Byte; M : LongInt);
      Const
        MaxX : LongInt = 640;
      Var
        L : LongInt;
        Page,Desloc : Word;
      Begin
        L:=(((y-1)*3*M)+(x-1)*3);
        Page:=L DIV $10000;
        Desloc:=Word(L);
        Port[$3D8]:=Page;
        Mem[$A000:Desloc]:=R;
        Mem[$A000:Desloc+1]:=G;
        Mem[$A000:Desloc+2]:=B;
        OldPage:=Page;
      End;


    Procedure Plot64K(x,y : Word; Color : Word);
      Const
        MaxX : LongInt = 640;
      Var
        L : LongInt;
        Page,Desloc : Word;
      Begin
        L:=((y-1)*2*MaxX)+(x)*2;
        Page:=L DIV $10000;
        Desloc:=Word(L);
        If Page<>OldPage Then
          Port[$3D8]:=Page;
        Mem[$A000:Desloc]:=Hi(Color);
        Mem[$A000:Desloc-1]:=Lo(Color);
        OldPage:=Page;
      End;


Begin
  OldPage:=0;
  Port[$3D8]:=OldPage;
End.
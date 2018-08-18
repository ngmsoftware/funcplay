{---------------------------------------------------------------------------}
{ Funcplay V2.0á by Ningu‚m                                                 }
{                                                                           }
{  O FuncPlay ‚ um "Plotador" de fun‡”es simples que serve de               }
{ labotar¢rio para estudo do comportamento de fuc”es simples.               }
{                                                                           }
{   Altor : Ningu‚m                                                         }
{   e-mail : ngmchaos@ncc.ufrn.br                                           }
{---------------------------------------------------------------------------}


  Program Funcplay2;

    {$F+}
    {$N+}
    {$E+}
    {$M 16384,0,655360}
    Uses
      Dos,Crt,NgMGraph,NgMMath,Mouse,Graph,NgMObj;

    Type
      BitMap=Array[1..100,1..100] of Byte;

      ArrayIcoMatrix = Array[0..25] of IcoMatrix;

      FArray = Array[1..600] of Record
                                  x,y : Real;
                                End;
      Main=object
        Im : Pointer;
        Funcao : ^FArray;
        Ico : IcoMatrix;
    {    Img : BitMap; }
        Ax1,Ay1,Ax2,Ay2 : Word;
        OnLine : String;
        EvExit : Boolean;
        Icon : ^ArrayIcoMatrix;
        Medidor : Meter;
     { 2 - Vari veis da  rea que mostra a fun‡„o }
        TelaX1,TelaY1,TelaX2,TelaY2 : Word;
        MODE : Byte;
        AxisColor : Byte;
        FuncColor : Byte;
        Func1,Func2,Func3 : String;
        LimX,LimY : Real;
        Rot3D,Tlt3D : Integer;
        ObsPosX,ObsPosY,ObsPosZ, Dist : Real;
     { 3 - Vari veis da barra de bot”es }
        BarraX1,BarraY1,BarraX2,BarraY2 : Word;
        NumBot : Byte;
        Pb : Array[0..25] of PBotao;
        OpenAD : Boolean;
        Adx,Ady,AdNumBot  : Word;
        OldOnLine : String;
     { 4 - Vari veis da  rea de comunica‡„o c/ o usu rio }
        AcX1,AcY1,AcX2,AcY2 : Word;
        CStr : String;
     { 5 - Status }
        StrStatus : String;

      {1}
        Constructor Init(x1,y1,x2,y2 : Word);  {Inicia e desenha a tela principal }
        Procedure RUN;
        Procedure ModifyMode(M : Byte);
        Procedure ModifyOnLine(Var S : String);
        Procedure SetDefault;
        Procedure About;
        Procedure DoEvent(Ev : Byte);
      {2}
        Procedure InitTela(x1,y1,x2,y2 : Word; Att : Byte);
        Procedure DrawTela;
        Procedure DrawFunc;
        Procedure DrawFunc2D;
        Procedure DrawFunc3D;

        Procedure DrawFuncPolar2D;
        Procedure DrawFuncPolar3D;
        Procedure SetPerpective;

        Procedure GetLimites;

        Procedure DrawDerivada;
        Procedure EstimateIntegral(x1,x2 : Real);
        Procedure CalcLimites(xo : Real);
       {3}
        Procedure InitBotaoBar(x1,y1 : Word;Num : Byte);
        Procedure DrawBotaoBar;
        procedure CreateAdvanced(x,y : Word; N : Byte);
        procedure DestroiAdvanced(x,y : Word; N : Byte);
       {4}
        Procedure InitComBar(x1,y1,x2,y2 : Word);
        Procedure DrawComBar;
        Procedure PutString(Line,Col : Byte; Mess : String);
        Function GetString(Var Mess : String) : String;
       {5}
        Procedure Status;
        {6}
        Procedure Calculator;

        Destructor Done;

      End;

  Var
    Velha_Int_08h : Pointer;
    M : MouseLoc;
    Mou : MouReset;

{ Rotina para finalizar o programa }

      Procedure Fim(Mess : String);
        Begin
          CloseGraph;
          RestoreCRTMode;
          Write(Mess);
          SwapVectors;
          SetIntVec($08,Velha_Int_08h);
          SwapVectors;
          Halt;
        End;


{ Rotina para deixar o mouse residente }

       Procedure STI;
         Inline($FB);
       Procedure CLI;
         Inline($FA);
       Procedure CallP(P : Pointer);
         Begin
           Asm
             PUSHF
             CALL DWORD PTR [bp+06]
           End
         End;

    Procedure Int_08h(Flags,CS,IP,AX,BX,CX,DX,
                SI,DI,DS,ES,BP : Word); Interrupt;
      Begin
        CLI;
        CallP(Velha_Int_08h);
        MPos(M);
        STI;
      End;

{-------------------------------------}
{------Procedures de uso normal-------}

      Procedure DrawCube(L : Word);
        Begin
          Line3D(L,L,L,L,L,-L);
          Line3D(L,L,L,L,-L,L);
          Line3D(L,L,L,-L,L,L);
          Line3D(-L,-L,-L,L,-L,-L);
          Line3D(-L,-L,-L,-L,-L,L);
          Line3D(-L,-L,-L,-L,L,-L);
          Line3D(L,-L,-L,L,-L,L);
          Line3D(L,-L,-L,L,L,-L);
          Line3D(-L,L,L,-L,-L,L);
          Line3D(-L,L,L,-L,L,-L);
          Line3D(L,-L,L,-L,-L,L);
          Line3D(L,L,-L,-L,L,-L);
          Line3D(-L,0,0,0,0,0);
          Line3D(0,0,0,-L DIV 2,L DIV 2,0);
          Line3D(0,0,0,-L DIV 2,-L DIV 2,0);
        End;

     Procedure Escreva(tip : Byte; x,y : Word; Var S : String);
       Begin
         SetTextJustify(LeftText,TopText);
         Case tip of
           1 : SetTextStyle(SmallFont,HorizDir,4);
           2 : SetTextStyle(DefaultFont,HorizDir,1);
           3 : SetTextStyle(TriplexFont,HorizDir,1);
         End;
         OutTextXY(x,y,S);
       End;

{------------------------------------------------------------------}

      Procedure Main.Status;
        Begin
          StrStatus:='Status : '+StrStatus;
          SetColor(White);
          SetFillStyle(SolidFill,LightRed);
          Bar(0,465,640,480);
          Escreva(2,3,470,StrStatus);
        End;

      Procedure Main.CreateAdvanced(x,y : Word; N : Byte);
        Const
          OL : Array[1..10] of String = ('Plotar derivada','Estimar integral','Calcular limites'
          ,'3','4','5','6','7','8','9');
        Var
          t,xx,yy,dx,dy : Word;
        Begin
          OpenAD:=TRUE;
          MHide;
          NumBot:=NumBot+N;
          xx:=10;
          yy:=10;
          dy:=10+50*(N DIV 5);
          If N>5 Then
            dx:=260
          Else
            dx:=10+N*50;

          GetMem(Im,ImageSize(x,y,x+dx,y+dy));
          GetImage(x,y,x+dx,y+dy,Im^);

          Botao(x,y,x+dx,y+dy,TRUE,3);
          Botao(x+5,y+5,x+dx-5,y+dy-5,FALSE,3);

          For t:=1 to N Do
            Begin
              Pb[10+t].Init(x+xx,y+yy,x+xx+40,y+yy+40,10+t,OL[t],Icon^[10+t]);
              xx:=xx+50;
              If xx>250 Then
                Begin
                  xx:=10;
                  yy:=yy+50;
                End;
            End;
          MShow;
        End;

      Procedure Main.DestroiAdvanced(x,y : Word; N : Byte);
        Var
          dx,dy : Word;
        Begin
          OpenAD:=FALSE;
          MHide;
          NumBot:=NumBot-N;
          dy:=10+50*(N DIV 5);
          If N>5 Then
            dx:=260
          Else
            dx:=10+N*50;
          PutImage(x,y,Im^,CopyPut);
          FreeMem(Im,ImageSize(x,y,x+dx,y+dy));
          MShow;
        End;



      Procedure Main.ModifyMode(M : Byte);
        Var
          MStr : String;
        Begin
          StrStatus:='Modificando modo...';
          Status;
          If M>4 Then M:=1;
          MODE:=M;
          SetViewPort(0,0,GetMaxX,GetMaxY,TRUE);
          BB(0,365,180,400,Blue);
          MStr:='Modo :';
          SetColor(White);
          Escreva(2,5,380,MStr);
          Case M of
            1 : MStr:='Cartesiano 2D';
            2 : Mstr:='Cartesiano 3D';
            3 : MStr:='Polar 2D';
            4 : MStr:='Polar 3D';
            5 : MStr:='Reservado';
          End;

          Escreva(2,5,390,MStr);
          StrStatus:='Esperando comando';
          Status;
        End;

      Procedure Main.ModifyOnLine(Var S : String);
        Var
          MStr : String;
        Begin
          If S<>OldOnLine Then
            Begin
              BB(1,400,180,465,Blue);
              MStr:='Ajuda "on Line" :';
              SetColor(White);
              Escreva(2,5,420,S);
              Escreva(2,5,410,Mstr);
              OldOnLine:=S;
            End;

        End;

      Procedure Main.SetDefault;
        Begin
          AxisColor:=LightBlue;
          FuncColor:=LightGreen;
          Func1:='x';
          Func2:='Sen(x)';
          Func3:='0';
          LimX:=pi; LimY:=Pi;
          Mx:=1; My:=1;
          Rot3D:=25; Tlt3D:=20;
          ObsPosX:=0; ObsPosY:=0; ObsPosZ:=-3000;
          Dist:=5000;
        End;

      Constructor Main.Init(x1,y1,x2,y2 : Word);  {Inicia e desenha a tela principal }
        Var
          Gm,Gd,t : Integer;
          OnLineStr : String;

        Procedure GetIniFile;
          Var
            Fileini : Text;
            scan : String;
            err : Integer;
            V1 : Real;
          Begin
            {$I-}
            Assign(FileIni,'funcplay.ini');
            Reset(FileIni);
            {$I+}
            If IOResult<>0 Then
              Exit;
            While NOT Eof(FileIni) do
              Begin
                ReadLN(FileIni,scan);
                If scan[1]<>';' Then
                  Begin
                    If Copy(scan,1,9)='AxisColor' Then
                      Begin
                        scan:=Copy(scan,11,Length(scan));
                        AxisColor:=Round(Solv(scan,v1,v1));
                      End;
                    If Copy(scan,1,9)='FuncColor' Then
                      Begin
                        scan:=Copy(scan,11,Length(scan));
                        FuncColor:=Round(Solv(scan,v1,v1));
                      End;
                    If Copy(scan,1,5)='Func1' Then
                      Begin
                        scan:=Copy(scan,7,Length(scan));
                        Func1:=scan;
                      End;
                    If Copy(scan,1,5)='Func2' Then
                      Begin
                        scan:=Copy(scan,7,Length(scan));
                        Func2:=scan;
                      End;
                    If Copy(scan,1,5)='Func3' Then
                      Begin
                        scan:=Copy(scan,7,Length(scan));
                        Func3:=scan;
                      End;
                    If Copy(scan,1,7)='LimiteX' Then
                      Begin
                        scan:=Copy(scan,9,Length(scan));
                        LimX:=Solv(scan,v1,v1);
                      End;
                    If Copy(scan,1,7)='LimiteY' Then
                      Begin
                        scan:=Copy(scan,9,Length(scan));
                        LimY:=Solv(scan,v1,v1);
                      End;
                    If Copy(scan,1,9)='ObserverX' Then
                      Begin
                        scan:=Copy(scan,11,Length(scan));
                        ObsPosx:=Solv(scan,v1,v1);
                      End;
                    If Copy(scan,1,9)='ObserverY' Then
                      Begin
                        scan:=Copy(scan,11,Length(scan));
                        ObsPosy:=Solv(scan,v1,v1);
                      End;
                    If Copy(scan,1,9)='ObserverZ' Then
                      Begin
                        scan:=Copy(scan,11,Length(scan));
                        ObsPosz:=Solv(scan,v1,v1);
                      End;
                    If Copy(scan,1,8)='Rotation' Then
                      Begin
                        scan:=Copy(scan,10,Length(scan));
                        Rot3D:=Round(Solv(scan,v1,v1));
                      End;
                    If Copy(scan,1,4)='Tilt' Then
                      Begin
                        scan:=Copy(scan,6,Length(scan));
                        Tlt3D:=Round(Solv(scan,v1,v1));
                      End;
                    If Copy(scan,1,4)='Dist' Then
                      Begin
                        scan:=Copy(scan,6,Length(scan));
                        Dist:=Solv(scan,v1,v1);
                      End;

                  End;
              End;
          End;

        Const
          Ic : Array[1..25] of String = ('Exit.ico','persp.ico','lim.ico','mode.ico'
,'getfun.ico','draw.ico','cls.ico','calca.ico','calc.ico','turbo.ico','deriv.ico','integ.ico','limi.ico',
'turbo.ico','turbo.ico','turbo.ico','lim.ico','lim.ico','lim.ico','lim.ico','lim.ico'
,'lim.ico','lim.ico','lim.ico','lim.ico');

        Begin
         SetDefault;
         GetIniFile;

          New(Icon);
          New(Funcao);
          For t:=1 to 25 Do
            Begin
              GetIco(Icon^[t],Ic[t]);
            End;
          StrStatus:='Iniciando tela principal...';
{          SetDefault; }
          EvExit := FALSE;
          OldOnLine:='';
          Ax1:=x1; Ax2:=x2; Ay1:=y1; Ay2:=y2;
{          InitPloting(Rot3D,Tlt3D,320,240,1,1,640,480,FALSE);    }
          InitPerspective(TRUE,ObsPosX,ObsPosY,ObsPosZ,Dist);
          DetectGraph(Gd,Gm);
          InitGraph(Gd,Gm,'c:\lng\tp70\bgi');
          If GraphResult<>0 Then
            Begin
              Dispose(Icon);
              Dispose(Funcao);
              Fim('Erro. Modo grafico nao iniciado');
            End;
          SetFillStyle(SolidFill,LightGray);
          FloodFill(1,1,1);
          BB(0,365,170,465,Blue);
          Status;
          ModifyMode(         1            );
          OnlineStr:=' ';
          ModifyOnLine(OnLineStr);
          ModifyOnLine(OnLineStr);
          OpenAD:=FALSE;
          Adx:=200;
          Ady:=100;
          AdNumBot:=10;
          Medidor.Init(130,300,2*LimY,0,50);
          Medidor.Draw;
      End;

      Procedure Main.Calculator;
        Var
          S : String;
          Value,v : Extended;
        Begin
          StrStatus:='Usando a calculadora';
          Status;
          S:='Diga a exprecao a ser resolvida';
          Value:=Solv(GetString(S),v,v);
          Str(Value:3:3,S);
          S:='Resultado : '+S;
          PutString(2,2,S);
          StrStatus:='Esperando comando';
          Status;
        End;


      Procedure Main.About;
        Var
          S : String;
        Begin
          Str(MaxAvail,S);
          S:='Memoria livre : '+S;
          PutString(1,1,S);
        End;

{---------------------------------------------}
      Procedure Main.DrawTela;
        Const
          Li : String= 'Limites :';
        Var
          LimStr,S : String;
          tt_t : word;
          ang : real;

        Procedure DrawExample;
          Begin
            SetViewPort(TelaX2-80,TelaY1+10,TelaX2-10,TelaY1+70,TRUE);
            InitPloting(Rot3D,Tlt3D,25,25);
            InitPerspective(TRUE,0,0,3000,5000);
            SetColor(Green);
            DrawCube(10);
            InitPloting(Rot3D,Tlt3D,(TelaX2-TelaX1)DIV 2,(TelaY2-TelaY1)DIV 2);
            InitPerspective(TRUE,0,0,3000,5000);
            SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          End;

       Procedure C2D(d : Boolean);

         Begin
           If D Then
             Begin
               SetColor(AxisColor);
               Line(1,(TelaY2-TelaY1) DIV 2,(TelaX2-TelaX1),(TelaY2-TelaY1) DIV 2);
               Line((TelaX2-TelaX1) DIV 2,1,(TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1));
             End;

           SetColor(Yellow);
           Escreva(1,2,2,Li);
           Str(LimX:3:2,S);
           LimStr:='X : '+S;
           Str(LimY:3:2,S);
           LimStr:=LimStr+' Y : '+S;
           Escreva(1,2,12,LimStr);
         End;

        Begin
          MHide;

          Botao(TelaX1,TelaY1,TelaX2,TelaY2,FALSE,4);
          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);

         if mode=1 then
           begin

         setlinestyle(1,1,1);
         setcolor(lightgray);
         for tt_t:= 0 to (TelaX2-TelaX1) do
          if ((tt_t mod 30)=0) then line(5+tt_t,30,5+tt_t,(Telay2-Telay1));
         line(0,30,(Telax2-Telax1),30);
         for tt_t:= 30 to (TelaX2-TelaX1) do
          if ((tt_t mod 29)=0) then line(0,tt_t-4,(Telax2-Telax1),tt_t-4);
         setlinestyle(0,0,0);

         setlinestyle(1,1,1);
         setcolor(darkgray);
         for tt_t:= 0 to (TelaX2-TelaX1) do
          if ((tt_t mod 30)=0) then line(5+tt_t+15,30,5+tt_t+15,(Telay2-Telay1));
         for tt_t:= 30 to (TelaX2-TelaX1) do
          if ((tt_t mod 29)=0) then line(0,tt_t-4+15,(Telax2-Telax1),tt_t-4+15);
         setlinestyle(0,0,0);
            end;

         setcolor(darkgray);
         setlinestyle(1,1,1);
         if mode=3 then
           begin
             ang:=0;
             while (ang<2*pi)do
               begin
                 line((Telax2-Telax1)div 2,(Telay2-Telay1)div 2,(Telax2-Telax1)div 2+round(150*cos(ang)),(Telay2-Telay1)div 2+
                   round(150*sin(ang)));
                 ang:=ang+pi/8;
               end;
            circle((TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2,30);
            circle((TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2,60);
            circle((TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2,90);
            circle((TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2,120);
            circle((TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2,150);

           end;
         setlinestyle(0,0,0);


          Case MODE of
            1 : Begin
                  C2D(TRUE);
                  S:='f(x)='+Func1;
                  Escreva(1,((TelaX2-TelaX1) DIV 2)+10,2,S);
                  S:='f(y)='+Func2;
                  Escreva(1,((TelaX2-TelaX1) DIV 2)+10,12,S);

                End;

            2 : Begin
                  C2D(FALSE);
                  InitPloting(Rot3D,Tlt3D,(TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2);
                  InitPerspective(TRUE,ObsPosX,ObsPosY,ObsPosZ,Dist);
                  SetColor(AxisColor);
                  Line3D(-100,0,0,100,0,0);
                  Line3D(0,-100,0,0,100,0);
                  Line3D(0,0,-100,0,0,100);
             {     BB(1,1,640,25,Black);  }
                  SetColor(Yellow);
{                  S:='Eixo X - f(x)='+Func1;
                  Escreva(1,12,2,S);
                  S:='Eixo Y - f(y)='+Func2;
                  Escreva(1,12,12,S);    }
                  S:='f(z)='+Func1;
                  Escreva(1,((TelaX2-TelaX1) DIV 2)+10,2,S);
                  DrawExample;
                End;
            3 : Begin
                  C2D(TRUE);
                  S:='f(r)='+Func1;
                  Escreva(1,((TelaX2-TelaX1) DIV 2)+10,2,S);
{                  S:='Fun‡„o do ƒngulo - f(a)='+Func2;
                  Escreva(1,((TelaX2-TelaX1) DIV 2)+10,12,S); }
                  SetColor(AxisColor);
                End;
            4 : Begin
                  C2D(FALSE);
                  InitPloting(Rot3D,Tlt3D,(TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2);
                  InitPerspective(TRUE,ObsPosX,ObsPosY,ObsPosZ,Dist);
                  SetColor(AxisColor);
                  Line3D(-100,0,0,100,0,0);
                  Line3D(0,-100,0,0,100,0);
                  Line3D(0,0,-100,0,0,100);
{                  BB(1,1,640,25,Black); }
                  SetColor(Yellow);
                  S:='f(r)='+Func1;
                  Escreva(1,((TelaX2-TelaX1) DIV 2)+10,2,S);
{                  S:='Fun‡„o do ƒngulo - f(a)='+Func1;
                  Escreva(1,12,12,S); }
                  DrawExample;

                End;
          End;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          MShow;
        End;

{--------------------- Calculo Avancado -------------------------}

      Procedure Main.CalcLimites(xo : Real);
        Var
          t : Word;
          Lim,LimE,LimD : Real;
          S : String;
        Begin
          MHide;
          StrStatus:='Calculando limites...';
          Status;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);

          t:=1+Round(((Telax2-Telax1)/2)+(xo*((telax2-telax1)/2)/Limx));

          LimE:=Solv(Func2,xo+1E-5,xo);
          LimD:=Solv(Func2,xo-1E-5,xo);
{          Str(LimE:3:3,S);
          S:='Limite a esquerda : '+S;
          PutString(1,1,S);
          Str(LimD:3:3,S);
          S:='Limite a direita : '+S;
          PutString(2,1,S);  }
          Lim:=Solv(Func2,xo,xo);
          Str(Lim:3:3,S);

          If Abs(LimE-LimD)<0.0001 Then
            Begin
              PutString(1,1,'O Limite ‚ '+S);
              SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
              Line(t,Round(Funcao^[t].y),t,(Telay2-Telay1)DIV 2);
              Line(t,Round(Funcao^[t].y),(Telax2-Telax1)DIV 2,Round(Funcao^[t].y));
            End
          Else
            PutString(3,1,'Nao existe o limite');


          StrStatus:='Esperando comando';
          Status;
          MShow;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          End;


      Procedure Main.EstimateIntegral(x1,x2 : Real);
        Var
          Integral,x,IncX : Extended;
          t,t1,t2 : Integer;
          sint : String;
        Begin
          MHide;
          StrStatus:='Integrando...';
          Status;

          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          Integral:=0;
          t1:=Round((x1*((TelaX2-TelaX1)/2)/LimX)+(Telax2-Telax1)/2);
          t2:=Round((x2*((TelaX2-TelaX1)/2)/LimX)+(Telax2-Telax1)/2);
          If (x1<x2) Then
            x:=x1
          Else
            x:=x2;
          IncX:=LimX/((telax2-telax1)/2);

          SetColor(White);
          For t:=Min(t1,t2) to Max(t1,t2) Do
            Begin
              If t<>Min(t1,t2) Then
                Line(t,Round(Funcao^[t].y),t,(Telay2-Telay1)DIV 2);
              Integral:=Integral+Solv(Func2,x,x)*IncX;
              X:=x+IncX;
            End;

          DrawFunc2D;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);

          Str(x2:3:3,Sint);
          PutString(1,1,Sint);
          Sint:='  ô';
          PutString(2,1,Sint);

          Str(Integral:1:1,Sint);

          Sint:='  õ('+Func2+')dx'+'='+Sint;
          PutString(3,1,Sint);

          Str(x1:3:3,Sint);
          PutString(4,1,Sint);


          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          StrStatus:='Esperando comando';
          Status;
          MShow;
        End;


      Procedure Main.DrawDerivada;
        Var
          t : Word;
          Deriv : Extended;
          xa,x,y,ya : Word;
        Begin
          StrStatus:='Derivando...';
          Status;
          MHide;
          DrawFunc2D;

          SetColor(LightGray);
          xa:=0; ya:=0;
          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          For t:= 1 to (TelaX2-TelaX1) Do
            Begin
              x:=Round(Funcao^[t].x);
              Deriv:=(Funcao^[t+1].y-Funcao^[t].y)/(Funcao^[t+1].x-Funcao^[t].x);
              If (Deriv>1000) Then
                Deriv:=1000;
              If (Deriv<-1000) Then
                Deriv:=-1000;
              Deriv:=40*Deriv;
              y:=(TelaY2-TelaY1) DIV 2 + Round(Deriv);
              If t>2 Then Line(x,y,xa,ya);
              xa:=x;
              ya:=y;
            End;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          PutString(1,1,'Grafico de Dx('+Func2+')');
          MShow;
          StrStatus:='Esperando comando';
          Status;
        End;


{=========================================================================}


      Procedure Main.DrawFunc2D;
        Var
          t,Xc,Yc,Xa,Ya : LongInt;
          tt: Word;
          fxx,rx,ry,X,Y,temp,yt : Real;
        Begin
          tt:=0;
          StrStatus:='plotando funcao em 2D';
          Status;
          SetColor(FuncColor);
          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          Xa:=1; Ya:=(TelaY2-TelaY1) DIV 2;
          X:=-LimX; Y:=LimY;
          For t:= -((TelaX2-TelaX1) DIV 2) to ((TelaX2-TelaX1) DIV 2)  Do
            Begin
              X:=LimX*t/((TelaX2-TelaX1)/2) ;  Y:=LimY*t/((TelaX2-TelaX1)/2);
              fxx:=Solv(Func2,X,temp);
              rx:=(Solv(Func1,X,temp)/LimX)*((TelaX2-TelaX1)DIV 2)+((TelaX2-TelaX1) DIV 2);
              ry:=((TelaY2-TelaY1) DIV 2)-(fxx/LimY)*((TelaY2-TelaY1) DIV 2);

              Xc:=Round(rx);
              Yc:=Round(ry);
              Inc(tt);

              Funcao^[tt].x:=rx;
              Funcao^[tt].y:=ry;

              If Yc>10000 Then Yc:=ya;
              If Xc>10000 Then Xc:=xa;
              If Yc<-10000 Then Yc:=-ya;
              If Xc<-10000 Then Xc:=-xa;

              If (t <> (-((TelaX2-TelaX1) DIV 2)))AND((Abs(Yc)+Abs(Ya))<1000)
                AND((Abs(Xc)+Abs(Xa))<1000) Then Line(Xa,Ya,Xc,Yc);
              Xa:=Xc; Ya:=Yc;
            End;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          StrStatus:='Esperando comando';
          Status;
        End;

      Procedure Main.DrawFunc3D;
        Var
          xa,ya,za,x,y,z : Extended;
          Vx,Vy : Integer;
          Linha : Array[-25..25] of Record
                                     x,y,z : Real;
                                   End;
        Begin
          StrStatus:='Plotando funcao em 3D';
          Status;
          SetColor(FuncColor);
          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          InitPerspective(TRUE,0,0,1000,Dist);
          xa:=0; ya:=0; za:=0;
          For Vx:=-25 to 25 Do
            Begin
            For Vy:=-25 to 25 Do
              Begin
                If KeyPressed Then
                  Begin
                    SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
                    StrStatus:='Esperando comando';
                    Status;
                    Exit;
                  End;
                x:=2*Vx;
                y:=2*Vy;
                z:=Solv(Func1,LimX*x/50,LimY*y/50);
                If (Vy<>-25) Then
                  Begin
                    Line3D(x,y,z,xa,ya,za);
                  End;
                If Vx<>-25 Then
                  Line3D(x,y,z,Linha[vy].x,Linha[vy].y,Linha[vy].z);
                Linha[Vy].x:=x;
                Linha[vy].y:=y;
                Linha[vy].z:=z;
                xa:=x;
                ya:=y;
                za:=z;
              End;
            End;

          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          StrStatus:='Esperando comando';
          Status;
        End;

      Procedure Main.DrawFuncPolar2D;
        Var
          t : Integer;
          R,Ang,tt,Ra,AngA : Extended;
        Begin
          Ra:=0;
          AngA:=0;
          StrStatus:='Plotando funcao polar em 2D';
          Status;
          SetColor(FuncColor);
          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          tt:=0;
          For t:=0 to 200 Do
            Begin
              Ang:=Pi*2*t/100;
              R:=100*Solv(Func1,Ang,tt);
              If t<>0 Then
                LinePolar(R,Ang,Ra,AngA,(Telax2-TelaX1)DIV 2,(TelaY2-TelaY1)DIV 2);
              Ra:=R;
              AngA:=Ang;
            End;
          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          StrStatus:='Esperando comando';
          Status;
        End;

      Procedure Main.DrawFuncPolar3D;
        Begin
        End;

      Procedure Main.DrawFunc;
        Begin
          Case MODE of
            1 : DrawFunc2D;
            2 : DrawFunc3D;
            3 : DrawFuncPolar2D;
          End;
        End;

      Procedure Main.SetPerpective;
        Var
          k : Char;
          tt : Word;
          A,T : Integer;
          D : Real;
        Begin
          MHide;
          StrStatus:='Ajustando perpectiva...';
          Status;
          A:=Rot3D;T:=Tlt3D;
          D:=Dist;
          k:=#0;
          SetColor(White);
          SetViewPort(TelaX1+5,TelaY1+5,TelaX2-5,TelaY2-5,TRUE);
          ClearViewPort;
          InitPloting(A+180,180+T,(TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2);
          InitPerspective(TRUE,0,0,1000,D);
          While k<>#13 Do
            Begin

              DrawCube(50);

              K:=ReadKey;
              SetColor(0);

              DrawCube(50);

              If K=#0 Then K:=ReadKey;
              Case K of
                 #77 : Begin
                         A := A+1;
                         If A>360 Then A:=0;
                       End;
                 #75 : Begin
                         A := A-1;
                         If A<0 Then A:=360;
                       End;
                 #72 : Begin
                         T := T+1;
                         If T>360 Then T:=0;
                       End;
                 #80 : Begin
                         T := T-1;
                         If T<0 Then T:=360;
                       End;
                 '+' : Begin
                         D := D+10;
                         If D>4000 Then D:=0;
                       End;
                 '-' : Begin
                         D := D-10;
                         If D<0 Then D:=4000;
                       End;


              End;
              InitPloting(A+180,180+T,(TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2);
              InitPerspective(TRUE,0,0,1000,D);
              SetColor(15);
            End;

          DrawCube(50);

          SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
          InitPloting(A+180,180+T,(TelaX2-TelaX1) DIV 2,(TelaY2-TelaY1) DIV 2);
          InitPerspective(TRUE,0,0,1000,D);
          Rot3D:=A+180; Tlt3D:=180+T; Dist:=D;
          StrStatus:='Esperando comando';
          Status;
          MShow;
        End;


      Procedure Main.GetLimites;
        Var
          temp : Real;
          s : String;
        Begin
          StrStatus:='Tomando limites';
          Status;
          S:='Diga o limite X :';
          LimX:=Solv(GetString(S),temp,temp);
          S:='Diga limite Y :';
          LimY:=Solv(GetString(S),temp,temp);
          StrStatus:='Esperando comando';
          Status;
          Medidor.ChangeValue(LimX,0);
        End;

      Procedure Main.InitTela(x1,y1,x2,y2 : Word; Att : Byte);
        Var
          temp : String;
          t : Word;
        Begin
          StrStatus:='Iniciando tela...';
          Status;
          GetIco(Ico,'pad.ico');
          Botao(5,5,170,55,FALSE,2);
          temp:='';
          Pb[0].Init(10,10,50,50,255,temp,Ico);
          SetColor(White);
          temp:=' Funcplay V2.0';
          OutTextXY(50,15,temp);
          temp:=' by Ninguem';
          OutTextXY(50,30,temp);

          TelaX1:=x1; TelaX2:=x2; TelaY1:=y1; TelaY2:=y2;
          DrawTela;
        End;

{---------------------------------------------}
      Procedure Main.InitBotaoBar(x1,y1 : Word;Num : Byte);
        Begin
          NumBot:=Num;
          StrStatus:='Iniciando barra de botoes...';
          Status;
          BarraX1:=x1; BarraY1:=y1;
          DrawBotaoBar;
        End;

      Procedure Main.DrawBotaoBar;
        Const
          StrTest : Array[1..16] of String = ('Sair','Ajustar perspectiva',
          'Ajustar Limites','Mudar de modo','Funcao','Plotar funcao','Cls','Op‡”es Avancado'
            ,'Calculadora','10','11','12','13','14','15','16');
        Var
          t : Byte;
          x,y : Word;
        Begin
          Botao(BarraX1,BarraY1,BarraX1+100,BarraY1+((NumBot DIV 2)+(NumBot MOD 2))*50,FALSE,3);
          t:=0; x:=BarraX1+5; y:=BarraY1+5;
          While (t<(NumBot-(NumBot MOD 2))) Do
            Begin
              Pb[t+1].Init(x,y,x+40,y+40,t,StrTest[t+1],Icon^[t+1]);
              Pb[t+2].Init(x+50,y,x+90,y+40,t+1,StrTest[t+2],Icon^[t+2]);
              y:=y+50;
              t:=t+2
            End;
          If (NumBot MOD 2)<>0 Then
              Pb[t+1].Init(x,y,x+40,y+40,t+1,StrTest[t+1],Icon^[t+1]);

        End;

{---------------------------------------------}
      Procedure Main.DrawComBar;
        Begin
          Botao(AcX1,AcY1,AcX2,AcY2,FALSE,3);
        End;

      Procedure Main.PutString(Line,Col : Byte; Mess : String);
        Begin
          BB((AcX1+Col*10)-1,(AcY1+Line*10)-1,AcX2-3,AcY2-3,0);
          SetColor(15);
          Escreva(2,AcX1+Col*10,AcY1+Line*10,Mess);
        End;

      Function Main.GetString(Var Mess : String) : String;
        Var
          S : String;
          TKey : Char;
          x : Word;
          Zero : Boolean;
        Begin
          MHide;
          PutString(2,3,'_');
          Zero:=FALSE;
          BB(AcX1+3,AcY1+3,AcX2-3,AcY2-3,0);
          S:='';
          TKey:=#0;
          x:=1;
          PutString(1,1,Mess);
          PutString(2,1,'-> ');
            While TKey<>#13 Do
              Begin
                TKey:=ReadKey;
                If TKey=#0 Then
                  Begin
                    Zero:=TRUE;
                    TKey:=ReadKey;
                  End;
                If TKey=#27 Then
                  Begin
                    GetString:='';
                    BB(AcX1+3,AcY1+3,AcX2-3,AcY2-3,0);
                    Exit;
                  End;
                If TKey=#8 Then
                  Begin
                     Zero:=FALSE;
                     PutString(2,3,'');
                     S:=Copy(S,1,Length(S)-1);
                     PutString(2,3,S);
                   End;
                If (TKey<>#8) AND (TKey<>#13) AND NOT Zero Then
                  Begin
                    Zero:=FALSE;
                    S:=S+Tkey;
                    PutString(2,3,S);
                  End;
              End;
          GetString:=S;
          BB(AcX1+3,AcY1+3,AcX2-3,AcY2-3,0);
          MShow;
        End;


      Procedure Main.InitComBar(x1,y1,x2,y2 : Word);
        Const
          C : String = 'Area de comunicacao';
        Begin
          StrStatus:='Iniciando barra de comunicacao...';
          Status;
          AcX1:=x1; AcX2:=x2; AcY1:=y1; AcY2:=y2;
          DrawComBar;
          PutString(1,1,C);
        End;

      Destructor Main.Done;
        Begin
          Dispose(Funcao);
          Dispose(Icon);
        End;

      Procedure Main.DoEvent(Ev : Byte);
        Var
          S : String;
          xx,x1,x2 : Real;
        Begin
          If NOT OpenAD Then
            Case Ev of
              255 : About;
              0 : Begin
                    EvExit:=TRUE;
                  End;
              1 : Begin
                    SetPerpective;
                    DrawTela;
                  End;
              2 : Begin
                    StrStatus:='Tomando limites';
                    Status;
                    GetLimites;
                    DrawTela;
                    StrStatus:='Esperando comandos';
                    Status;
                  End;
              3 : Begin
                    Delay(200);
                    ModifyMode(Mode+1);
                    DrawTela;
                  End;
              4 : Begin
                    StrStatus:='Tomando funcao';
                    Status;
                    Case Mode of
                      1 : Begin
                            S:='Diga a funcao do eixo X';
                            Func1:=GetString(S);
                            S:='Diga a funcao do eixo Y';
                            Func2:=GetString(S);
                          End;
                      2 : Begin
                            S:='Diga uma funcao do tipo z:=f(x,y)';
                            Func1:=GetString(S);
                          End;
                      3 : Begin
                            S:='Diga a funcao do raio';
                            Func1:=GetString(S);
                          End;
                      4 : Begin
                            S:='Diga a funcao do raio';
                            Func1:=GetString(S);
                          End;
                    End;
{                    DrawTela;  }
                    StrStatus:='Esperando comandos';
                    Status;
                  End;
              5 : Begin
                    DrawFunc;
                    Inc(FuncColor);
                    If FuncColor>15 Then FuncColor:=1;
                  End;
              6 : Begin
                    DrawTela;
                  End;
              7 : Begin
                    CreateAdvanced(Adx,Ady,AdNumBot)
                  End;
              8 : Calculator;
              9 : Beep(100,100);
              10 : Beep(100,100);
            End
          Else
            Begin
              DestroiAdvanced(Adx,Ady,AdNumBot);
              If MODE =1 Then
                Case Ev of
                  11 : Begin
                         DrawDerivada;
                       End;
                  12 : Begin
                         If LimX=LimY Then
                           Begin
                             S:='diga o x1 do intervalo';
                             x1:=Solv(GetString(S),xx,xx);
                             S:='diga o x2 do intervalo';
                             x2:=Solv(GetString(S),xx,xx);
                             EstimateIntegral(x1,x2);
                           End;
                       End;
                  13 : Begin
                         S:='Diga o valor de X';
                         CalcLimites(Solv(GetString(S),x1,x1));
                       End;
                  14 : Begin
                       End;
                  15 : Begin
                       End;
                  16 : Begin
                       End;
                  17 : Begin
                       End;
                  18 : Begin
                       End;
                  19 : Begin
                       End;
                  20 : Begin
                       End;
                End;
            End;
        End;
      Procedure Main.RUN;
        Var
          t : Byte;
          Cha : Char;
        Begin
          StrStatus:='Esperando comando';
          Status;
          While NOT EvExit Do
            Begin
              For t:= 0 to NumBot Do
                If (M.X>Pb[t].x1) AND (M.X<Pb[t].x2) AND (M.Y>Pb[t].y1) AND (M.Y<Pb[t].y2)
                  AND (M.BtStatus=1) Then
                    Begin
                      Pb[t].Aperte;
                      DoEvent(Pb[t].Event);
                    End
                Else
                  If (M.X>Pb[t].x1) AND (M.X<Pb[t].x2) AND (M.Y>Pb[t].y1) AND (M.Y<Pb[t].y2) Then
                    ModifyOnLine(Pb[t].OnLine);

              If KeyPressed then Cha:=ReadKey;
              If Cha=#27 Then DoEvent(0);
            End;
         End;

      Procedure InitGeral;
        Begin
          WriteLN;
          WriteLN(' FuncPlay V1.0 Ningu‚m Soft S/A');
          WriteLN('  Memoria disponivel : ',MaxAvail);
          SwapVectors;
          GetIntVec($08,Velha_Int_08h);
          SetIntVec($08,@Int_08h);
          SwapVectors;
          If MaxAvail<60000 Then
            Fim('Memoria insuficiente para executar o Funcplay');
          MReset(Mou);
          If NOT Mou.exists Then
            Fim('Erro , Driver de mouse nao encontrado');
          WriteLN('  Precione uma tecla para entrar...');
          ReadKey;
        End;

      Procedure AnimatePalette;
        Var
          t : Word;
        Begin
          For t:=16 to 255 do
            SetRGBPAlette(t,0,0,0);
        End;


 Var
   Prog : Main;
{   Txt : String; }

 Begin
   InitGeral;

{   SetMode($6c);

   GetBMP256t(0,0,'funcplay.bmp');

{   SetColor(White);
   SetTextStyle(10,HorizDir,2);
   Txt:='Funcplay V2.0 by Ninguem';
   OutTextXY(90,1,Txt);

   SetColor(Red);
   Txt:='Processador de funcoes Matematicas ';
   SetTextStyle(10,HorizDir,1);
   OutTextXY(50,435,Txt); }


{   ReadKey;
   AnimatePalette;
SetMode(3);
Write('Por favor, espere...');

   InitG(FALSE);
   CloseGraph;
   RestoreCRTMode; }


   Prog.Init(10,10,100,100);
   Prog.InitTela(200,10,630,350,3);
   Prog.InitBotaoBar(10,80,10);
   Prog.InitComBar(200,370,630,450);
   Prog.DrawFunc2D;

   MShow;
   Prog.RUN;
   Prog.Done;
   RestoreCRTMode;
   Fim('Obrigado por usar "Funcplay V2.0"');
 End.















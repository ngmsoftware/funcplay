
 Unit NgmObj;

   Interface

     Uses
       Crt,Dos,Graph,NgMGraph,NgmMath,Mouse;

     Type
       PBotao=Object
         x1,y1,x2,y2 : Word;
         Ico : IcoMatrix;
         Event : Byte;
         OnLine : String;
         Procedure Init(xx1,yy1,xx2,yy2 : Word; Ev : Byte; Var OnL : String;Var I : IcoMatrix);
         Procedure Aperte;
       End;

       Meter=Object
         Value : Real;
         x,y : Word;
         Range : Real;
         Tam : Word;
         Constructor Init(xi,yi : Word; Rangei,Vi : Real; Ti : Word);
         Procedure Draw; virtual;
         Procedure ChangeValue(NRange,Nv : Real); virtual;
         Destructor Done;
       End;

  Implementation

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
{---------------------------------------------------------------}

    Constructor Meter.Init(xi,yi : Word; Rangei,Vi : Real; Ti : Word);
      Begin
        x:=xi;
        y:=yi;
        Range:=Rangei;
        Value:=vi;
        Tam:=Ti;
      End;

    Procedure Meter.Draw;
      Var
        Ang : Real;
        Vp : ViewPortType;
      Begin
        GetViewSettings(Vp);
        Botao(x,y,x+Tam,y+Tam,TRUE,3);
        Botao(x+Tam DIV 10,y+Tam div 10,x+9*Tam div 10,Round(y+2*Tam/3),FALSE,2);
        SetViewPort(x+tam div 10,y+tam div 10,x+9*Tam DIV 10,Round(y+2*Tam/3)-2,ClipOn);

        SetColor(Green);
        Ellipse((8*Tam div 10) div 2,2*Tam DIV 3,90-45,90+45,Tam DIV 2,Tam DIV 2);

        SetColor(LightRed);
        Ang:=(-Value/Abs(Range/2))*Pi/4.5;
        Line((8*Tam div 10) div 2,2*Tam DIV 3,
        (8*Tam div 10) div 2+Round(Tam/2*Cos(Pi/2+ang)),Round(Tam*0.55)-Round(tam/2*Sin(Pi/2+Ang)));

        SetViewPort(Vp.x1,Vp.y1,Vp.x2,Vp.y2,Vp.Clip);
      End;

    Procedure Meter.ChangeValue(NRange,Nv : Real);
      Var
        Ang : Real;
        Vp : ViewPortType;
      Begin
        GetViewSettings(Vp);
        If (Abs(Nv)<(Range/2)) Then
          Begin
            Ang:=(-Value/Abs(Range/2))*Pi/4.5;
            SetViewPort(x+tam div 10,y+tam div 10,x+9*Tam DIV 10,Round(y+2*Tam/3)-2,ClipOn);

            SetColor(Green);
            Ellipse((8*Tam div 10) div 2,2*Tam DIV 3,90-45,90+45,Tam DIV 2,Tam DIV 2);

            SetColor(0);
            Line((8*Tam div 10) div 2,2*Tam DIV 3,
              (8*Tam div 10) div 2+Round(Tam/2*Cos(Pi/2+ang)),Round(Tam*0.55)-Round(tam/2*Sin(Pi/2+Ang)));
            Range:=NRange;
            Value:=Nv;
            SetColor(Red);
            Ang:=(-Value/Abs(Range/2))*Pi/4.5;
            Line((8*Tam div 10) div 2,2*Tam DIV 3,
              (8*Tam div 10) div 2+Round(Tam/2*Cos(Pi/2+ang)),Round(Tam*0.55)-Round(tam/2*Sin(Pi/2+Ang)));

             SetViewPort(Vp.x1,Vp.y1,Vp.x2,Vp.y2,Vp.Clip);
          End;
      End;

    Destructor Meter.Done;
      Begin
      End;

{---------------------------------------------------------------------------}

End.
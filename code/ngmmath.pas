
  Unit NgmMath;
  {$N+}

  Interface

    Uses
      Crt;

    Const
      Ln10       = 2.30258509299405E+000;
      PiOver180  = 1.74532925199433E-002;
      PiUnder180 = 5.72957795130823E+001;

    Type
      Vet = Record
              x,y : Extended;
            End;
      TDA = Array[0..2] of Extended;
      TDIA = Array[0..2] of Integer;
      FDA = Array[0..3] of Extended;
      Matriz4x4 = Array[0..3,0..3] of Extended;
      Complex = Record
                  Re : Extended;
                  Im : Extended;
                End;


    Function Solv(S : String; x,y : Extended) : Extended;
    Function Log(x : Extended) : Extended;
    Function ExpReal(Base, Expo : Extended) : Extended;
    Function Sign(x : Extended) : integer;
    Function IntSign(x : Integer) : integer;
            { Trigonometric }
    Function CosD(Angle : Extended) : Extended;
    Function SinD(Angle : Extended) : Extended;
    Function Tan(x : Extended) : Extended;
    Function CoTan(x : Extended) : Extended;
    Function Radians(Angle : Extended) : Extended;
    Function Degrees(Angle : Extended) : Extended;
    Function Cossec(x : Extended) : Extended;
    Function Sec(x : Extended) : Extended;
            { Hiperbolic }
    Function SinH(x : Extended) : Extended;
    Function CosH(x : Extended) : Extended;
    Function TanH(x : Extended) : Extended;
    Function CoTanH(x : Extended) : Extended;
            {Vectors}
    Procedure CalcAngle(P1,P2,P3 : Vet;Var Sen,Coss : Extended);
    Procedure Vec(r,s,t : Extended; Var A : TDA);
    Procedure UnVec(A : TDA; Var r,s,t : Extended);
    Procedure VecInt(r,s,t : Integer; Var A : TDIA);
    Procedure UnVecInt(A : TDIA; Var r,s,t : Integer);
    Function VecDot(A,B : TDA) : Extended;
    Procedure VecCross(A,B : TDA; Var C : TDA);
    Function VecLen(A : TDA) : Extended;
    Procedure VecNormalize(Var A : TDA);
    Procedure VecMatxMult(A : FDA; Matrix : Matriz4x4; Var B : FDA);
    Procedure VecSub(A,B : TDA; Var C : TDA);
    Procedure VecSubInt(A,B : TDIA; Var C : TDIA);
    Procedure VecAdd(A,B : TDA; Var C : TDA);
    Procedure VecAddInt(A,B : TDIA; Var C : TDIA);
    Procedure VecAdd3(A,B,C : TDA; Var D : TDA);
    Procedure VecLinComb(r : Extended; A : TDA; s : Extended; B : TDA; Var C : TDA);
    Procedure VecScalMult(r : Extended; A : TDA; Var B : TDA);
    Procedure VecScalMultI(r : Extended; A : TDIA; Var B : TDA);
    Procedure VecScalMultInt(r : Extended; A : TDA; Var B : TDIA);
    Procedure VecAddScalMult(r : Extended; A,B : TDA; Var C : TDA);
    Procedure VecNull(Var A : TDA);
    Procedure VecNullInt(Var A : TDIA);
    Procedure VecElemMult(r : Extended; A,B : TDA; Var C : TDA);
    Procedure Vec2Matrix(A : TDA; Var B : FDA);
    Procedure Matrix2Vec(A : FDA; Var B : TDA);
    Procedure VecCopy(A : TDA; Var B : TDA);
            {Matrix}
    Procedure ZeroMatrix(Var A : Matriz4x4);
    Procedure TranslateMatriz(tx,ty,tz : Extended; Var A : Matriz4x4);
    Procedure ScaleMatriz(sx,sy,sz : Extended; Var A : Matriz4x4);
    Procedure RotateMatriz(m : Byte; Theta : Extended; Var A : Matriz4x4);
    Procedure Translate3D(tx,ty,tz : Extended; A : TDA; Var  B : TDA);
    Procedure Scale3D(sx,sy,sz : Extended; A : TDA; Var  B : TDA);
    Procedure Rotate3D(E : Byte; Ang : Extended; A : TDA; Var  B : TDA);
    Procedure Multiply3DMatrizes(A,B : Matriz4x4; Var C : Matriz4x4);
    Procedure PrepareMartiz(tx,ty,tz,sx,sy,sz,rx,ry,rz : Extended; Var XForm : Matriz4x4);
    Procedure PrepareInvMartiz(tx,ty,tz,sx,sy,sz,rx,ry,rz : Extended; Var XForm : Matriz4x4);
    Procedure Transform(A : TDA; M : Matriz4x4; Var B : TDA);
            { Complexos }
    Procedure CSoma(X : Complex; Y : Complex; Var C : Complex);
    Procedure CMult(X : Complex; Y : Complex; Var C : Complex);
    Procedure CMultE(x : Complex; k : Extended;Var s : Complex);
    Procedure CDiv(x,y : Complex;Var s : Complex);
    Procedure Complexo(R : Extended; I : Extended; Var C : Complex);
    Procedure Show(X: Complex);


  Implementation
{------------------- Trigonometrics ----------------------------}

    Function Radians(Angle : Extended) : Extended;
      Begin
        Radians:=Angle*PiOver180;
      End;

    Function Degrees(Angle : Extended) : Extended;
      Begin
        Degrees:=Angle*PiUnder180;
      End;

    Function CosD(Angle : Extended) : Extended;
      Begin
        CosD:=Cos(Radians(Angle));
      End;

    Function SinD(Angle : Extended) : Extended;
      Begin
        SinD:=Sin(Radians(Angle));
      End;

    Function Tan(x : Extended) : Extended;
      Begin
        Tan:=Sin(x)/(Cos(x)+1E-10);
      End;

    Function CoTan(x : Extended) : Extended;
      Begin
        CoTan:=Cos(x)/(Sin(x)+1E-10);
      End;

    Function Cossec(x : Extended) : Extended;
      Begin
        Cossec:=1/(Sin(x)+1E-10);
      End;

    Function Sec(x : Extended) : Extended;
      Begin
        Sec:=1/(Cos(x)+1E-10);
      End;

{-------------------------- Exponencials ---------------------------}


    Function Log(x : Extended) : Extended;
      Begin
        If x=0 Then x:=0.0000000001;
        If x>0 Then
          Log:=Ln(x)/Ln10
        Else
          Log:=0;
      End;

    Function Sign(x : Extended) : integer;
      Begin
        If (x>0) Then
          Sign:=1
        Else
          If (x<0) Then
            Sign:=-1
          Else
            Sign:=0;
      End;

    Function IntSign(x : Integer) : integer;
      Begin
        If (x>0) Then
          IntSign:=1
        Else
          If (x<0) Then
            IntSign:=-1
          Else
            IntSign:=0;
      End;

    Function ExpInt(Base : Extended; Expoent : Integer) : Extended;
      Var
        BPower : Extended;
        t : Integer;
      Begin
        If Expoent=0 Then
          ExpInt:=1
        Else
          If Expoent>0 Then
            Begin
              BPower:=1;
              For t:=1 to Expoent Do
                BPower:=BPower*Base;
              ExpInt:=BPower;
            End
          Else
            Begin
              BPower:=1;
              For t:=1 to Abs(Expoent) Do
                BPower:=1/(BPower*Base+1E-10);
              ExpInt:=BPower;
            End;

      End;


    Function ExpReal(Base, Expo : Extended) : Extended;
      Begin
        If (Frac(Expo) = 0) Then
          Begin
            ExpReal:=ExpInt(Base,Round(Expo));
          End
        Else
          If Base<>0 Then
            If Base>0 Then
              Begin
                If Expo=0 Then
                  ExpReal:=1
                Else
                  If Expo<>0 Then
                    ExpReal:=Exp(Expo*Ln(Base));
              End
{            Else
              Begin
                If Expo=0 Then
                  ExpReal:=1
                Else
                  If Expo<>0 Then
                    ExpReal:=-Exp(Expo*Ln(Abs(Base)));
              End      }
          Else
            ExpReal:=0;
      End;

    Function MSqrt(x : Extended) : Extended;
      Begin
        If x>0 Then
          MSqrt:=Sqrt(x)
        Else
          MSqrt:=0;
      End;

    Function MLn(x : Extended) : Extended;
      Begin
        If x>0 Then
          MLn:=Ln(x)
        Else
          MLn:=0;
      End;

{--------------------------- Hiperbolic ----------------------------------}
    Function SinH(x : Extended) : Extended;
      Begin
        SinH:=(Exp(x)-Exp(-x))/2;
      End;
    Function CosH(x : Extended) : Extended;
      Begin
        CosH:=(Exp(x)+Exp(-x))/2;
      End;
    Function TanH(x : Extended) : Extended;
      Begin
        TanH:=SinH(x)/CosH(x);
      End;
    Function CoTanH(x : Extended) : Extended;
      Begin
        CoTanH:=1/TanH(x);
      End;
{------------------------------  Vetores -----------------------------------}

    Procedure CalcAngle(P1,P2,P3 : Vet;Var Sen,Coss : Extended);
      Begin
      End;


    Procedure Vec(r,s,t : Extended; Var A : TDA);
      Begin
        A[0]:=r;
        A[1]:=s;
        A[2]:=t;
      End;

    Procedure VecInt(r,s,t : Integer; Var A : TDIA);
      Begin
        A[0]:=r;
        A[1]:=s;
        A[2]:=t;
      End;

    Procedure UnVec(A : TDA; Var r,s,t : Extended);
      Begin
        r:=A[0];
        s:=A[1];
        t:=A[2];
      End;

    Procedure UnVecInt(A : TDIA; Var r,s,t : Integer);
      Begin
        r:=A[0];
        s:=A[1];
        t:=A[2];
      End;

    Function VecDot(A,B : TDA) : Extended;
      Begin
        VecDot:=A[0]*B[0] + A[1]*B[1] + A[2]*B[2];
      End;

    Procedure VecCross(A,B : TDA; Var C : TDA);
      Begin
        C[0]:=A[1]*B[2] - A[2]*B[1];
        C[1]:=A[2]*B[0] - A[0]*B[2];
        C[2]:=A[0]*B[1] - A[1]*B[0];
      End;

    Function VecLen(A : TDA) : Extended;
      Begin
        VecLen:=Sqrt( Sqr(A[0]) + Sqr(A[1]) + Sqr(A[2]) );
      End;

    Procedure VecNormalize(Var A : TDA);
      Var
        err : String;
        dist,invdist :  Extended;
      Begin
        dist:=VecLen(A);
        If NOT (dist=0.0) Then
          Begin
            invdist:=1/dist;
            A[0]:=A[0]*invdist;
            A[1]:=A[1]*invdist;
            A[2]:=A[2]*invdist;
          End
        Else
          err:='Zero length vectors can''t be normalized';
      End;

    Procedure VecMatxMult(A : FDA; Matrix : Matriz4x4; Var B : FDA);
      Var
        mRow,mCol : Integer;
      Begin
        For mCol:=0 to 3 Do
          Begin
            B[mCol]:=0;
            For mRow:=0 to 3 Do
              B[mCol]:=B[mCol] + a[mRow] * Matrix[mRow,mCol];
          End;
      End;

    Procedure VecSub(A,B : TDA; Var C : TDA);
      Begin
        C[0]:=A[0]-B[0];
        C[1]:=A[1]-B[1];
        C[2]:=A[2]-B[2];
      End;

    Procedure VecSubInt(A,B : TDIA; Var C : TDIA);
      Begin
        C[0]:=A[0]-B[0];
        C[1]:=A[1]-B[1];
        C[2]:=A[2]-B[2];
      End;

    Procedure VecAdd(A,B : TDA; Var C : TDA);
      Begin
        C[0]:=A[0]+B[0];
        C[1]:=A[1]+B[1];
        C[2]:=A[2]+B[2];
      End;

    Procedure VecAddInt(A,B : TDIA; Var C : TDIA);
      Begin
        C[0]:=A[0]+B[0];
        C[1]:=A[1]+B[1];
        C[2]:=A[2]+B[2];
      End;

    Procedure VecAdd3(A,B,C : TDA; Var D : TDA);
      Begin
        D[0]:=A[0]+B[0]+C[0];
        D[1]:=A[1]+B[1]+C[1];
        D[2]:=A[2]+B[2]+C[2];
      End;

    Procedure VecLinComb(r : Extended; A : TDA; s : Extended; B : TDA; Var C : TDA);
      Begin
        C[0]:=r*A[0]+s*B[0];
        C[1]:=r*A[1]+s*B[1];
        C[2]:=r*A[2]+s*B[2];
      End;

    Procedure VecScalMult(r : Extended; A : TDA; Var B : TDA);
      Begin
        B[0]:=A[0]*r;
        B[1]:=A[1]*r;
        B[2]:=A[2]*r;
      End;

    Procedure VecScalMultI(r : Extended; A : TDIA; Var B : TDA);
      Begin
        B[0]:=A[0]*r;
        B[1]:=A[1]*r;
        B[2]:=A[2]*r;
      End;

    Procedure VecScalMultInt(r : Extended; A : TDA; Var B : TDIA);
      Begin
        B[0]:=Round(A[0]*r);
        B[1]:=Round(A[1]*r);
        B[2]:=Round(A[2]*r);
      End;

    Procedure VecAddScalMult(r : Extended; A,B : TDA; Var C : TDA);
      Begin
        C[0]:=r * A[0] + B[0];
        C[1]:=r * A[1] + B[1];
        C[2]:=r * A[2] + B[2];
      End;

    Procedure VecNull(Var A : TDA);
      Begin
        A[0]:=0;
        A[1]:=0;
        A[2]:=0;
      End;

    Procedure VecNullInt(Var A : TDIA);
      Begin
        A[0]:=0;
        A[1]:=0;
        A[2]:=0;
      End;

    Procedure VecElemMult(r : Extended; A,B : TDA; Var C : TDA);
      Begin
        C[0]:=r * A[0] * B[0];
        C[1]:=r * A[1] * B[1];
        C[2]:=r * A[2] * B[2];
      End;



    Procedure Vec2Matrix(A : TDA; Var B : FDA);
      Begin
        B[0]:=A[0];
        B[1]:=A[1];
        B[2]:=A[2];
        B[3]:=1.0;
      End;

    Procedure Matrix2Vec(A : FDA; Var B : TDA);
      Begin
        B[0]:=A[0];
        B[1]:=A[1];
        B[2]:=A[2];
      End;

    Procedure VecCopy(A : TDA; Var B : TDA);
      Begin
        B:=A;
      End;

{------------- Matrizes e transformaáîes lineares ----------------------}

    Procedure ZeroMatrix(Var A : Matriz4x4);
      Var
        i,j : Byte;
      Begin
        For i:=0 to 3 Do
          For j:=0 to 3 Do
            A[i,j]:=0;
      End;

    Procedure TranslateMatriz(tx,ty,tz : Extended; Var A : Matriz4x4);
      Var
        i : Byte;
      Begin
        ZeroMatrix(A);
        For i :=0 to 3 Do
          A[i,i]:=1.0;
        A[3,0]:=-tx;
        A[3,1]:=-ty;
        A[3,2]:=-tz;
      End;

    Procedure ScaleMatriz(sx,sy,sz : Extended; Var A : Matriz4x4);
      Begin
        ZeroMatrix(A);
        A[0,0]:=sx;
        A[1,1]:=sy;
        A[2,2]:=sz;
        A[3,3]:=1.0;
      End;

    Procedure RotateMatriz(m : Byte; Theta : Extended; Var A : Matriz4x4);
      Var
        m1,m2 : Byte;
        c,s : Extended;
        t : Byte;
      Begin
        ZeroMatrix(A);
        c:=CosD(Theta);
        s:=SinD(Theta);
        For t:=0 to 3 Do
          A[t,t]:=1.0;

        Case m of
          1 : Begin
                A[1,1]:=c;
                A[2,2]:=c;
                A[2,1]:=s;
                A[1,2]:=-s;
              End;
          2 : Begin
                A[0,0]:=c;
                A[1,1]:=c;
                A[1,0]:=s;
                A[0,1]:=-s;
              End;
          3 : Begin
                A[0,0]:=c;
                A[2,2]:=c;
                A[2,0]:=s;
                A[0,2]:=-s;
              End;
        End;

      End;

    Procedure Translate3D(tx,ty,tz : Extended; A : TDA; Var  B : TDA);
      Var
        TM : Matriz4x4;
        tA,tB : FDA;
      Begin
        TranslateMatriz(tx,ty,tz,TM);
        Vec2Matrix(A,tA);
        VecMatxMult(tA,TM,tB);
        Matrix2Vec(tB,B);
      End;

    Procedure Scale3D(sx,sy,sz : Extended; A : TDA; Var  B : TDA);
      Var
        TM : Matriz4x4;
        tA,tB : FDA;
      Begin
        ScaleMatriz(sx,sy,sz,TM);
        Vec2Matrix(A,tA);
        VecMatxMult(tA,TM,tB);
        Matrix2Vec(tB,B);
      End;

    Procedure Rotate3D(E : Byte; Ang : Extended; A : TDA; Var  B : TDA);
      Var
        TM : Matriz4x4;
        tA,tB : FDA;
      Begin
        RotateMatriz(E,Ang,TM);
        Vec2Matrix(A,tA);
        VecMatxMult(tA,TM,tB);
        Matrix2Vec(tB,B);
      End;

    Procedure Multiply3DMatrizes(A,B : Matriz4x4; Var C : Matriz4x4);
      Var
        i,j,k : Byte;
        ab : Extended;
      Begin
        For i:=0 to 3 Do
          For j:=0 to 3 Do
            Begin
              ab:=0;
              For k:=0 to 3 Do
                ab:=ab + A[i,k] * B[k,j];
              C[i,j]:=ab;
            End;
      End;

    Procedure PrepareMartiz(tx,ty,tz,sx,sy,sz,rx,ry,rz : Extended; Var XForm : Matriz4x4);
      Var
        M1,M2,M3,M4,M5,M6,M7,M8,M9 : Matriz4x4;
      Begin
        ScaleMatriz(sx,sy,sz,M1);
        RotateMatriz(1,rx,M2);
        RotateMatriz(2,ry,M3);
        RotateMatriz(3,rz,M4);
        TranslateMatriz(tx,ty,tz,M5);
        Multiply3DMatrizes(M2,M1,M6);
        Multiply3DMatrizes(M3,M6,M7);
        Multiply3DMatrizes(M4,M7,M8);
        Multiply3DMatrizes(M5,M8,M9);
        Xform:=M9;
      End;

    Procedure PrepareInvMartiz(tx,ty,tz,sx,sy,sz,rx,ry,rz : Extended; Var XForm : Matriz4x4);
      Var
        M1,M2,M3,M4,M5,M6,M7,M8,M9 : Matriz4x4;
      Begin
        ScaleMatriz(sx,sy,sz,M1);
        RotateMatriz(1,rx,M2);
        RotateMatriz(2,ry,M3);
        RotateMatriz(3,rz,M4);
        TranslateMatriz(tx,ty,tz,M5);
        Multiply3DMatrizes(M4,M5,M6);
        Multiply3DMatrizes(M3,M6,M7);
        Multiply3DMatrizes(M2,M7,M8);
        Multiply3DMatrizes(M1,M8,M9);
        Xform:=M9;
      End;

    Procedure Transform(A : TDA; M : Matriz4x4; Var B : TDA);
      Begin
        B[0]:=M[0,0] * A[0] + M[0,1] * A[1] + M[0,2] * A[2] + M[0,3];
        B[1]:=M[1,0] * A[0] + M[1,1] * A[1] + M[1,2] * A[2] + M[1,3];
        B[2]:=M[2,0] * A[0] + M[2,1] * A[1] + M[2,2] * A[2] + M[2,3];
      End;

{------------------ N£meros complexos ----------------------------}

    Procedure CSoma(X : Complex; Y : Complex; Var C : Complex);
      Begin
        C.Re:=X.Re + Y.Re;
        C.Im:=X.Im + Y.Im;
      End;

    Procedure CMult(X : Complex; Y : Complex; Var C : Complex);
      Begin
        C.Re:=(X.Re * Y.Re) - (X.Im * Y.Im);
        C.Im:=(X.Re * Y.Im) + (Y.Re * X.Im);
      End;

    Procedure CMultE(x : Complex; k : Extended;Var s : Complex);
      Begin
        s.Re:=x.Re*k;
        s.Im:=x.Im*k;
      End;


    Procedure CDiv(x,y : Complex;Var s : Complex);
      Var
        z,num,den : Complex;
        k : Extended;
      Begin
        z.Re:=y.Re;
        z.Im:=-y.Im;

        CMult(x,z,num);
        CMult(y,z,den);
        k:=1/den.Re;

        CMultE(num,k,s);
      End;

    Procedure Complexo(R : Extended; I : Extended; Var C : Complex);
      Begin
        C.Re:=R;
        C.Im:=I;
      End;

    Procedure Show(X: Complex);
      Begin
        If X.Re<>0 Then
          Write(X.Re :3:3);

        If X.Im<>0 Then
          Begin
            If X.Im>0 Then
              Write(' + ',Abs(X.Im) :3:3)
            Else
              Write(' - ',Abs(X.Im) :3:3);
              Write('i');
          End;

        If (x.Im=0) AND (x.Re=0) Then
          Write('0');

        WriteLN;
      End;



{-------------------------------------------------------------------}
{---------------  A L E L U I A -  I'm Hapy  -----------------------}
{-------------------------------------------------------------------}
{---------------------     ‹‹‹‹‹‹‹‹     ----------------------------}
{--------------------    ‹ﬂ ‹    ‹ ﬂ‹    ---------------------------}
{--------------------    €          €    ---------------------------}
{--------------------    €  ¿ƒƒƒƒŸ  €    ---------------------------}
{---------------------    ﬂ‹‹‹‹‹‹‹‹ﬂ    ----------------------------}
{-------------------------------------------------------------------}
{-------------------------------------------------------------------}

  Function Solv(S : String; x,y : Extended) : Extended;

{------ Procedure para retirar os espaáos da string original ------}

    Procedure RetireSpace(Var S : String);
      Var
        t,tt : Byte;
        st : string;
      Begin
        tt:=1;
        st:='';
        For t:=1 to Length(S) Do
          If S[t] <> ' ' Then
            Begin
              st[tt]:=S[t];
              tt:=tt+1;
            End;
        st[0]:=Chr(tt);
        s:=st;
        s:=Copy(s,1,Length(s)-1);
      End;

{---- Procedure para localizar o paràntese oposto ao £ltimo aberto ------}

    Function Oposto(St : String; Loc : Byte) : Byte;
      Var
        i : Byte;
        PO,PC : String;
        Cont : Boolean;
      Begin
        PO:='(';
        PC:='';
        Cont:=TRUE;

        For i:=Loc To Length(St) Do
          Begin
            If St[i]=')' Then PC:=PC+')';
            If St[i]='(' Then PO:=PO+'(';
            If (Length(PO)=Length(PC)) AND Cont Then
              Begin
                Cont:=FALSE;
                Oposto:=i;
              End;
          End;
      End;

{---- Procedure para localizar um caractere em uma string ------}

    Function Onde(C : Char; Stg : String) : Byte;
      Var
        i : Byte;
        Cont,Cont1 : Boolean;
        p2 : Byte;
        cc : Char;
      Begin
        cc:=UpCase(C);
        p2:=0;
        Onde:=0;
        Cont1:=TRUE;
        Cont:=TRUE;
        For i:=1 To Length(Stg) Do
          Begin
            If ((Stg[i]=C)OR(Stg[i]=cc)) AND (i>p2) AND Cont1 Then
              Begin
                Onde:=i;
{                Cont1:=FALSE;   } { Observacao }
              End
            Else
              If (Stg[i]='(') AND (i>p2) Then
                Begin
                  p2:=Oposto(S,i+1);
                  Cont:=FALSE;
                End
          End;
      End;


{---- Procedure para localizar o primeiro parentese aberto ------}

    Function OndeP(Stg : String) : Byte;
      Var
        i : Byte;
        Cont : Boolean;
        p1,p2 : Byte;
      Begin
        OndeP:=0;
        Cont:=TRUE;
        For i:=1 To Length(Stg) Do
          Begin
            If (Stg[i]='(') AND Cont Then
              Begin
                OndeP:=i;
                Cont:=FALSE;
              End;
          End;
      End;


{----------------- DeclaraáÑo de vari†veis --------------------}

    Var
      tmp : Extended;
      Pnt,P1,P2 : Byte;
      err : Integer;

{----------------- Corpo da funáÑo --------------------}

    Begin
      RetireSpace(S);
      If Length(S)=0 Then Solv:=0;

      {- Base neperiana "e" -}
      Pnt:=Onde('e',S);
      If (Pnt=1) AND (Length(s)=1) Then
        Solv:= 2.718281828
       Else
         Begin

      {- Constante „ -}
      Pnt:=Onde('p',S);
      If (Pnt=1) AND (Length(s)=2) AND ((S[Pnt+1]='i')OR(S[Pnt+1]='I')) Then
        Solv:= Pi
       Else
         Begin

      {- logaritmo de base 10 -}
      Pnt:=Onde('l',S);
      If (Pnt=1) AND ((S[Pnt+1]='o')OR(S[Pnt+1]='O')) AND ((S[Pnt+2]='g')OR(S[Pnt+2]='G')) AND (Oposto(S,5)=Length(s)) Then
        Solv:= Log( Solv(Copy(S,5,Length(S)-5),x,y) )
       Else
         Begin

      {- logaritmo de base neperiana -}
      Pnt:=Onde('l',S);
      If (Pnt=1) AND ((S[Pnt+1]='n')OR(S[Pnt+1]='N')) AND (Oposto(S,4)=Length(s)) Then
        Solv:= MLn( Solv(Copy(S,4,Length(S)-4),x,y) )
       Else
         Begin


      {- Seno Hiperbolico -}
      Pnt:=Onde('s',S);
      If (Pnt=1) AND ((S[Pnt+1]='e')OR(S[Pnt+1]='E')) AND ((S[Pnt+2]='n')OR(S[Pnt+2]='N')) AND ((S[Pnt+3]='h')OR(S[Pnt+3]='H'))
        AND (Oposto(S,6)=Length(s)) Then
        Solv:= SinH( Solv(Copy(S,6,Length(S)-6),x,y) )
       Else
         Begin

      {- Coseno Hiperbolio -}
      Pnt:=Onde('c',S);
      If (Pnt=1) AND ((S[Pnt+1]='o')OR(S[Pnt+1]='O')) AND ((S[Pnt+2]='s')OR(S[Pnt+2]='S')) AND ((S[Pnt+3]='h')OR(S[Pnt+3]='H'))
        AND (Oposto(S,6)=Length(s)) Then
        Solv:=CosH( Solv(Copy(S,6,Length(S)-6),x,y) )
       Else
         Begin

      {- Cotangente trigonometrica -}
      Pnt:=Onde('c',S);
      If (Pnt=1) AND ((S[Pnt+1]='o')OR(S[Pnt+1]='O')) AND ((S[Pnt+2]='t')OR(S[Pnt+2]='T')) AND ((S[Pnt+3]='g')OR(S[Pnt+3]='G'))
        AND (Oposto(S,6)=Length(s)) Then
        Solv:=CoTan( Solv(Copy(S,6,Length(S)-6),x,y) )
       Else
         Begin


      {- Tangente trigonometrica -}
      Pnt:=Onde('t',S);
      If (Pnt=1) AND ((S[Pnt+1]='g')OR(S[Pnt+1]='G')) AND (Oposto(S,4)=Length(s)) Then
        Solv:=Tan( Solv(Copy(S,4,Length(S)-4),x,y) )
       Else
         Begin

      {- Raiz quadrada -}
      Pnt:=Onde('s',S);
      If (Pnt=1) AND ((S[Pnt+1]='q')OR(S[Pnt+1]='Q')) AND ((S[Pnt+2]='r')OR(S[Pnt+2]='R')) AND (Oposto(S,5)=Length(s)) Then
        Solv:= MSqrt( Solv(Copy(S,5,Length(S)-5),x,y) )
       Else
         Begin

      {- Valor absoluto (modulo) -}
      Pnt:=Onde('a',S);
      If (Pnt=1) AND ((S[Pnt+1]='b')OR(S[Pnt+1]='B')) AND ((S[Pnt+2]='s')OR(S[Pnt+2]='S')) AND (Oposto(S,5)=Length(s)) Then
        Solv:= Abs(Solv(Copy(S,5,Length(S)-5),x,y))
       Else
         Begin

      {- Cossecante trigonometrica -}
      Pnt:=Onde('c',S);
      If (Pnt=1) AND ((S[Pnt+1]='s')OR(S[Pnt+1]='S')) AND ((S[Pnt+2]='c')OR(S[Pnt+2]='C')) AND (Oposto(S,5)=Length(s)) Then
        Solv:= Cossec(Solv(Copy(S,5,Length(S)-5),x,y))
       Else
         Begin

      {- Cosseno trigonometrico -}
      Pnt:=Onde('c',S);
      If (Pnt=1) AND ((S[Pnt+1]='o')OR(S[Pnt+1]='O')) AND ((S[Pnt+2]='s')OR(S[Pnt+2]='S')) AND (Oposto(S,5)=Length(s)) Then
        Solv:= Cos(Solv(Copy(S,5,Length(S)-5),x,y))
       Else
         Begin

      {- Seno trigonometrico -}
      Pnt:=Onde('s',S);
      If (Pnt=1) AND ((S[Pnt+1]='e')OR(S[Pnt+1]='E')) AND ((S[Pnt+2]='n')OR(S[Pnt+2]='N')) AND (Oposto(S,5)=Length(s)) Then
         Solv:= Sin(Solv(Copy(S,5,Length(S)-5),x,y))
       Else
         Begin

      {- Secante trigonometrico -}
      Pnt:=Onde('s',S);
      If (Pnt=1) AND ((S[Pnt+1]='e')OR(S[Pnt+1]='E')) AND ((S[Pnt+2]='c')OR(S[Pnt+2]='C')) AND (Oposto(S,5)=Length(s)) Then
         Solv:= Sec(Solv(Copy(S,5,Length(S)-5),x,y))
       Else
         Begin

      {- Procura de parentese -}
      Pnt:=OndeP(S);
      If (Pnt=1) AND (Oposto(S,2)=Length(s)) Then
         Solv:= Solv(Copy(S,2,Length(S)-2),x,y)

       Else
         Begin

      {- efetua soma -}
      Pnt:=Onde('+',S);
      If Pnt<>0 Then
        Solv:= Solv(Copy(S,1,Pnt-1),x,y) + Solv(Copy(S,Pnt+1,Length(S)-Pnt),x,y)

       Else
          Begin

      {- efetua diferenca -}
      Pnt:=Onde('-',S);
      If Pnt<>0 Then
           Solv:= Solv(Copy(S,1,Pnt-1),x,y) - Solv(Copy(S,Pnt+1,Length(S)-Pnt),x,y)

       Else
          Begin

      {- efetua multiplicacao -}
      Pnt:=Onde('*',S);
      If Pnt<>0 Then
          Solv:= Solv(Copy(S,1,Pnt-1),x,y) * Solv(Copy(S,Pnt+1,Length(S)-Pnt),x,y)

       Else
           Begin

      {- efetua divisao -}
      Pnt:=Onde('/',S);
      If Pnt<>0 Then
          Solv:= Solv(Copy(S,1,Pnt-1),x,y) / (Solv(Copy(S,Pnt+1,Length(S)-Pnt),x,y)+1E-7)

       Else
           Begin

      {- efetua exponenciacao -}
      Pnt:=Onde('^',S);
      If Pnt<>0 Then
          Solv:= ExpReal( Solv(Copy(S,1,Pnt-1),x,y) , Solv(Copy(S,Pnt+1,Length(S)-Pnt),x,y) )

       Else

      If (S='X') OR (S='x') Then
        Begin
          Solv:=x;
        End

       Else

      If (S='Y') OR (S='y') Then
        Begin
          Solv:=y;
        End

       Else

         Begin
           Val(S,tmp,err);
           Solv:=tmp;
         End;


        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;
        End;


    End;

End.
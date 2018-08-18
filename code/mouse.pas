{$A+,B-,D-,E-,F+,G-,I-,L-,N-,O-,R-,S-,V-,X-}
{$M 1024,0,655360}
Unit Mouse;

interface

type MouReset = record
       exists   : boolean;
       nbuttons : integer;
     end;

     GraphMatrix=Array[1..4,1..20] of Byte;

     MouseLoc = record
       btstatus : integer;
       opcount      : integer;
       x            : integer;
       y            : integer;
     end;

     MoveRec = record
       hcount : integer;
       vcount : integer;
     end;

procedure mreset(var mouse:MouReset);
procedure mshow;
procedure mhide;
procedure mpos(var mouse:MouseLoc);
procedure mmoveto(col, row:integer);
procedure mpressed(button:integer;var mouse:MouseLoc);
procedure mreleased(button:integer;var mouse:MouseLoc);
procedure mcolrange(min,max:integer);
procedure mrowrange(min,max:integer);
procedure mgraphcursor(hhot,vhot:integer;GraphM:GraphMatrix);
procedure mtextcursor(ctype,p1,p2:word);
procedure mmotion(var moved:moverec);
procedure minsttask(mask,taskseg,taskofs:word);
procedure mlpenon;
procedure mlpenoff;
procedure mratio(horiz,vert:integer);

implementation

uses crt,dos;

const MDD       = $33;

var reg       : registers;

function lower(n1,n2:integer):integer;
begin
  if (n1<n2) then
    lower:=n1
  else
    lower:=n2;
end;

function upper(n1,n2:integer):integer;
begin
  if (n1>n2) then
    upper:=n1
  else
    upper:=n2;
end;

procedure mreset;
begin
  reg.ax:=0;
  intr(mdd,reg);
  if (reg.ax<>0) then
    mouse.exists:=true
  else
    mouse.exists:=false;
  mouse.nbuttons:=reg.bx;
end;

procedure mshow;
begin
  reg.ax:=1;
  intr(mdd,reg);
end;

procedure mhide;
begin
  reg.ax:=2;
  intr(mdd,reg);
end;

procedure mpos;
begin
   reg.ax:=3;
   intr(mdd,reg);
   mouse.btstatus:=reg.bx;
   mouse.x:=integer(reg.cx);
   mouse.y:=integer(reg.dx);
end;

procedure mmoveto;
begin
  reg.ax:=4;
  reg.cx:=col;
  reg.dx:=row;
  intr(mdd,reg);
end;

procedure mpressed;
begin
  reg.ax:=5;
  reg.bx:=button;
  intr(mdd,reg);
  mouse.btstatus:=reg.ax;
  mouse.opcount:=reg.bx;
  mouse.x:=reg.cx;
  mouse.y:=reg.dx;
end;

procedure mreleased;
begin
  reg.ax:=6;
  reg.bx:=button;
  intr(mdd,reg);
  mouse.btstatus:=reg.ax;
  mouse.opcount:=reg.bx;
  mouse.x:=reg.cx;
  mouse.y:=reg.dx;
end;

procedure mcolrange;
begin
  reg.ax:=7;
  reg.cx:=lower(min,max);
  reg.dx:=upper(min,max);
  intr(mdd,reg);
end;

procedure mrowrange;
begin
  reg.ax:=8;
  reg.cx:=lower(min,max);
  reg.dx:=upper(min,max);
  intr(mdd,reg);
end;

procedure mgraphcursor;
begin
  reg.ax:=9;
  reg.bx:=hhot;
  reg.cx:=vhot;
  reg.dx:=Ofs(GraphM);
  reg.es:=Seg(GraphM);
  intr(mdd,reg);
end;

procedure mtextcursor;
begin
  reg.ax:=10;
  reg.bx:=ctype;
  reg.cx:=p1;
  reg.dx:=p2;
  intr(mdd,reg);
end;

procedure mmotion;
begin
   reg.ax:=11;
   intr(mdd,reg);
   moved.hcount:=integer(reg.cx);
   moved.vcount:=integer(reg.dx);
end;

procedure minsttask;
begin
  reg.ax:=12;
  reg.cx:=mask;
  reg.dx:=taskofs;
  reg.es:=taskseg;
  intr(mdd,reg);
end;

procedure mlpenon;
begin
  reg.ax:=14;
  intr(mdd,reg);
end;

procedure mlpenoff;
begin
  reg.ax:=15;
  intr(mdd,reg);
end;

procedure mratio;
begin
  reg.ax:=15;
  reg.cx:=horiz;
  reg.dx:=vert;
end;

end.



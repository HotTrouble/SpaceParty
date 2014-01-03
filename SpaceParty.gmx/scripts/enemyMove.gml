var moveA, moveB, moveC, move, tx, ty;

moveA[0]=2;
moveA[1]=0;

moveB[0]=-1;
moveB[1]=1;

moveC[0]=-1;
moveC[1]=-1;

goodMove=false;
while(!goodMove)
{
  move = choose(moveA, moveB, moveC);
  tx=hexX+move[0];
  ty=hexY+move[1];
  if(
       (ty==0 && tx> 1 && tx<7)
    || (ty==1 && tx> 0 && tx<8)
    || (ty==2 && tx>-1 && tx<9)
    || (ty==3 && tx> 0 && tx<8)
    || (ty==4 && tx> 1 && tx<7)
  )
  {
    goodMove=true;
  }
}

destX=tx;
destY=ty;
speed=1;

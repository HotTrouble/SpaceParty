#define checkMatches
var moves, i, move, tx, ty, tile;

moves[0,0]=-1
moves[0,1]=-1

moves[1,0]=1
moves[1,1]=-1

moves[2,0]=-2
moves[2,1]=0

moves[3,0]=2
moves[3,1]=0

moves[4,0]=-1
moves[4,1]=1

moves[5,0]=1
moves[5,1]=1

for(i=0; i<6; i++)
{
  move=moves[i];
  tx=hexX+move[0];
  ty=hexY+move[1];
  if(tx>-1 && tx<10 && ty>-1 && ty<6)
  {
    tile=global.tiles[tx,ty];
    if(tile.sprite_index==sprite_index)
    {
        //match in this direction
    }
  }
}


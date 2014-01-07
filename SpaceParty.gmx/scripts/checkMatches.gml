var moves, i, tx, ty, tileObj, found;
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

found=false;
for(i=0; i<6; i++)
{
  if(
       (checkMatch(moves[i,0], moves[i,1],  1))
    && (checkMatch(moves[i,0], moves[i,1], -1))
  )
  {
      clearTile(moves[i,0], moves[i,1], 1);
      clearTile(moves[i,0], moves[i,1], -1);
      
      if(checkMatch(moves[i,0], moves[i,1], 2))
      {
        clearTile(moves[i,0], moves[i,1], 2);       
      }
      
      if(checkMatch(moves[i,0], moves[i,1], -2))
      {
        clearTile(moves[i,0], moves[i,1], -2);       
      }
      
      upgradeTile();
      found=true;
  }
  else if(
       (checkMatch(moves[i,0], moves[i,1], 1))
    && (checkMatch(moves[i,0], moves[i,1], 2))
  )
  {
      clearTile(moves[i,0], moves[i,1], 1);
      clearTile(moves[i,0], moves[i,1], 2);
      
      if(checkMatch(moves[i,0], moves[i,1], -1))
      {
        clearTile(moves[i,0], moves[i,1], -1);       
      }      
      
      upgradeTile();      
      found=true;
  }
}

return found;

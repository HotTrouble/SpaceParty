///checkMatch
var mx, my, mult, tx, ty, tileObj;
mx=argument0;
my=argument1;
mult=argument2;

tx=hexX+(mx*mult);
ty=hexY+(my*mult);
if(tx>-1 && tx<9 && ty>-1 && ty<5)
{
  tileObj=global.tiles[tx,ty];
  if(tileObj!=0)
  {
    return tileObj.sprite_index==sprite_index;
  }
  else
  {
    return false;
  }
}

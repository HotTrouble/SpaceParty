var pos, obj_temp, foundEnemy, i, enemy;

foundEnemy=false;
for(var i=0; i<global.numEnemies; i++)
{
  enemy=global.enemies[i];
  if(
       (enemy.hexX  ==hexX && enemy.hexY  ==hexY)
    || (enemy.hexX+2==hexX && enemy.hexY  ==hexY)
    || (enemy.hexX-1==hexX && enemy.hexY+1==hexY)
    || (enemy.hexX-1==hexX && enemy.hexY-1==hexY)
  )
  {
    foundEnemy=true;
  }
}

if(
     (sprite_index==mars_spr)
  && (global.numEnemies<4)
  && (!foundEnemy)
)
{
    pos=getTilePosition(hexX,hexY);
    obj_temp = instance_create(pos[0], pos[1], enemy_ship);

    obj_temp.x=pos[0];
    obj_temp.y=pos[1];        
    obj_temp.hexX=hexX;
    obj_temp.hexY=hexY;
            
    global.enemies[global.numEnemies]=obj_temp;
    global.numEnemies++;
}    

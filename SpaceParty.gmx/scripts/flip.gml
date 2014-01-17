/// flip()
var pos, obj_temp, possibleEnemy;

if(global.numSpawners<2)
{
  possibleEnemy=mars_spr;
}
else
{
  possibleEnemy=hexagon_spr;
}

if(
     (player_ship.hexX==hexX)
  && (player_ship.hexY==hexY)
  && (flipped=false)
)
{
    flipped=true;
    global.tilesLeft--;
    if((hexX==0) && (hexY==2))
    {
        sprite_index=hexagon_spr;
    }
    else if(global.tilesLeft==0)
    {
        sprite_index=earth_spr;
    }
    else
    {
        if(hexX>4)
        {
            sprite_index=choose(hexagon_spr, earth_spr, possibleEnemy);
        }
        else
        {
            if(global.tilesLeft==1)
            {
                sprite_index=earth_spr;
            }
            else
            {
                sprite_index=choose(hexagon_spr, hexagon_spr, possibleEnemy);
            }
        }
    }
            
    if(sprite_index==earth_spr)
    {
        ChangeRoom(captain_win);
    }
    else if(sprite_index==mars_spr)
    {
      global.numSpawners++;
    }
}

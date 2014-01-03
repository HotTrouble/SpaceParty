///colorHex
var foundEnemy, enemy, i;

/*
var shipMoves=[
    [2,0],
    [-2,0],           
    [-1,1],
    [1,1],           
    [-1,-1],
    [1,-1],           
    [0,0],
];

var enemyMoves=[
    [2,0],
    [-1,1],
    [-1,-1],
    [0,0],
];
*/

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

if(foundEnemy)
{
  image_index=2;
}
else
{
    if(
        (player_ship.hexX+2==hexX && player_ship.hexY==hexY  )
      ||(player_ship.hexX-2==hexX && player_ship.hexY==hexY  )
      ||(player_ship.hexX-1==hexX && player_ship.hexY==hexY+1)
      ||(player_ship.hexX+1==hexX && player_ship.hexY==hexY+1)
      ||(player_ship.hexX-1==hexX && player_ship.hexY==hexY-1)
      ||(player_ship.hexX+1==hexX && player_ship.hexY==hexY-1)
      ||(player_ship.hexX  ==hexX && player_ship.hexY==hexY  )
    )
    {
        image_index=1;
    }
    else
    {
        image_index=0;
    }
}

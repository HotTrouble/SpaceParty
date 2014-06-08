//Move_Ship(hexX, hexY);
var hexX, hexYpos;
hexX=argument0;
hexY=argument1;

pos=getTilePosition(hexX, hexY);
player_ship.x=pos[0]+16;
player_ship.y=pos[1]+16;
player_ship.hexX=hexX;
player_ship.hexY=hexY;
with (hexagon) spawnEnemy();
with (hexagon) flip();
with (hexagon) colorHex();

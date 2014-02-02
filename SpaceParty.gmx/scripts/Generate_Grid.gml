// Generate_Grid(width, height, egrid); 
var width, height, egrid, tileType, hexX, hexY, pos, obj_temp; 
width = argument0*2; 
height = argument1; 
egrid = argument2;
tileType = argument3;

for (hexY=0; hexY<height; hexY++)
{
    for (hexX=0; hexX<width; hexX++)
    {    
        if(egrid[hexX,hexY]==1)
        {
            pos=getTilePosition(hexX,hexY);
            obj_temp = instance_create(pos[0], pos[1], tileType);
        
            obj_temp.image_xscale=global.hexScale;
            obj_temp.image_yscale=global.hexScale;
            obj_temp.hexX=hexX;
            obj_temp.hexY=hexY;
            obj_temp.image_speed=0;
            obj_temp.flipped=false;
            global.tilesLeft++;
            global.tiles[hexX, hexY]=obj_temp;
        }
        else
        {
          global.tiles[hexX, hexY]=0;
        }
    }
}


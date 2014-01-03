/// getTilePosition(x,y)
var hexX, hexY, result;
hexX=argument0;
hexY=argument1;

result[0]=hexX*global.hex_width *0.9 + global.xoffset;
result[1]=hexY*global.hex_height*1.5 + global.yoffset;

return result;        

// Expand_Grid(width, height, grid); 
var width, height, grid, egrid; 
width = argument0; 
height = argument1; 
grid = argument2; 

for(y=0; y<height; y++)
{
    for(x=0; x<width; x++)
    {
        if(y%2==0)
        {
            egrid[x*2,y]=grid[x,y];
            egrid[x*2+1,y]=0;
        }
        else
        {
            egrid[x*2+1,y]=grid[x,y];
            egrid[x*2,y]=0;
        }
    }
}    

return egrid;

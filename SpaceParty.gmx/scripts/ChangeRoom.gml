///ChangeRoom(room)
var dest, dh, dw, width, height, stretchWidth, aspect, offset;
dest=argument0;

/*
window_set_color(c_black);

if(os_type==os_windows || os_type==os_macosx)
{
  dw=1136
  dh=640
  offset=0;
}
else if(os_type==os_ios)
{
  dw=1136;
  dh=640;
  offset=0;
}
else if(os_type==os_android)
{
  show_debug_message("Resizing for mobile");
  dw=display_get_width();
  dh=display_get_height();
    
  if(dw>dh) // landscape
  {
    show_debug_message("Landscape mode");
    width=640;
    height=480;  
  }
  else // portrait
  {
    show_debug_message("Portrait mode");
    dw=display_get_height();
    dh=display_get_width();
    width=480;
    height=640;  
  }
    
  aspect=width/height;
  stretchWidth=floor(dh*aspect);
  offset=floor((dw-stretchWidth)/2);
}
else
{
  dw=1136;
  dh=640;
  offset=0;
}

show_debug_message("dw: " + string(dw));  
show_debug_message("dh: " + string(dh));  

room_set_view(dest,0,1,0,0,dw,dh,offset,0,dw,dh,-1,-1,-1,-1,-1);
*/
room_goto(dest);

var buff, command, numShips;

show_debug_message("Receive network");

buff = ds_map_find_value(async_load, "buffer");
command = buffer_read(buff, buffer_u8);

if command == WHERE
{
  r = buffer_read(buff, buffer_u16 );
  a = buffer_read(buff, buffer_u16 );
  show_debug_message("Sector ("+string(r)+", "+string(a)+")");
  
  Who();
}
else if command == WHO
{
  numShips = buffer_read(buff, buffer_u8 );
  show_debug_message("Found "+string(numShips)+" ships");
  repeat(numShips)
  {
    name = buffer_read(buff, buffer_string);
    hasCaptain = buffer_read(buff, buffer_bool);
    hasEngineer = buffer_read(buff, buffer_bool);
    repeat(6)
    {
      buffer_read(buff, buffer_bool); // read unused fields
    }
    show_debug_message("ship "+name+" "+string(hasCaptain)+" "+string(hasEngineer));
  }
}

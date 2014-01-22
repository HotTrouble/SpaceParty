///Join(ship, position)
var ship, position, buff, client;

ship=argument0;
position=argument1;

buff=global.sendBuffer;
client=global.client;

show_debug_message("join!");

buffer_seek(buff, buffer_seek_start, 0);
buffer_write(buff, buffer_u8, JOIN);
buffer_write(buff, buffer_u8, ship);
buffer_write(buff, buffer_u8, position);

network_send_raw(client, buff, buffer_tell(buff));

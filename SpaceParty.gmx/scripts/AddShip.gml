///AddShip()
var buff, client;

buff=global.sendBuffer;
client=global.client;

show_debug_message("add ship!");

buffer_seek(buff, buffer_seek_start, 0);
buffer_write(buff, buffer_u8, ADD_SHIP);

network_send_raw(client, buff, buffer_tell(buff));

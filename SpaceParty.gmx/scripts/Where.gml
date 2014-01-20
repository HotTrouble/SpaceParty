///Where()
var buff, client;

buff=global.sendBuffer;
client=global.client;

show_debug_message("where?");

buffer_seek(buff, buffer_seek_start, 0);
buffer_write(buff, buffer_u8, WHERE);
network_send_raw(client, buff, buffer_tell(buff));

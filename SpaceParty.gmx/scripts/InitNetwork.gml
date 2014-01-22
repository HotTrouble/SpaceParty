show_debug_message("Connecting to "+serverHost+":"+string(serverPort));
global.client = network_create_socket(network_socket_tcp);
var server = network_connect_raw(global.client , serverHost, serverPort);
if server < 0
{
  show_debug_message("Connection failed");
  show_debug_message(string(server));
  global.connected=false;
}
else
{
  show_debug_message("Connected");
  global.connected=true;
  global.sendBuffer=buffer_create(1500, buffer_fixed, 1);
  global.recvBuffer=buffer_create(1500, buffer_fixed, 1);
}

///upgradeTile
if(sprite_index==red_spr)
{
  sprite_index=orange_spr;
  ach_orange.sprite_index=orange_spr;
  audio_play_sound(match_snd, 10, false);
}
else if(sprite_index==orange_spr)
{
  sprite_index=yellow_spr;
  ach_yellow.sprite_index=yellow_spr;
  audio_play_sound(match_snd, 10, false);}
else if(sprite_index==yellow_spr)
{
  sprite_index=green_spr;
  ach_green.sprite_index=green_spr;  
  audio_play_sound(match_snd, 10, false);}
else if(sprite_index==green_spr)
{
  sprite_index=blue_spr;
  ach_blue.sprite_index=blue_spr;  
  audio_play_sound(match_snd, 10, false);}
else if(sprite_index==blue_spr)
{
  sprite_index=purple_spr;
  ach_purple.sprite_index=purple_spr;
  audio_play_sound(purplematch_snd, 10, false);}


var display_w, display_h, height, aspect_ratio;
display_w = display_get_width();
display_h = display_get_height();

height=480;

// override if we are testing on Windows:
if os_type == os_android
{
  view_hview = height;
  view_hport = height;

  aspect_ratio = display_w/display_h;

  view_wview = view_hview*aspect_ratio;
  view_wport = view_wview;
}

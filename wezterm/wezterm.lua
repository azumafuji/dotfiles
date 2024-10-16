-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- Import our new module (put this near the top of your wezterm.lua)
local appearance = require 'appearance'

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Use it!
if appearance.is_dark() then
  config.color_scheme = 'Tomorrow Night (Gogh)'
else
  config.color_scheme = 'Tomorrow (light) (terminal.sexy)'
end


config.window_decorations = "RESIZE"
config.adjust_window_size_when_changing_font_size = false

config.window_frame = {
  border_left_width = '0.5cell',
  border_right_width = '0.5cell',
  border_bottom_height = '0.1cell',
  border_top_height = '0.1cell',
  border_left_color = '#5E81AC',
  border_right_color = '#5E81AC',
  border_bottom_color = '#5E81AC',
  border_top_color = '#5E81AC',
}

config.enable_wayland = true
config.freetype_load_target = 'VerticalLcd'
config.freetype_load_flags = 'NO_AUTOHINT'
config.font_size = 12
config.line_height = 1.1
config.font = wezterm.font 'Iosevka Fixed Curly'

wezterm.on('update-status', function(window)
  -- Grab the utf8 character for the "powerline" left facing
  -- solid arrow.
  local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

  -- Grab the current window's configuration, and from it the
  -- palette (this is the combination of your chosen colour scheme
  -- including any overrides).
  local color_scheme = window:effective_config().resolved_palette
  local bg = color_scheme.background
  local fg = color_scheme.foreground

  window:set_right_status(wezterm.format({
    -- First, we draw the arrow...
    { Background = { Color = 'none' } },
    { Foreground = { Color = bg } },
    { Text = SOLID_LEFT_ARROW },
    -- Then we draw our text
    { Background = { Color = bg } },
    { Foreground = { Color = fg } },
    { Text = ' ' .. wezterm.hostname() .. ' ' },
  }))
end)

return config

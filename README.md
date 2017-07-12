## R connector to Philips Hue

This R package implements a thin wrapper around the Philips Hue API. It let's you control your Hue lights from R.

## Usage

You can change the color of any lamp with `set_light_color()`,
set it to a specific color temperature and brightness with `set_light_temp()`,
blink a lamp with `alert_light()`,
query the state and type of a light with `get_light_data()`,
and dump all state data with `dump_lights_data()`.

First register a new user token with your Philips Hue bridge by calling `create_user()`

Find more inforamtion on usage by using R's help system, e.g. `?set_light_color`

## Installation

```
devtools::install_github("acartauchenius/rhue")
```



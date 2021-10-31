#!/bin/bash

mute=$(pamixer --get-mute)
volume=$(pamixer --get-volume)

if [ $mute == "true" ]; then
  icon="<fn=1>\xf6a9</fn>"
elif [ $volume -eq 0 ]; then
  icon="<fn=1>\xf026</fn>"
elif [ $volume -lt 30 ]; then
  icon="<fn=1>\xf027</fn>"
else
  icon="<fn=1>\xf028</fn>"
fi

echo "$icon $volume%"

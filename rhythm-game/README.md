# Rhythm Game Documentation

This documentation includes how to run this program, and how to deal with some common issues or bugs. 

## How to run the code
```
>>> stack setup
>>> stack build
>>> stack exec rhythm-game-exe
or
>>> stack run
``` 

## Dependencies
- brick
- stack
- ghcup

## Deal with Issues
### Linux
1. Install `xos` to play music file. (https://www.unixmen.com/how-to-play-music-from-command-line-terminal/)
   
   ```
   play music.mp3 > /dev/null 2>&1
   ```
2. Error: play fail formats: no handler for file extension `mp3'
   
   ```
   sudo apt-get install libsox-fmt-mp3
   ```

### MacOS
No issues found right now
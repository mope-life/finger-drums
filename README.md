# what am i looking at?
This is the beginning of a web-based modular synthesizer built on the Web Audio API. It is still a work-in-progress, very much in its infancy.

For more information on modular synthesis and what it is, [this YouTube video](https://www.youtube.com/watch?v=cWslSTTkiFU) explains it nicely.

Since the JavaScript uses imports, you'll have to host the site on a local web server if you would like to use it. This can be done easily with `python3 -m http.server` from the root directory, for example.

# why should i care?
It's fun to play with! With the addition of a recording feature, this can also have practical uses in music production. It can also be used as a sandbox to test out modular ideas before purchasing any expensive equipment.

# where are we now?
The basic functionality of several modules (keys, const, env) is in place. I also believe that the core structure of the project is complete. Here is an image of a working set-up with a nifty sub-octave square wave:

![example](/examples/snapshot.png)


# where are we going?
### Other modules:
- filters (draggable shelf / peak / pass)
- polyphonic keyboard / oscillators
- built-in drum kit
- white/pink noise generators
- visualizations (oscilloscope, spectrogram, et al.)
- ...and whatever else we dream up
### Other planned features:
- Save your work (account creation)
- Share your work (permalinks)
- Save a whole patch as a new module
- Midi compatibility

### Similar projects:
- [Zupiter](http://z.musictools.live/) -- *I discovered Zupiter after I started this project, but I have taken some inspiration from it*
- [ModularGrid](http://www.modulargrid.net) -- *modular synthesis database and planner with real-world modules*

TimePaper
=========

Create a wallpaper displaying your average time usage! This program
takes logs from [TagTime](http://messymatters.com/tagtime/), and uses
them to automatically generate some helpful graphs for figuring out
how you spend your time! Currently it can be installed with

    cabal install --bindir=/path/to/where/you/want/bin

You can then run the program as follows

    TimePaper logFile -o test.svg -w 200

This should produce an image much like this one!

![Time usage pie chart!](./images/time_usage.png)

Issues
------

Charts does not display lots of text around a pie chart very
well. Sometimes it puts the same colour next to each other, which is
very annoying...

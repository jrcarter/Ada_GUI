# Ada_GUI
An Ada-oriented GUI

Traditionally, GUIs have been implemented by the client registering callback operations, then giving up its thread of control to the GUI. This is a hack to address the inherent parallelism of a GUI in sequential languages. It results in code that is unnecessarily difficult to understand.

In a concurrent language like Ada, a better design for a GUI is for the GUI to have its own task, so it doesn't require that its client give up a thread of control. GUI events are communicated to the client through a protected queue of events. The client software can be written in the clearest way possible, unconstrained by the GUI.

Ada_GUI is such a GUI.

The implementation of Ada_GUI is derived from Gnoga, but simplified, reorganized, and converted to put events on a protected queue rather than call callbacks. The name Gnoga has been retained for the name of the implementation hierarchy, located at Ada_GUI.Gnoga, in recognition. Those parts of Kazakov's Simple Components used by the implementation are provided as well, with some minor modifications. If you have your own version of the Simple Components, you may use it instead: Delete all Ada source files that do not begin with ada_gui.

In the process of modifying Gnoga I seem to have prevented responses to resizing the window. Provided you don't need that, Ada GUI is reasonably full-featured and useful. The implementation is more responsive than the former proof-of-concept directly on full Gnoga.

The Test directory contains boot.html, boot.js, and jquery.min.js. These files must be available to every Ada-GUI program. It also contains favicon.ico, an icon of Ada.

Show_All creates at least one of each widget. Visibility, playing audio, and responding to key-press events is demonstrated. Changing this program is the easiest way to experiment with Ada GUI. The audio file glass.ogg is used by Show_All.

Luhn_Gen calculates Luhn checksum digits. Enter the first 15 digits of your credit-card number and it will generate the 16th.

Random_Int is a Gnoga program that has been revised to use Ada GUI instead. It will generate random integers in a desired range.

## Installation

Before compiling Ada_GUI, you have to specify the OS on which it will be used. Open ada_gui-gnoga-application.adb in an editor and search for "OS_ID". Change the constant OS in the next line to match your OS (anything other than Mac OSX or Windows should use Unix). You can then compile the library.

Gnoga and the Simple Components require a recent GNAT compiler, so Ada_GUI cannot be compiled with another compiler. The file gnat.adc is needed to compile the Simple Components extensions to GNAT.Sockets. The use of gnatmake is recommended, as gprbuild has a serious problem that causes lots of unnecessary recompilation. After compiling, it is recommended that you write-protect the .ali files. Typically, you can use a command such as

gnatmake -m -j0 -gnatan -gnato2 -O2 -fstack-check ada_gui*.adb

If you keep the Ada-GUI source files in a single directory, including the Simple Components source files, and have the compiler put the object and .ali files in that directory, then you need only add -I/path/to/Ada_GUI/ to your program's gnatmake command to use Ada_GUI.

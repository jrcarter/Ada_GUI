# Ada_GUI
An Ada-oriented GUI

Traditionally, GUIs have been implemented by the client registering callback operations, then giving up its thread of control to the GUI. This is a hack to address the inherent parallelism of a GUI in sequential languages. It results in code that is unnecessarily difficult to understand.

In a concurrent language like Ada, a better design for a GUI is for the GUI to have its own task, so it doesn't require that its client give up a thread of control. GUI events are communicated to the client through a protected queue of events. The client software can be written in the clearest way possible, unconstrained by the GUI.

Ada_GUI is such a GUI.

The purpose of Ada_GUI is to provide the specification, in hopes that multiple implementations will be created. Note that some of the comments in the specification are specific to the sample implementation, and should be removed or modified for other implementations. Especially interesting would be an implementation that is not GNAT specific.

Beginning with commit 59fdef9, the ID parameter to Set_Up has been removed.

## Sample Implementation

A sample implementation of Ada_GUI is supplied, derived from Gnoga, but simplified, reorganized, and converted to put events on a protected queue rather than call callbacks. The name Gnoga has been retained for the name of the implementation hierarchy, located at Ada_GUI.Gnoga, in recognition. Those parts of Kazakov's Simple Components used by the implementation are provided as well, with some minor modifications. If you have your own version of the Simple Components, you may use it instead: Delete all Ada source files that do not begin with ada_gui.

The sample implementation is reasonably full-featured and useful.

With the removal of the ID parameter to Set_Up, the sample implementation attempts to find an unused port in 8080 .. 8179. Programs that accepted the default ID need only be recompiled. Programs that specify the ID will need to be modified.

The port-use information is stored in a file named Ada_GUI_Ports. On Windows, it is located in the %userprofile% directory. On other systems, it is located in the /home/<username> directory. These are typically the user's home directory. The directory must exist and the user must have write permission for it.

If two Ada-GUI programs start or stop at the same time, **a race condition is possible**. If a port in the range is being used by something other than an Ada-GUI program, an Ada-GUI program **may attempt to use the same port and fail**.

The automatic port-selection process has been tested on Linux and Windows. **Testing on other platforms, such as BSDs and Mac OS, is still needed.**

### Test Programs

The Test directory contains boot.html, boot.js, and jquery.min.js. **These files must be in the working directory of every Ada-GUI program.** It also contains favicon.ico, an icon of Ada.

Show_All creates at least one of each widget. Visibility, playing audio, and responding to key-press events is demonstrated. Changing this program is the easiest way to experiment with the sample implementation. The audio file glass.ogg and image file rgb.jpg are used by Show_All.

Luhn_Gen calculates Luhn checksum digits. Enter the first 15 digits of your credit-card number and it will generate the 16th.

Random_Int is a Gnoga program that has been revised to use Ada GUI instead. It will generate random integers in a desired range.

## Installation of the Sample Implementation

Gnoga and the Simple Components require a recent GNAT compiler, so Ada_GUI cannot be compiled with another compiler. The file gnat.adc is needed to compile the Simple Components extensions to GNAT.Sockets. The use of gnatmake is recommended, as gprbuild is for mixed-language or complicated builds, which this is not. After compiling, it is recommended that you write-protect the .ali files. Typically, you can use a command such as

gnatmake -m -j0 -gnat12 -gnatan -gnato2 -O2 -fstack-check Test/show_all.adb

If you keep the Ada-GUI source files in a single directory, including the Simple Components source files, and have the compiler put the object and .ali files in that directory, then you need only add -I/path/to/Ada_GUI/ to your program's gnatmake command to use Ada_GUI.

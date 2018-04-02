# Ada_GUI
An Ada-oriented GUI proof of concept

Traditionally, GUIs have been implemented by the client registering callback operations, then giving up its thread of control to the GUI. This is a hack to address the inherent parallelism of a GUI in sequential languages. It results in code that is unnecessarily very difficult to understand.

In a concurrent language like Ada, a better design for a GUI is for the GUI to have its own task, so it doesn't require that its client give up a thread of control. GUI events are communicated to the client through a protected queue of events. The client software can be written in the clearest way possible, unconstrained by the GUI.

Ada_GUI is a proof of concept of the interface for such a GUI. It implements only a few kinds of widgets. A quick and dirty implementation on top of Gnoga is provided. A version of Random_Int using this GUI interface demonstrates how it might be used, though it only uses buttons and text boxes.

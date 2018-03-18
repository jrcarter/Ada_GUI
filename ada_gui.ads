-- Proof of concept of an Ada-oriented GUI interface, with only those things needed to implement Random_Int
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package Ada_GUI is
   type Widget_Kind_ID is (Text_Box, Button);
   type Widget_ID is private;

   function Program_Finished return Boolean;
   -- Returns True after End_GUI has been called; False otherwise

   function New_Item (Kind : Widget_Kind_ID; Text : in String; Break_Before : Boolean := False; Label : in String := "")
   return Widget_ID with
      Pre => not Program_Finished;
   -- Creates a new widget of kind Kind with text Text.
   -- Text is the initial content of a Text_Box widget and the button label of a Button widget
   -- If Break_Before, the widget appears below any existing widgets; otherwise, it appears to the right of the most recent widget
   -- If Kind = Text_Box and Label /= "", Label will appear to the left of the widget
   -- Returns the ID of the new widget

   type Event_Kind_ID is (Left_Click);
   type Event_Info (Widget_Kind : Widget_Kind_ID := Text_Box) is record
      ID : Widget_ID;

      case Widget_Kind is
      when Text_Box =>
         null;
      when Button =>
         Event_Kind : Event_Kind_ID;
      end case;
   end record;

   type Next_Result_Info (Timed_Out : Boolean := False) is record
      case Timed_Out is
      when False =>
         Event : Event_Info;
      when True =>
         null;
      end case;
   end record;

   procedure Set_Title (Title : in String) with Pre => not Program_Finished;
   -- Sets the window title to Title

   function Next_Event (Timeout : Duration := Duration'Last) return Next_Result_Info with Pre => not Program_Finished;
   -- Blocks until an event is available, or Timeout seconds have passed, whichever is first
   -- Result is (Timed_Out => True) if Timeout seconds pass before there's an event available
   -- Otherwise, the result R has not R.Timed_Out, and R.Event contains the Event_Info for the event

   procedure Set_Text (ID : in Widget_ID; Text : in String) with Pre => not Program_Finished;
   -- Sets the text for ID to Text

   function Text (ID : Widget_ID) return String with Pre => not Program_Finished;
   -- Returns the text for ID

   procedure End_GUI with Pre => not Program_Finished;
   -- Destroys the GUI
private -- Ada_Gui
   type Widget_ID is mod Integer'Size;
end Ada_Gui;


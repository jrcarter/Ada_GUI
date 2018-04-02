-- Proof of concept of an Ada-oriented GUI interface, with only those things needed to implement Random_Int
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package Ada_GUI is
   type Widget_Kind_ID is (Button, Check_Box, Text_Area, Text_Box);
   type Widget_ID is private;

   function Program_Finished return Boolean;
   -- Returns True after End_GUI has been called; False otherwise

   -- For the New_Widget functions, if Break_Before, the button appears below any existing widgets;
   -- otherwise, it appears to the right of the most recent widget
   -- All New_Widget functions return the ID of the widget

   function New_Button (Text : in String; Break_Before : Boolean := False) return Widget_ID with Pre => not Program_Finished;
   -- Creates a Button with button label Text.

   function New_Check_Box (Label : String; Break_Before : Boolean := False; Active : Boolean := False) return Widget_ID
   with Pre => not Program_Finished;
   -- Creates a new Check_Box with label Label
   -- If Active, the box will be checked; otherwise, it will be unchecked

   function New_Text_Area (Text : String := ""; Break_Before : Boolean := False; Width : Positive := 20; Height : Positive := 2)
   return Widget_ID with Pre => not Program_Finished;
   -- Creates a new Text_Box with initial content of Text.
   -- Width is width of area in characters
   -- Height is height of area in lines

   function New_Text_Box
      (Text : String; Break_Before : Boolean := False; Label : String := ""; Placeholder : String := ""; Width : Positive := 20)
   return Widget_ID with Pre => not Program_Finished;
   -- Creates a new Text_Box with initial content of Text.
   -- Label will appear to the left of the text box
   -- If Placeholder /= "", Placeholder will appear in the text box when it is empty and awaiting input
   -- Width is width of box in characters

   type Event_Kind_ID is (Left_Click, Middle_Click, Right_Click);
   type Event_Info (Widget_Kind : Widget_Kind_ID := Text_Box) is record
      ID : Widget_ID;

      case Widget_Kind is
      when Button =>
         Event_Kind : Event_Kind_ID;
      when others =>
         null;
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
   -- If ID is a Button, Text_Area, or Text_Box, sets the text for ID to Text
   -- Otherwise raises Program_Error
   -- For a Text_Area, embedded LFs cause line breaks

   function Text (ID : Widget_ID) return String with Pre => not Program_Finished;
   -- If ID is a Button, Text_Area, or Text_Box, returns the text for ID
   -- Otherwise raises Program_Error
   -- For a Text_Area, line breaks are encoded as LFs

   procedure Set_Active (ID : in Widget_ID; Active : in Boolean) with Pre => not Program_Finished;
   -- IF ID is not a Check_Box, raises Program_Error
   -- Otherwise, if Active, makes ID checked, else makes ID unchecked

   function Active (ID : Widget_ID) return Boolean with Pre => not Program_Finished;
   -- IF ID is not a Check_Box, raises Program_Error
   -- Otherwise, returns True if ID is checked; False otherwise

   procedure End_GUI with Pre => not Program_Finished;
   -- Destroys the GUI
private -- Ada_Gui
   type Widget_ID is mod Integer'Size;
end Ada_Gui;


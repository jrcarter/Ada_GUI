-- Proof of concept of an Ada-oriented GUI interface, with only those things needed to implement Random_Int
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Strings.Unbounded;

package Ada_GUI is
   type Widget_Kind_ID is
      (Audio_Player, Background_Text, Button, Check_Box, Graphic_Area, Radio_Buttons, Selection_List, Text_Area, Text_Box);

   type Widget_ID is tagged private;

   function Program_Finished return Boolean;
   -- Returns True after End_GUI has been called; False otherwise

   function Kind (ID : Widget_ID) return Widget_Kind_ID with Pre => not Program_Finished;
   -- If ID was returned by a call to a New_[Widget] function, returns the kind of widget identified by ID
   -- Raises Constraine_Error otherwise

   -- For the New_[Widget] functions below, if Break_Before, the button appears below any existing widgets;
   -- otherwise, it appears to the right of the most recent widget
   -- All New_[Widget] functions return the ID of the new widget

   function New_Audio_Player (Break_Before : Boolean := False; Source : String := ""; Controls : Boolean := True) return Widget_ID
   with Pre => not Program_Finished;
   -- Creates an Audio_Player
   -- Source is the source of audio; if "", then no audio is loaded
   -- Audio sources seem to be relative path names of audio files and URLs
   -- If Controls, then controls are displayed for the player and the user can use them to control the player
   -- Otherwise, no controls are displayed and control of the player must be done by the program

   function New_Background_Text (Text : in String; Break_Before : Boolean := False) return Widget_ID
   with Pre => not Program_Finished;
   -- Creates a new Background_Text with contents Text
   -- Like a label, background text is not in any visible widget; unlike a label, background text is not associated with a widget

   function New_Button (Text : in String; Break_Before : Boolean := False) return Widget_ID with Pre => not Program_Finished;
   -- Creates a Button with button label Text.
   -- Left clicks on buttons generate events

   function New_Check_Box (Label : String; Break_Before : Boolean := False; Active : Boolean := False) return Widget_ID
   with Pre => not Program_Finished;
   -- Creates a new Check_Box with label Label
   -- If Active, the box will be checked; otherwise, it will be unchecked

   function New_Graphic_Area (Width : in Positive; Height : in Positive; Break_Before : Boolean := False) return Widget_ID
   with Pre => not Program_Finished;
   -- Creates a new Graphic_Area of Width by Height pixels
   -- (0, 0) is the upper-left corner (Width, Height) is the lower-right corner

   type Text_List is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   type Orientation_ID is (Horizontal, Vertical);

   function New_Radio_Buttons (Label : in Text_List; Break_Before : Boolean := False; Orientation : in Orientation_ID := Vertical)
   return Widget_ID with Pre => not Program_Finished;
   -- Creates Label'Length radio buttons; Label contains the labels for the buttons
   -- Orientation = Horizontal results in a row of buttons
   --             = Vertical has each button after the 1st below the preceding buttons
   -- The button for Label'First will be active
   -- The operations Set_Active and Active for radio buttons take an Index; Index will refer to the button for Label (Index)

   function New_Selection_List
      (Text : in Text_List; Break_Before : Boolean := False; Height : in Positive := 1; Multiple_Select : in Boolean := False)
   return Widget_ID with Pre => not Program_Finished;
   -- Text contains the initial set of options; it may be empty
   -- Height is in lines; 1 results in a drop-down list; otherwise it scrolls if it has more than Height options
   -- If Multiple_Select, the user can select more than one option at a time; otherwise, only one option may be selected at a time
   -- The set of options may be modified later using Insert and Delete
   -- Left clicks on selection lists generate events

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
      when Button | Selection_List =>
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

   procedure Show_Message_Box (Text : in String) with Pre => not Program_Finished;
   -- Shows a dialog box with Text and an OK button

   function Next_Event (Timeout : Duration := Duration'Last) return Next_Result_Info with Pre => not Program_Finished;
   -- Blocks until an event is available, or Timeout seconds have passed, whichever is first
   -- Result is (Timed_Out => True) if Timeout seconds pass before there's an event available
   -- Otherwise, the result R has not R.Timed_Out, and R.Event contains the Event_Info for the event

   procedure Set_Source (ID : in Widget_ID; Source : in String) with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Makes source the audio source for ID

   function Source (ID : Widget_ID) return String with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Returns the current audio source for ID

   function Ready (ID : Widget_ID) return Boolean with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Returns True if ID is ready to play its source; False otherwise
   -- There is a perceptable (to a computer) delay between a call to Set_Source and Ready returning True

   procedure Play (ID : in Widget_ID) with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Plays the current source from the current position for ID

   procedure Pause (ID : in Widget_ID) with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Pauses playback for ID

   function Paused (ID : Widget_ID) return Boolean with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Returns True if playback for ID is paused; False otherwise

   function Playback_Ended (ID : Widget_ID) return Boolean with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Returns True if playback for ID has ended; False otherwise

   function Length (ID : Widget_ID) return Float with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Returns the length of the current source for ID in seconds

   procedure Set_Position (ID : in Widget_ID; Position : in Float) with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Sets the current position for ID to Position in seconds
   -- 0.0 is the beginning and Duration (ID) is the end

   function Position (ID : Widget_ID) return Float with Pre => not Program_Finished and Kind (ID) = Audio_Player;
   -- Returns the current position of ID in seconds

   procedure Set_Text (ID : in Widget_ID; Text : in String)
   with Pre => not Program_Finished and Kind (ID) in Background_Text .. Button | Text_Area .. Text_Box;
   -- Sets the text for ID to Text
   -- For a Text_Area, embedded LFs cause line breaks

   function Multiple_Select (ID : Widget_ID) return Boolean with Pre => not Program_Finished and Kind (ID) = Selection_List;
   -- Returns the value of Multiple_Select used to create ID

   function Text (ID : Widget_ID) return String
   with Pre => not Program_Finished                                                and
               Kind (ID) in Background_Text .. Button | Selection_List .. Text_Box and
               (if Kind (ID) = Selection_List then not Multiple_Select (ID) else True);
   -- Returns the text for ID; for a Selection_List with no selection, returns ""
   -- For a Text_Area, line breaks are encoded as LFs

   procedure Set_Active (ID : in Widget_ID; Active : in Boolean) with Pre => not Program_Finished and Kind (ID) = Check_Box;
   -- If Active, makes ID checked, else makes ID unchecked

   function Active (ID : Widget_ID) return Boolean with Pre => not Program_Finished and Kind (ID) = Check_Box;
   -- Returns True if ID is checked; False otherwise

   type RGB_Value is mod 256;

   type Color_Info is record
      Red   : RGB_Value := 0;
      Green : RGB_Value := 0;
      Blue  : RGB_Value := 0;
   end record;

   type Color_ID is (Alice_Blue,        Antique_White,      Aqua,                    Aquamarine,          Azure,
                     Beige, Bisque,     Black,              Blanched_Almond,         Blue,                Blue_Violet,
                     Brown,             Burly_Wood,
                     Cadet_Blue,        Chartreuse,         Chocolate,               Coral,               Cornflower_Blue,
                     Cornsilk,          Crimson,            Cyan,
                     Dark_Blue,         Dark_Cyan,          Dark_Golden_Rod,         Dark_Gray,           Dark_Green,
                     Dark_Grey,         Dark_Khaki,         Dark_Magenta,            Dark_Olive_Green,    Dark_Orange,
                     Dark_Orchid,       Dark_Red,           Dark_Salmon,             Dark_Sea_Green,      Dark_Slate_Blue,
                     Dark_Slate_Gray,   Dark_Slate_Grey,    Dark_Turquoise,          Dark_Violet,         Deep_Pink,
                     Deep_Sky_Blue,     Dim_Gray,           Dim_Grey,                Dodger_Blue,
                     Fire_Brick,        Floral_White,       Forest_Green,            Fuchsia,
                     Gainsboro,         Ghost_White,        Gold_Deep_Sky_Blue,      Golden_Rod,          Gray,
                     Green,             Green_Yellow,       Grey,
                     Honey_Dew,         Hot_Pink,
                     Indian_Red,        Indigo,             Ivory,
                     Khaki,
                     Lavender,          Lavender_Blush,     Lawn_Green,              Lemon_Chiffon,       Light_Blue,
                     Light_Coral,       Light_Cyan,         Light_Golden_Rod_Yellow, Light_Gray,          Light_Green,
                     Light_Grey,        Light_Pink,         Light_Salmon,            Light_Sea_Green,     Light_Sky_Blue,
                     Light_Slate_Gray,  Light_Slate_Grey,   Light_Steel_Blue,        Light_Yellow,        Lime,
                     Lime_Green,        Linen,
                     Magenta,           Maroon,             Medium_Aqua_Marine,      Medium_Blue,         Medium_Orchid,
                     Medium_Purple,     Medium_Sea_Green,   Medium_Slate_Blue,       Medium_Spring_Green, Medium_Turquoise,
                     Medium_Violet_Red, Midnight_Blue,      Mint_Cream,              Misty_Rose,          Moccasin,
                     Navajo_White,      Navy,
                     Old_Lace,          Olive,              Olive_Drab,              Orange,              Orange_Red,
                     Orchid,
                     Pale_Golden_Rod,   Pale_Green,         Pale_Turquoise,          Pale_Violet_Red,     Papaya_Whip,
                     Peach_Puff,        Peru,               Pink,                    Plum,                Powder_Blue,
                     Purple,
                     Red,               Rosy_Brown,         Royal_Blue,
                     Saddle_Brown,      Salmon,             Sandy_Brown,             Sea_Green,           Sea_Shell,
                     Sienna,            Silver,             Sky_Blue,                Slate_Blue,          Slate_Gray,
                     Slate_Grey,        Snow,               Spring_Green,            Steel_Blue,
                     Tan,               Teal,               Thistle,                 Tomato,              Turquoise,
                     Violet,
                     Wheat,             White,              White_Smoke,
                     Yellow,            Yellow_Green);

   function To_Color (Color : Color_ID) return Color_Info;
   function To_ID (Color : Color_Info) return Color_ID with Pre => (for some ID in Color_ID => To_Color (ID) = Color);
   -- Convert back and forth between Color_Info and Color_ID

   -- Graphic_Area operations for which any part of the drawn element is outside the drawing area work; the extra part is not drawn

   procedure Set_Pixel (ID : in Widget_ID; X : in Integer; Y : in Integer; Color : in Color_Info := To_Color (Black) )
   with Pre => not Program_Finished and Kind (ID) = Graphic_Area;
   -- If (X, Y) is in the drawing area, sets it to Color

   function Pixel (ID : Widget_ID; X : Integer; Y : Integer) return Color_Info
   with Pre => not Program_Finished and Kind (ID) = Graphic_Area;
   -- Returns the color of the pixel at (X, Y)

   procedure Draw_Line (ID     : in Widget_ID;
                        From_X : in Integer;
                        From_Y : in Integer;
                        To_X   : in Integer;
                        To_Y   : in Integer;
                        Color  : in Color_Info := To_Color (Black) )
   with Pre => not Program_Finished and Kind (ID) = Graphic_Area;
   -- Draws a line from (From_X, From_Y) to (To_X, To_Y) in Color

   type Optional_Color (None : Boolean) is record
      case None is
      when False =>
         Color : Color_Info;
      when True =>
         null;
      end case;
   end record;

   procedure Draw_Rectangle (ID         : in Widget_ID;
                             From_X     : in Integer;
                             From_Y     : in Integer;
                             To_X       : in Integer;
                             To_Y       : in Integer;
                             Line_Color : in Optional_Color := (None => False, Color => To_Color (Black) );
                             Fill_Color : in Optional_Color := (None => True) )
   with Pre => not Program_Finished and Kind (ID) = Graphic_Area;
   -- Draws a rectangle with one corner at (From_x, From_Y) and the opposite corner at (To_X, To_Y)
   -- If not Line_Color.None, the rectangle will have a line around it in Line_Color.Color
   -- If not Fill_Color.None, the rectangle will be filled with Fill_Color.Color
   -- If Line_Color.None and Fill_Color.None, does nothing

   procedure Draw_Arc (ID                : in Widget_ID;
                       X                 : in Integer;
                       Y                 : in Integer;
                       Radius            : in Positive;
                       Start             : in Float;
                       Stop              : in Float;
                       Counter_Clockwise : in Boolean := False;
                       Line_Color        : in Optional_Color := (None => False, Color => To_Color (Black) );
                       Fill_Color        : in Optional_Color := (None => True) )
   with Pre => not Program_Finished and Kind (ID) = Graphic_Area;
   -- Draws an arc with center at (X, Y) and radius Radius from angle Start to angle Stop; angles are in radians
   -- If Counter_Clockwise, draws an arc counter-clockwise from Start to Stop; otherwise, draws clockwise
   -- If not Line_Color.None, the arc will have a line along it in Line_Color.Color; if not Fill_Color.None as well, there will also
   -- be lines between the ends of the arc and the center point
   -- If not Fill_Color.None, the arc will be filled with Fill_Color.Color
   -- If Line_Color.None and Fill_Color.None, does nothing

   procedure Set_Active (ID : in Widget_ID; Index : in Positive; Active : in Boolean)
   with Pre => not Program_Finished and Kind (ID) = Radio_Buttons;
   -- Makes Active (ID, Index) return Active
   -- Raises Constraint_Error if Index not in Label'Range

   function Active (ID : Widget_ID; Index : Positive) return Boolean
   with Pre => not Program_Finished and Kind (ID) = Radio_Buttons;
   -- Returns True if the button created for Label (Index) is selected; False otherwise
   -- Raises Constraint_Error if Index not in Label'Range

   function Active (ID : Widget_ID) return Positive with Pre => not Program_Finished and Kind (ID) = Radio_Buttons;
   -- Returns the index in ID of the active button

   function Length (ID : Widget_ID) return Natural with Pre => not Program_Finished and Kind (ID) = Selection_List;
   -- Returns the number of options in ID

   procedure Set_Selected (ID : in Widget_ID; Index : in Positive; Selected : in Boolean := True)
   with Pre => not Program_Finished and Kind (ID) = Selection_List and Index in 1 .. Length (ID);
   -- Makes Selected (ID, Index) return Selected

   function Selected (ID : Widget_ID) return Natural
   with Pre => not Program_Finished and Kind (ID) = Selection_List and not Multiple_Select (ID);
   -- Returns the index of the currently selected option in ID or zero if there is no selection

   function Selected (ID : Widget_ID; Index : Positive) return Boolean
   with Pre => not Program_Finished and Kind (ID) = Selection_List and Index in 1 .. Length (ID);
   -- Returns True if option Index in ID is selected; False otherwise

   function Text (ID : Widget_ID; Index : Positive) return String
   with Pre => not Program_Finished and Kind (ID) = Selection_List and Index in 1 .. Length (ID);
   -- Returns the text of option Index; if there are no options in ID, returns ""

   procedure Insert (ID : in Widget_ID; Text : in String; Before : in Positive := Integer'Last)
   with Pre => not Program_Finished and Kind (ID) = Selection_List;
   -- if Before > Length (ID), appends Text to the options of ID
   -- Otherwise, inserts Text as the option with index Before, moving up the options previously at Before .. Length (ID)

   procedure Delete (ID : in Widget_ID; Index : in Positive)
   with Pre => not Program_Finished and Kind (ID) = Selection_List and Index in 1 .. Length (ID);
   -- Deletes the option at Index, moving down the options previously at Index + 1 .. Length (ID)

   procedure End_GUI with Pre => not Program_Finished, Post => Program_Finished;
   -- Destroys the GUI
private -- Ada_Gui
   type Widget_ID is tagged record
      Value : Natural;
   end record;
end Ada_Gui;


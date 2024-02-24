-- An Ada-oriented GUI library specification. The included sample implementation uses a browser as its platform
--
-- Copyright (C) 2024 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Strings.Unbounded;

package Ada_GUI is
   type Widget_Kind_ID is (Audio_Player,  Background_Text, Button,    Check_Box, Graphic_Area, Image, Password_Box, Progress_Bar,
                           Radio_Buttons, Selection_List,  Text_Area, Text_Box);

   type Widget_ID is tagged private;

   function Set_Up return Boolean;
   -- Returns True after Set_Up has been called and before End_GUI has been called; False otherwise

   function Kind (ID : Widget_ID) return Widget_Kind_ID with Pre => Set_Up;
   -- If ID was returned by a call to a New_[Widget] function, returns the kind of widget identified by ID
   -- Raises Constraint_Error otherwise

   type Area_Kind_ID is (Area, Extension);
   type Alignment_ID is (Left, Center, Right);

   type Grid_Info (Kind : Area_Kind_ID := Area) is record
      case Kind is
      when Area =>
         Alignment : Alignment_ID := Center;
      when Extension =>
         null;
      end case;
   end record;

   type Grid_Set is array (Positive range <>, Positive range <>) of Grid_Info;

   procedure Set_Up (Grid  : in Grid_Set := (1 => (1 => (others => <>) ) );
                     ID    : in Positive := 8080;
                     Title : in String   := "Ada-GUI Application";
                     Icon  : in String   := "favicon.ico")
   with Pre => not Set_Up          and
               Grid'Length (1) > 0 and
               Grid'Length (2) > 0 and
               Grid'First (1) = 1  and
               Grid'First (2) = 1  and
               (for all R in Grid'Range (1) => Grid (R, 1).Kind = Area),
        Post => Set_Up;
   -- Sets up a grid of Grid'Length (1) rows by Grid'Length (2) Columns of display areas
   -- Each display area has the alignment given by Grid for its row and column
   -- A display area is either a new Area, or an Extension of the area to its left
   -- Title is the initial window title
   -- In the sample implementation, each application has an Ada-GUI ID; two applications with the same ID cannot run at the same
   -- time
   -- (The ID is used as the port for talking to the browser, hence the restriction)
   -- Other implementations may ignore or remove ID
   --
   -- Grid example: if Grid = (1 => (1 => (Area, Left), 2 => (Extension),  3 => (Area, Left) )
   --                          2 => (1 => (Area, Left), 2 => (Area, Left), 3 => (Extension) ) )
   -- then the resulting display areas will look something like
   --     1  2  3
   --   +-----+--+
   -- 1 |     |  |
   --   +--+--+--+
   -- 2 |  |     |
   --   +--+-----+

   function Window_Height return Positive with Pre => Set_Up;
   function Window_Width  return Positive with Pre => Set_Up;
   -- Returns the window dimensions

   procedure End_GUI with Post => not Set_Up;
   -- Destroys the GUI
   -- For the sample implementation, this must called by any program that withs Ada_GUI, even if procedure Set_Up has not been
   -- called, to terminate the event-handling task

   -- For the New_[Widget] functions below, if Break_Before, the button appears below any existing widgets;
   -- otherwise, it appears to the right of the most recent widget
   -- All New_[Widget] functions return the ID of the new widget
   -- All New_[Widget] functions take the Row and Column of the display area in which the widget will be created
   -- The Column should refer to an area with Kind = Area, but if it does not, it will be adjusted to the column
   -- of the aread that Column extends

   function New_Audio_Player (Row          : Positive := 1;
                              Column       : Positive := 1;
                              Break_Before : Boolean  := False;
                              Source       : String   := "";
                              Controls     : Boolean  := True)
   return Widget_ID with Pre => Set_Up;
   -- Creates an Audio_Player
   -- Source is the source of audio; if "", then no audio is loaded
   -- The source may be changed with Set_Source
   -- In the sample implementation, Audio sources are path names of audio files relative to the working directory and URLs
   -- If Controls, then controls are displayed for the player and the user can use them to control the player
   -- Otherwise, no controls are displayed and control of the player must be done by the program

   function New_Background_Text
      (Row : Positive := 1; Column : Positive := 1; Text : String := ""; Break_Before : Boolean := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Background_Text with contents Text
   -- Like a label, background text is not in any visible widget; unlike a label, background text is not associated with a widget
   -- In the sample implementation, background text may contain HTML text attributes such as "<b>...</b>" and embedding "<br>"
   -- will cause new lines

   function New_Button (Row : Positive := 1; Column : Positive := 1; Text : String := ""; Break_Before : Boolean := False)
   return Widget_ID With Pre => Set_Up;
   -- Creates a Button with button label Text.
   -- Left clicks on buttons generate events
   -- In the sample implementation, button text may contain HTML text attributes such as "<b>...</b>" and embedding "<br>" will
   -- cause new lines

   function New_Check_Box (Row          : Positive := 1;
                           Column       : Positive := 1;
                           Label        : String   := "";
                           Break_Before : Boolean  := False;
                           Active       : Boolean  := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Check_Box with label Label
   -- If Active, the box will be checked; otherwise, it will be unchecked
   -- In the sample implementation, a check-box label may contain HTML text attributes such as "<b>...</b>" and embedding "<br>"
   -- will cause new lines

   function New_Graphic_Area
      (Row : Positive := 1; Column : Positive := 1; Width : Positive; Height : Positive; Break_Before : Boolean := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Graphic_Area with initial dimensions of Width by Height pixels
   -- (0, 0) is the upper-left corner; (Width, Height) is the lower-right corner

   function New_Image (Row          : Positive := 1;
                       Column       : Positive := 1;
                       Source       : String   := "";
                       Description  : String   := "";
                       Break_Before : Boolean  := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Image with contents defined by Source
   -- The source may be changed with Set_Source
   -- In the sample implementation, Source may be an image URL or a path name relative to the working directory, and Description
   -- (also called Alt Text) will be read by screen readers and displayed if Source is invalid. BMP, JPG, PNG, and WEBP files have
   -- been tested and work; PBM, PPM, and QOI are not supported

   function New_Password_Box (Row          : Positive := 1;
                              Column       : Positive := 1;
                              Text         : String   := "";
                              Break_Before : Boolean  := False;
                              Label        : String   := "";
                              Width        : Positive := 20)
   return Widget_ID with Pre => Set_Up;
   -- Same as New_Text_Box, but the resulting box does not echo the characters in it

   function New_Progress_Bar (Row          : Positive :=   1;
                              Column       : Positive :=   1;
                              Value        : Natural  :=   0;
                              Maximum      : Natural  := 100;
                              Break_Before : Boolean  := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Progress_Bar with given Value and Maximum
   -- The real value Value / Maximum represents the proportion of the progrss that has been completed

   type Text_List is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String with
      Dynamic_Predicate => Text_List'First = 1;

   type Orientation_ID is (Horizontal, Vertical);

   function New_Radio_Buttons (Row          : Positive := 1;
                               Column       : Positive := 1;
                               Label        : Text_List;
                               Break_Before : Boolean        := False;
                               Orientation  : Orientation_ID := Vertical)
   return Widget_ID with Pre => Set_Up and Label'Length > 1;
   -- Creates Label'Length radio buttons; Label contains the labels for the buttons
   -- Orientation = Horizontal results in a row of buttons
   --             = Vertical   has each button after the 1st below the preceding buttons
   -- The button for Label'First will be active
   -- The operations Set_Active and Active for radio buttons take an Index; Index will refer to the button for Label (Index)
   -- In the sample implementation, radio-button labels may contain HTML text attributes such as "<b>...</b>" and embedding "<br>"
   -- will cause new lines

   function New_Selection_List (Row             : Positive  := 1;
                                Column          : Positive  := 1;
                                Text            : Text_List := (1 .. 0 => <>);
                                Break_Before    : Boolean   := False;
                                Height          : Positive  := 1;
                                Multiple_Select : Boolean   := False)
   return Widget_ID with Pre => Set_Up;
   -- Text contains the initial set of options; it may be empty
   -- Height is in lines; 1 results in a drop-down list; otherwise it scrolls if it has more than Height options
   -- If Multiple_Select, the user can select more than one option at a time; otherwise, only one option may be selected at a time
   -- Specifying the options here may be faster than adding them individually with Insert
   -- The set of options may be modified later using Insert and Delete
   -- Left clicks on selection lists generate events
   -- For the sample implementation, specifying the options here is much faster than adding them individually with Insert

   function New_Text_Area (Row          : Positive := 1;
                           Column       : Positive := 1;
                           Text         : String   := "";
                           Break_Before : Boolean  := False;
                           Width        : Positive := 20;
                           Height       : Positive := 2)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Text_Area with initial content of Text.
   -- Width is width of area in characters
   -- Height is height of area in lines
   -- In the sample implementation, text-area text may contain HTML text attributes such as "<b>...</b>" and embedding "<br>" will
   -- cause new lines

   function New_Text_Box (Row          : Positive := 1;
                          Column       : Positive := 1;
                          Text         : String   := "";
                          Break_Before : Boolean  := False;
                          Label        : String   := "";
                          Placeholder  : String   := "";
                          Width        : Positive := 20)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Text_Box with initial content of Text.
   -- Label will appear to the left of the text box
   -- If Placeholder /= "", Placeholder will appear in the text box when it is empty and awaiting input
   -- Width is width of box in characters

   type Event_Kind_ID is (Left_Click, Right_Click, Double_Click, Key_Press, Window_Closed);

   type Mouse_Event_Info is record
      X             : Integer;
      Y             : Integer;
      Screen_X      : Integer;
      Screen_Y      : Integer;
      Left_Button   : Boolean := False;
      Middle_Button : Boolean := False; -- There does not appear to be any way for this to become True
      Right_Button  : Boolean := False;
      Alt           : Boolean := False;
      Control       : Boolean := False;
      Shift         : Boolean := False;
      Meta          : Boolean := False;
   end record;

   type Keyboard_Event_Info Is record
      Key_Code : Integer;
      Key_Char : Wide_Character;
      Alt      : Boolean := False;
      Control  : Boolean := False;
      Shift    : Boolean := False;
      Meta     : Boolean := False;
   end record;

   type Event_Info (Kind : Event_Kind_ID := Left_Click) is record
      ID   : Widget_ID; -- ID for Window (Kind => Window_Closed) is invalid for all operations
      Data : Ada.Strings.Unbounded.Unbounded_String; -- Implementation-defined event data that was parsed to give Mouse or Key

      case Kind is
      when Left_Click | Right_Click | Double_Click =>
         Mouse : Mouse_Event_Info;
      when Key_Press =>
         Key : Keyboard_Event_Info;
      when Window_Closed =>
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

   function Next_Event (Timeout : Duration := Duration'Last) return Next_Result_Info with Pre => Set_Up;
   -- Blocks until an event is available, or Timeout seconds have passed, whichever is first
   -- Result is (Timed_Out => True) if Timeout seconds pass before there's an event available
   -- Otherwise, the result R has not R.Timed_Out, and R.Event contains the Event_Info for the event
   -- A double click generates two events, a Left_Click followed by a Double_Click
   -- This function blocks if a call to Selected_File is in progress

   procedure Set_Title (Title : in String) with Pre => Set_Up;
   -- Sets the window title to Title

   procedure Show_Message_Box (Text : in String) with Pre => Set_Up;
   -- Shows a dialog box with Text and an OK button

   procedure Set_Hidden (ID : in Widget_ID; Hidden : in Boolean := True) with Pre => Set_Up;
   -- Set whether or not ID is hidden
   -- Widgets are not hidden by default

   procedure Set_Visibility (ID : in Widget_ID; Visible : in Boolean := True) with Pre => Set_Up;
   -- Sets whether or not ID is visible
   -- Widgets are visible by default

   -- Difference between visibile/invisible and hidden/not hidden:
   -- An invisible widget does not show up, but does take up space
   -- Making a widget invisible does not change the poisition of other widgets
   -- A hidden widget does not show and does not take up space
   -- Making a widget hidden may change the poisition of other widgets

   procedure Log (Message : in String);
   -- Writes Message with a time stamp to standard output

   package Dialogs is
      -- During a dialog, events are not available and Next_Event will block
      -- Only one dialog may proceed at a time; if a dialog is in progress, another dialog will return a specified value

      type File_Result_Info (Picked : Boolean := False) is record
         case Picked is
         when False =>
            null;
         when True =>
            Value : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

      function Selected_File (Initial_Directory : in String := ".") return File_Result_Info with Pre => Set_Up;
      -- Opens a file-selection dialog with the files for Initial_Directory
      -- If the user cancels the dialog or closes the window, or another dialog is in progress, returns (Picked => False)
      -- Otherwise, the return value has Picked => True and the Value component contains the full path of the selected file

      function Selected_Button (Title : in String; Text : in String; Button : in Text_List) return String with
         Pre => Set_Up and Button'Length > 1;
      -- Displays Text under Title with a row of Button'Length buttons, each with the text given by the corresponding value in
      -- Button
      -- If the user clicks on a button, returns the button text of the corresponding button
      -- If the user closes the window, or another dialog is in progress, returns ""
      -- For a dialog with one OK button, use Show_Message_Box

      Yes_No : constant Text_List := (Ada.Strings.Unbounded.To_Unbounded_String ("Yes"),
                                      Ada.Strings.Unbounded.To_Unbounded_String ("No") );

      function Yes_Or_No (Title : in String; Text : in String; Button : in Text_List := Yes_No) return String
      renames Selected_Button;
   end Dialogs;

   procedure Set_Source (ID : in Widget_ID; Source : in String) with Pre => Set_Up and ID.Kind in Audio_Player | Image;
   -- Makes Source the source for ID

   function Source (ID : Widget_ID) return String with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns the current audio source for ID

   function Ready (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns True if ID is ready to play its source; False otherwise
   -- In the sample implementation, there is a perceptible (to a computer) delay between a call to Set_Source and Ready returning
   -- True

   function Loaded (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Image;
   -- Returns True if ID has completed loading its source; False otherwise
   -- In the sample implementation, there is a perceptible (to a computer) delay between a call to Set_Source and Loaded returning
   -- True

   procedure Play (ID : in Widget_ID) with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Plays the current source from the current position for ID

   procedure Pause (ID : in Widget_ID) with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Pauses playback for ID

   function Paused (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns True if playback for ID is paused; False otherwise

   function Playback_Ended (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns True if playback for ID has ended; False otherwise

   function Length (ID : Widget_ID) return Float with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns the length of the current source for ID in seconds

   procedure Set_Position (ID : in Widget_ID; Position : in Float) with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Sets the current position for ID to Position in seconds
   -- 0.0 is the beginning and ID.Length is the end

   function Position (ID : Widget_ID) return Float with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns the current position of ID in seconds

   procedure Set_Text (ID : in Widget_ID; Text : in String) with
      Pre => Set_Up and ID.Kind in Background_Text | Button | Password_Box | Text_Area | Text_Box;
   -- Sets the text for ID to Text
   -- In the sample implementation, all kinds except Password_Box and Text_Box may contain HTML text attributes such as
   -- "<b>...</b>" and embedding "<br>" will cause new lines

   procedure Set_Text_Alignment (ID : in Widget_ID; Alignment : in Alignment_ID) with
      Pre => Set_Up and ID.Kind in Background_Text | Button | Password_Box | Selection_List | Text_Area | Text_Box;
   -- Sets the text alignment for ID to Alignment

   type Font_Kind_ID is (Proportional, Monospaced);

   procedure Set_Text_Font_Kind (ID : in Widget_ID; Kind : in Font_Kind_ID) with
      Pre => Set_Up and ID.Kind in Background_Text | Button | Password_Box | Selection_List | Text_Area | Text_Box;
   -- Sets the text font kind for ID to Kind
   -- Default is Proportional

   procedure Set_Label (ID : in Widget_ID; Text : in String) with
      Pre => Set_Up and Id.Kind in Check_Box | Password_Box | Text_Box;
   -- Sets the label text for ID to Text
   -- In the sample implementation, a label may contain HTML text attributes such as "<b>...</b>" and embedding "<br>" will cause
   -- new lines

   procedure Set_Read_Only (ID : in Widget_ID; Read_Only : in Boolean := True) with
      Pre => Set_Up and ID.Kind in Text_Area | Text_Box;
   -- Set the read-only status for ID to Read_Only

   function Multiple_Select (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Selection_List;
   -- Returns the value of Multiple_Select used to create ID

   function Text (ID : Widget_ID) return String with
      Pre => Set_Up                                                                                     and
             ID.Kind in Background_Text | Button | Password_Box | Selection_List | Text_Area | Text_Box and
             (if ID.Kind = Selection_List then not ID.Multiple_Select else True);
   -- Returns the text for ID; for a Selection_List with no selection, returns ""

   procedure Set_Active (ID : in Widget_ID; Active : in Boolean) with Pre => Set_Up and ID.Kind = Check_Box;
   -- If Active, makes ID checked, else makes ID unchecked

   function Active (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Check_Box;
   -- Returns True if ID is checked; False otherwise

   type RGB_Value is mod 256;
   subtype Alpha_Value is Float range 0.0 .. 1.0;

   type Color_Info is record
      Red   : RGB_Value   := 0;
      Green : RGB_Value   := 0;
      Blue  : RGB_Value   := 0;
      Alpha : Alpha_Value := 1.0;
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

   procedure Set_Background_Color (Color : in Color_Info) with Pre => Set_Up;
   -- Sets the background color of the window to Color

   procedure Set_Background_Color (ID : Widget_ID; Color : in Color_Info := To_Color (Black) ) with Pre => Set_Up;
   -- Sets the background color for ID to Color

   procedure Set_Foreground_Color (ID : Widget_ID; Color : in Color_Info := To_Color (Black) ) with Pre => Set_Up;
   -- Sets the foreground color for ID to Color
   -- For text widgets, this is the text color

   -- In the sample implementation, Graphic_Area operations for which any part of the drawn element is outside the drawing area
   -- work; the extra part is not drawn

   procedure Set_Size (ID: in Widget_ID; Width : in Natural; Height : in Natural) with
      Pre => Set_Up and ID.Kind in Graphic_Area | Image;
   -- Changes the dimensions of ID to Width x Height
   -- In the sample implementation, the drawn image in a Graphic_Area will be resized

   function Width (ID : in Widget_ID) return Natural with
      Pre => Set_Up and ID.Kind in Graphic_Area | Image;
   -- Returns the current width of ID

   function Height (ID : in Widget_ID) return Natural with
      Pre => Set_Up and ID.Kind in Graphic_Area | Image;
   -- Returns the current height of ID

   procedure Set_Pixel (ID : in Widget_ID; X : in Integer; Y : in Integer; Color : in Color_Info := To_Color (Black) ) with
      Pre => Set_Up and ID.Kind = Graphic_Area;
   -- If (X, Y) is in the drawing area, sets it to Color
   -- In the sample implementation, if (X, Y) is not in the drawing area, has no effect

   function Pixel (ID : Widget_ID; X : Integer; Y : Integer) return Color_Info with
      Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Returns the color of the pixel at (X, Y)
   -- In the sample implementation, if (X, Y) is not in the drawing area, the returned value is valid but undefined

   type Line_Style_ID is (Normal, Dashed, Dotted, Dot_Dash);

   procedure Draw_Line (ID     : in Widget_ID;
                        From_X : in Integer;
                        From_Y : in Integer;
                        To_X   : in Integer;
                        To_Y   : in Integer;
                        Width  : in Positive      := 1;
                        Color  : in Color_Info    := To_Color (Black);
                        Style  : in Line_Style_ID := Normal)
   with Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Draws a line from (From_X, From_Y) to (To_X, To_Y) in Color using Style
   -- Width is width of line in pixels

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
   with Pre => Set_Up and ID.Kind = Graphic_Area;
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
   with Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Draws an arc with center at (X, Y) and radius Radius (in pixels) from angle Start to angle Stop; angles are in radians
   -- If Counter_Clockwise, draws an arc counter-clockwise from Start to Stop; otherwise, draws clockwise
   -- If not Line_Color.None, the arc will have a line along it in Line_Color.Color; if not Fill_Color.None as well, there will
   -- also be lines between the ends of the arc and the center point
   -- If not Fill_Color.None, the arc will be filled with Fill_Color.Color

   procedure Draw_Text (ID         : in Widget_ID;
                        X          : in Integer;
                        Y          : in Integer;
                        Text       : in String;
                        Height     : in Positive       := 20;
                        Line_Color : in Optional_Color := (None => True);
                        Fill_Color : in Optional_Color := (None => False, Color => To_Color (Black) ) )
   with Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Draws Text with its left side at X and the bottom of letters without a descender at Y
   -- Letters will be Height pixels tall
   -- If not Line_Color.None, letters will be outlined in Line_Color.Color
   -- If not Fill_Color.None, letters will be filled with Fill_Color.Color
   -- If (Line_Color.None and Fill_Color.None) or Text = "", does nothing

   procedure Replace_Pixels (ID : in Widget_ID; Image : in Widget_ID; X : in Integer := 0; Y : in Integer := 0)
   with Pre => Set_Up and ID.Kind = Graphic_Area and Image.Kind in Graphic_Area | Ada_GUI.Image;
   -- Replaces pixels in ID starting at (X, Y) and extending to the right by the width of Image, and down by the height of Image,
   -- with the pixels in Image

   procedure Fill (ID : in Widget_ID; Image : in Widget_ID)
   with Pre => Set_Up and ID.Kind = Graphic_Area and Image.Kind = Ada_GUI.Image;
   -- Fills ID with Image, replacing any existing content
   -- ID should have been created with an intial width and height at least those of Image, and resized to the width and height of
   -- Image

   type Image_Data is array (Natural range <>, Natural range <>) of Color_Info with
      Dynamic_Predicate => Image_Data'First (1) = 0 and Image_Data'First (2) = 0;
   -- First dimension is Y/rows; second is X/columns

   procedure Replace_Pixels (ID : in Widget_ID; Image : in Image_Data; X : in Integer := 0; Y : in Integer := 0) with
      Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Draws the pixels in Image in ID, starting at (X, Y) and extending to the right by the width of Image, and down by the height
   -- of Image
   -- This may be faster than calling Set_Pixel repeatedly

   function Data (ID : in Widget_ID) return Image_Data with
      Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Extracts the pixels in ID and returns the image
   -- This may be faster than calling Pixel repeatedly

   function Maximum (ID : Widget_ID) return Natural with Pre => Set_Up and ID.Kind = Progress_Bar;
   -- Returns the current Maximum value for ID

   function Value (ID : Widget_ID) return Natural with Pre => Set_Up and ID.Kind = Progress_Bar;
   -- Returns the current Value for ID

   procedure Set_Value (ID : in Widget_ID; Value : in Natural) with
      Pre => Set_Up and ID.Kind = Progress_Bar and Value in 0 .. ID.Maximum;
   -- Sets the current value for for ID to Value

   procedure Set_Maximum (ID : in Widget_ID; Maximum : in Natural) with
      Pre => Set_Up and ID.Kind = Progress_Bar and Maximum >= ID.Value;
   -- Sets the current maximum for ID to Maximum

   function Num_Buttons (ID : Widget_ID) return Positive with Pre => Set_Up and ID.Kind = Radio_Buttons;
   -- Returns the number of buttons in the radio-button set

   procedure Set_Active (ID : in Widget_ID; Index : in Positive; Active : in Boolean) with
      Pre => Set_Up and ID.Kind = Radio_Buttons and Index in 1 .. ID.Num_Buttons;
   -- Makes Active (ID, Index) return Active

   function Active (ID : Widget_ID; Index : Positive) return Boolean with
      Pre => Set_Up and ID.Kind = Radio_Buttons and Index in 1 .. ID.Num_Buttons;
   -- Returns True if the button created for Label (Index) is selected; False otherwise

   function Active (ID : Widget_ID) return Positive with
      Pre  => Set_Up and ID.Kind = Radio_Buttons,
      Post => Active'Result in 1 .. ID.Num_Buttons;
   -- Returns the index in ID of the active button

   procedure Set_Label (ID : in Widget_ID; Index : in Positive; Text : in String) with
      Pre => Set_Up and ID.Kind = Radio_Buttons and Index in 1 .. ID.Num_Buttons;
   -- In the sample implementation, a label may contain HTML text attributes such as "<b>...</b>" and embedding "<br>" will cause
   -- new lines

   function Length (ID : Widget_ID) return Natural with Pre => Set_Up and ID.Kind = Selection_List;
   -- Returns the number of options in ID

   procedure Clear (ID : in Widget_ID) with Pre => Set_Up and ID.Kind = Selection_List;
   -- Deletes all options from ID

   procedure Set_Selected (ID : in Widget_ID; Index : in Positive; Selected : in Boolean := True) with
      Pre => Set_Up and ID.Kind = Selection_List and Index in 1 .. ID.Length;
   -- Makes Selected (ID, Index) return Selected

   function Selected (ID : Widget_ID) return Natural with
      Pre => Set_Up and ID.Kind = Selection_List and not Multiple_Select (ID);
   -- Returns the index of the currently selected option in ID or zero if there is no selection

   function Selected (ID : Widget_ID; Index : Positive) return Boolean with
      Pre => Set_Up and ID.Kind = Selection_List and Index in 1 .. ID.Length;
   -- Returns True if option Index in ID is selected; False otherwise

   function Text (ID : Widget_ID; Index : Positive) return String with
      Pre => Set_Up and ID.Kind = Selection_List and Index in 1 .. ID.Length;
   -- Returns the text of option Index

   procedure Append (ID : in Widget_ID; Text : in Text_List) with
      Pre => Set_Up and ID.Kind = Selection_List;
   -- Appends the options in Text to the end of ID
   -- This may be faster than appending them individually with Insert
   -- In the sample implementation, Append is much faster than appending the values individually with Insert

   procedure Insert (ID : in Widget_ID; Text : in String; Before : in Positive := Integer'Last) with
      Pre => Set_Up and ID.Kind = Selection_List;
   -- if Before > ID.Length, appends Text to the options of ID
   -- Otherwise, inserts Text as the option with index Before, moving up the options previously at Before .. ID.Length

   procedure Delete (ID : in Widget_ID; Index : in Positive) with
      Pre => Set_Up and ID.Kind = Selection_List and Index in 1 .. ID.Length;
   -- Deletes the option at Index, moving down the options previously at Index + 1 .. ID.Length

   package Plotting is -- Allows using Graphic_Areas as mathematical plotting areas, with real-valued Cartesian coordinates
      type Plot_Info (<>) is tagged private;

      function New_Plot (ID : in Widget_ID; X_Min : in Float; X_Max : in Float; Y_Min : in Float; Y_Max : in Float)
      return Plot_Info with
         Pre => Set_Up and ID.Kind = Graphic_Area and X_Min < X_Max and Y_Min < Y_Max;
      -- Associates ID with the result and establishes scale values for the left (X_Min), right (X_Max), top (Y_Max), and bottom
      -- (Y_Min) edges of ID
      -- Note that the Y scale is reversed from that of a Graphic_Area

      function Scale_X (Plot : Plot_Info; X : Float) return Integer;
      -- Returns the pixel-X coordinate corresponding to X

      function Scale_Y (Plot : Plot_Info; Y : Float) return Integer;
      -- Returns the pixel-Y coordinate corresponding to Y

      procedure Draw_Point (Plot : in Plot_Info; X : in Float; Y : in Float; Color : in Color_Info := To_Color (Black) ) with
         Pre => Set_Up;
      -- Draws a point (filled circle with radius of 2 pixels) at (X, Y) in color Color

      procedure Draw_Line (Plot   : in Plot_Info;
                           From_X : in Float;
                           From_Y : in Float;
                           To_X   : in Float;
                           To_Y   : in Float;
                           Color  : in Color_Info    := To_Color (Black);
                           Style  : in Line_Style_ID := Normal)
      with Pre => Set_Up;
      -- Draws a line (width 1 pixel) from (From_X, From_Y) to (To_X, To_Y) in color Color

      subtype Positive_Float is Float range Float'Succ (0.0) .. Float'Last;

      procedure Draw_X_Axis (Plot     : in Plot_Info;
                             Interval : in Positive_Float;
                             Length   : in Positive;
                             Label    : in String     := "";
                             Color    : in Color_Info := To_Color (Black) )
      with Pre => Set_Up;
      -- Draws the X axis with ticks every Interval away from zero
      -- The axis is labeled with Label
      -- Ticks extend Length pixels from the axis on both sides
      -- Ticks that are for integer values are labeled with their values
      -- The axis and labels will be in color Color
      -- No tick or label is drawn for 0.0, since that is usually crossed by the Y axis, which labels it
      -- The axis has a Y of 0.0 unless that is off the plot, in which case it is on the edge of the plot nearest to 0.0

      procedure Draw_Y_Axis (Plot     : in Plot_Info;
                             Interval : in Positive_Float;
                             Length   : in Positive;
                             Label    : in String     := "";
                             Color    : in Color_Info := To_Color (Black) )
      with Pre => Set_Up;
      -- Draws the Y axis with ticks every Interval away from zero
      -- The axis is labeled with Label
      -- Ticks extend Length pixels from the axis on both sides
      -- Ticks that are for integer values are labeled with their values
      -- The axis and ticks will be in color Color
      -- The axis has an X of 0.0 unless that is off the plot, in which case it is on the edge of the plot nearest to 0.0

      procedure Draw_Axes
         (Plot : in Plot_Info; Interval : in Positive_Float; Length : in Positive; Color : in Color_Info := To_Color (Black) )
      with Pre => Set_Up;
      -- Draws both the X and Y axes with ticks every Interval away from zero
      -- Ticks extend Length pixels from the axes on both sides
      -- The axes and ticks will be in color Color
      -- Only the Y label is drawn at the origin
   private -- Plotting
      type Plot_Info is tagged record
         ID      : Widget_ID;
         X_Min   : Float;
         X_Max   : Float;
         Y_Min   : Float;
         Y_Max   : Float;
         X_Scale : Float;
         Y_Scale : Float;
      end record;
   end Plotting;
private -- Ada_Gui
   type Widget_ID is tagged record
      Value : Natural := 0;
   end record;

   Closed_Text : constant String := "window_closed";
end Ada_Gui;

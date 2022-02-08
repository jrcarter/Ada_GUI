-- An Ada-oriented GUI library that uses a browser as its platform
--
-- Copyright (C) 2021 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Strings.Unbounded;

package Ada_GUI is
   type Widget_Kind_ID is (Audio_Player,   Background_Text, Button, Check_Box, Graphic_Area, Password_Box, Radio_Buttons,
                           Selection_List, Text_Area,       Text_Box);

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
   -- Each application has an Ada-GUI ID; two applications with the same ID cannot run at the same time
   -- (The ID is used as the port for talking to the browser, hence the restriction)
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

   procedure End_GUI with Pre => Set_Up, Post => not Set_Up;
   -- Destroys the GUI

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
   -- Audio sources seem to be relative path names of audio files and URLs
   -- If Controls, then controls are displayed for the player and the user can use them to control the player
   -- Otherwise, no controls are displayed and control of the player must be done by the program

   function New_Background_Text (Row : Positive := 1; Column : Positive := 1; Text : String := ""; Break_Before : Boolean := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Background_Text with contents Text
   -- Like a label, background text is not in any visible widget; unlike a label, background text is not associated with a widget
   -- Background text may contain HTML text attributes such as "<b>...</b>" and embedding "<br>" will cause new lines

   function New_Button (Row : Positive := 1; Column : Positive := 1; Text : String := ""; Break_Before : Boolean := False)
   return Widget_ID With Pre => Set_Up;
   -- Creates a Button with button label Text.
   -- Left clicks on buttons generate events

   function New_Check_Box (Row          : Positive := 1;
                           Column       : Positive := 1;
                           Label        : String   := "";
                           Break_Before : Boolean  := False;
                           Active       : Boolean  := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Check_Box with label Label
   -- If Active, the box will be checked; otherwise, it will be unchecked

   function New_Graphic_Area
      (Row : Positive := 1; Column : Positive := 1; Width : Positive; Height : Positive; Break_Before : Boolean := False)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Graphic_Area of Width by Height pixels
   -- (0, 0) is the upper-left corner (Width, Height) is the lower-right corner

   function New_Password_Box (Row          : Positive := 1;
                              Column       : Positive := 1;
                              Text         : String   := "";
                              Break_Before : Boolean  := False;
                              Label        : String   := "";
                              Width        : Positive := 20)
   return Widget_ID with Pre => Set_Up;
   -- Same as New_Text_Box, but the result box does not echo the characters in it

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
   -- The set of options may be modified later using Insert and Delete
   -- Left clicks on selection lists generate events

   function New_Text_Area (Row          : Positive := 1;
                           Column       : Positive := 1;
                           Text         : String   := "";
                           Break_Before : Boolean  := False;
                           Width        : Positive := 20;
                           Height       : Positive := 2)
   return Widget_ID with Pre => Set_Up;
   -- Creates a new Text_Box with initial content of Text.
   -- Width is width of area in characters
   -- Height is height of area in lines

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
      Data : Ada.Strings.Unbounded.Unbounded_String; -- Event data that was parsed to give Mouse or Key

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

   procedure Set_Visibility (ID : in Widget_ID; Visible : in Boolean := True) With Pre => Set_Up;
   -- Sets whether or not ID is visible
   -- Widgets are visible by default

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
   -- If the user cancels the dialog, returns (Picked => False)
   -- Otherwise, the return value has Picked => True and the Value component contains the full path of the selected file
   -- Until the function returns, events are not available and Next_Event will block
   -- Only one call may proceed at a time; if a call is in progress, another call will return  (Picked => False)

   procedure Set_Source (ID : in Widget_ID; Source : in String) with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Makes Source the audio source for ID

   function Source (ID : Widget_ID) return String with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns the current audio source for ID

   function Ready (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Audio_Player;
   -- Returns True if ID is ready to play its source; False otherwise
   -- There is a perceptible (to a computer) delay between a call to Set_Source and Ready returning True

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
   -- For a Text_Area, embedded LFs cause line breaks

   procedure Set_Text_Aligbnment (ID : in Widget_ID; Alignment : in Alignment_ID) with
      Pre => Set_Up and ID.Kind in Background_Text | Button | Password_Box | Selection_List | Text_Area | Text_Box;
   -- Sets the text aligbnmanet for ID to Alignment

   type Font_Kind_ID is (Proportional, Monospaced);

   procedure Set_Text_Font_Kind (ID : in Widget_ID; Kind : in Font_Kind_ID) with
      Pre => Set_Up and ID.Kind in Background_Text | Button | Password_Box | Selection_List | Text_Area | Text_Box;
   -- Sets the text font kind for ID to Kind
   -- Default is Proportional

   function Multiple_Select (ID : Widget_ID) return Boolean with Pre => Set_Up and ID.Kind = Selection_List;
   -- Returns the value of Multiple_Select used to create ID

   function Text (ID : Widget_ID) return String with
      Pre => Set_Up                                                                                     and
             ID.Kind in Background_Text | Button | Password_Box | Selection_List | Text_Area | Text_Box and
             (if ID.Kind = Selection_List then not ID.Multiple_Select else True);
   -- Returns the text for ID; for a Selection_List with no selection, returns ""
   -- For a Text_Area, line breaks are encoded as LFs

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

   -- Graphic_Area operations for which any part of the drawn element is outside the drawing area work; the extra part is not drawn

   procedure Set_Pixel (ID : in Widget_ID; X : in Integer; Y : in Integer; Color : in Color_Info := To_Color (Black) ) with
      Pre => Set_Up and ID.Kind = Graphic_Area;
   -- If (X, Y) is in the drawing area, sets it to Color

   function Pixel (ID : Widget_ID; X : Integer; Y : Integer) return Color_Info with
      Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Returns the color of the pixel at (X, Y)

   procedure Draw_Line (ID     : in Widget_ID;
                        From_X : in Integer;
                        From_Y : in Integer;
                        To_X   : in Integer;
                        To_Y   : in Integer;
                        Width  : in Positive := 1;
                        Color  : in Color_Info := To_Color (Black) )
   with Pre => Set_Up and ID.Kind = Graphic_Area;
   -- Draws a line from (From_X, From_Y) to (To_X, To_Y) in Color
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
   -- If not Line_Color.None, the arc will have a line along it in Line_Color.Color; if not Fill_Color.None as well, there will also
   -- be lines between the ends of the arc and the center point
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

   procedure Replace_Pixels (ID : in Widget_ID; Image : in Widget_ID; X : in Integer; Y : in Integer) with
      Pre => Set_Up and ID.Kind = Graphic_Area and Image.Kind = Graphic_Area;
   -- Replaces pixels in ID starting at (X, Y) and extending to the right by the width of Image, and down by the height of Image,
   -- with the pixels in Image

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

   procedure Insert (ID : in Widget_ID; Text : in String; Before : in Positive := Integer'Last) with
      Pre => Set_Up and ID.Kind = Selection_List;
   -- if Before > ID.Length, appends Text to the options of ID
   -- Otherwise, inserts Text as the option with index Before, moving up the options previously at Before .. ID.Length

   procedure Delete (ID : in Widget_ID; Index : in Positive) with
      Pre => Set_Up and ID.Kind = Selection_List and Index in 1 .. ID.Length;
   -- Deletes the option at Index, moving down the options previously at Index + 1 .. ID.Length
private -- Ada_Gui
   type Widget_ID is tagged record
      Value : Natural := 0;
   end record;

   Closed_Text : constant String := "window_closed";
end Ada_Gui;

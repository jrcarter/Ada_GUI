-- An Ada-oriented GUI library
-- Implementation derived from Gnoga
--
-- Copyright (C) 2022 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;
with Ada.Strings.Fixed;

with Ada_GUI.Gnoga.Application;
with Ada_GUI.Gnoga.Gui.Element.Canvas.Context_2D;
with Ada_GUI.Gnoga.Gui.Element.Common;
with Ada_GUI.Gnoga.Gui.Element.Form;
with Ada_GUI.Gnoga.Gui.Element.Multimedia;
with Ada_GUI.Gnoga.Gui.View.Console;
with Ada_GUI.Gnoga.Gui.View.Grid;
with Ada_GUI.Gnoga.Gui.Window;
with Ada_GUI.Gnoga.Colors;

package body Ada_GUI is
   type Form_Set     is array (Positive range <>, Positive range <>) of Gnoga.Gui.Element.Form.Form_Type;
   type Form_Ptr     is access Form_Set; -- Extensive use of access due to nature of Gnoga

   package Column_Maps is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Boolean);
   package Row_Maps    is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Column_Maps.Vector, "=" => Column_Maps."=");

   Window : Gnoga.Gui.Window.Window_Type;
   View   : Gnoga.Gui.View.Console.Console_View_Type;
   Grid   : Gnoga.Gui.View.Grid.Grid_View_Type;
   Form   : Form_Ptr;
   Valid  : Row_Maps.Vector; -- Valid (Row) (Column) is True if (Row, Column) is an Area
   Setup  : Boolean := False with Atomic;

   function Set_Up return Boolean is (Setup);

   function Converted (Alignment : Alignment_ID) return Gnoga.Gui.Element.Alignment_Type is
      (case Alignment is
       when Left   => Gnoga.Gui.Element.Left,
       when Center => Gnoga.Gui.Element.Center,
       when Right  => Gnoga.Gui.Element.Right);

   procedure Set_Up (Grid  : in Grid_Set := (1 => (1 => (others => <>) ) );
                     ID    : in Positive := 8080;
                     Title : in String   := "Ada-GUI Application";
                     Icon  : in String   := "favicon.ico")
   is
      G_Grid : Gnoga.Gui.View.Grid.Grid_Rows_Type (Grid'Range (1), Grid'Range (2) );
   begin -- Set_Up
      Fill_Grid : for Row in Grid'Range (1) loop
         Valid.Append (New_Item => Column_Maps.Empty_Vector);

         Grid_Cols : for Column in Grid'Range (2) loop
            G_Grid (Row, Column) := (if Grid (Row, Column).Kind = Area then Gnoga.Gui.View.Grid.COL else Gnoga.Gui.View.Grid.SPN);
            Valid (Row).Append (New_Item =>  Grid (Row, Column).Kind = Area);
         end loop Grid_Cols;
      end loop Fill_Grid;

      Gnoga.Application.Initialize (Main_Window => Window, ID => ID, Title => Title, Icon => Icon);
      View.Create (Parent => Window);
      Ada_GUI.Grid.Create (Parent => View, Layout => G_Grid);
      Form := new Form_Set (Grid'Range (1), Grid'Range (2) );

      All_Rows : for I in Form'Range (1) loop
         All_Columns : for J in Form'Range (2) loop
            if Grid (I, J).Kind = Area then
               Form (I, J).Create (Parent => Ada_GUI.Grid.Panel (I, J).all);
               Form (I, J).Text_Alignment (Value => Converted (Grid (I, J).Alignment) );
            end if;
         end loop All_Columns;
      end loop All_Rows;

      Setup := True;
   end Set_Up;

   type Radio_Info is record
      Button : Gnoga.Gui.Element.Form.Radio_Button_Type;
      Label  : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   type Radio_List is array (Positive range <>) of Radio_Info;
   type Radio_Ptr is access Radio_List;

   type Widget_Info (Kind : Widget_Kind_ID := Button) is record
      case Kind is
      when Audio_Player =>
         Audio : Gnoga.Gui.Element.Multimedia.Audio_Access;
      when Background_Text =>
         Background : Gnoga.Gui.Element.Common.Span_Access;
      when Button =>
         Switch : Gnoga.Gui.Element.Common.Button_Access;
      when Check_Box =>
         Check       : Gnoga.Gui.Element.Form.Check_Box_Access;
         Check_Label : Gnoga.Gui.Element.Form.Label_Access;
      when Graphic_Area =>
         Canvas : Gnoga.Gui.Element.Canvas.Canvas_Access;
      when Password_Box =>
         Password       : Gnoga.Gui.Element.Form.Password_Access;
         Password_Label : Gnoga.Gui.Element.Form.Label_Access;
      when Radio_Buttons =>
         Radio : Radio_Ptr;
      when Selection_List =>
         Selector : Gnoga.Gui.Element.Form.Selection_Access;
         Multi    : Boolean;
      when Text_Area =>
         Area : Gnoga.Gui.Element.Form.Text_Area_Access;
      when Text_Box =>
         Box       : Gnoga.Gui.Element.Form.Text_Access;
         Box_Label : Gnoga.Gui.Element.Form.Label_Access;
      end case;
   end record;

   package Widget_Lists is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Widget_Info);

   Widget_List : Widget_Lists.Vector;

   function Kind (ID : Widget_ID) return Widget_Kind_ID is (Widget_List (ID.Value).Kind);

   function Adjusted (Row : in Positive; Column : in Positive) return Positive;
   -- If (Row, Column) is an Extension, reduces Column until (Row, Column) is an Area

   procedure Break (Desired : in Boolean; Row : in Positive; Column : in Positive);
   -- If Desired, make the next thing declared in Form (Row, Column) appear below existing things there

   function Adjusted (Row : in Positive; Column : in Positive) return Positive is
   begin -- Adjusted
      Find : for C in reverse 1 .. Column loop
         if Valid (Row) (C) then
            return C;
         end if;
      end loop Find;

      raise Program_Error;
   end Adjusted;

   procedure Break (Desired : in Boolean; Row : in Positive; Column : in Positive) is
      -- Empty
   begin -- Break
      if Desired then
         Form (Row, Column).New_Line;
      end if;
   end Break;


   function New_Audio_Player (Row          : Positive := 1;
                              Column       : Positive := 1;
                              Break_Before : Boolean  := False;
                              Source       : String   := "";
                              Controls     : Boolean  := True)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Audio_Player);
   begin -- New_Audio_Player
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Audio := new Gnoga.Gui.Element.Multimedia.Audio_Type;
      Widget.Audio.Create (Parent   => Form (Row, Adjusted (Row, Column) ),
                           Source   => Source,
                           Controls => Controls,
                           Preload  => True,
                           ID       => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Audio_Player;

   function New_Background_Text (Row : Positive := 1; Column : Positive := 1; Text : String := ""; Break_Before : Boolean := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Background_Text);
   begin -- New_Background_Text
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Background := new Gnoga.Gui.Element.Common.Span_Type;
      Widget.Background.Create (Parent => Form (Row, Adjusted (Row, Column) ), Content => Text, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Background_Text;

   function New_Button (Row : Positive := 1; Column : Positive := 1; Text : String := ""; Break_Before : Boolean := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Button);
   begin -- New_Button
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Switch := new Gnoga.Gui.Element.Common.Button_Type;
      Widget.Switch.Create (Parent => Form (Row, Adjusted (Row, Column) ), Content => Text, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Button;

   function New_Check_Box (Row          : Positive := 1;
                           Column       : Positive := 1;
                           Label        : String   := "";
                           Break_Before : Boolean  := False;
                           Active       : Boolean  := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Check_Box);
   begin -- New_Check_Box
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Check := new Gnoga.Gui.Element.Form.Check_Box_Type;
      Widget.Check.Create (Form => Form (Row, Adjusted (Row, Column) ), ID => ID.Value'Image);
      Widget.Check.Checked (Value => Active);
      Widget.Check_Label := new Gnoga.Gui.Element.Form.Label_Type;
      Widget.Check_Label.Create
         (Form => Form (Row, Adjusted (Row, Column) ), Label_For => Widget.Check.all, Content => Label, Auto_Place => False);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Check_Box;

   function New_Graphic_Area
      (Row : Positive := 1; Column : Positive := 1; Width : Positive; Height : Positive; Break_Before : Boolean := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Graphic_Area);
   begin -- New_Graphic_Area
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Canvas := new Gnoga.Gui.Element.Canvas.Canvas_Type;
      Widget.Canvas.Create (Parent => Form (Row, Adjusted (Row, Column) ), Width => Width, Height => Height, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Graphic_Area;

   function New_Password_Box (Row          : Positive := 1;
                              Column       : Positive := 1;
                              Text         : String   := "";
                              Break_Before : Boolean  := False;
                              Label        : String   := "";
                              Width        : Positive := 20)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Password_Box);
   begin -- New_Password_Box
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Password := new Gnoga.Gui.Element.Form.Password_Type;
      Widget.Password.Create (Form => Form (Row, Adjusted (Row, Column) ), Size => Width, Value => Text, ID => ID.Value'Image);
      Widget.Password_Label := new Gnoga.Gui.Element.Form.Label_Type;
      Widget.Password_Label.Create
         (Form => Form (Row, Adjusted (Row, Column) ), Label_For => Widget.Password.all, Content => Label);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Password_Box;

   use Ada.Strings.Unbounded;

   Next_Button : Positive := 1;

   function New_Radio_Buttons (Row          : Positive := 1;
                               Column       : Positive := 1;
                               Label        : Text_List;
                               Break_Before : Boolean        := False;
                               Orientation  : Orientation_ID := Vertical)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Radio_Buttons);
      Name   : String := Next_Button'Image;
   begin -- New_Radio_Buttons
      Name (Name'First) := 'R';
      Next_Button := Next_Button + 1;
      Widget.Radio := new Radio_List (Label'Range);
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );

      All_Buttons : for I in Label'Range loop
         Widget.Radio (I).Button.Create (Form    => Form (Row, Adjusted (Row, Column) ),
                                         Checked => I = Label'First,
                                         Name    => Name,
                                         ID      => I'Image & 'R' & ID.Value'Image);
         Widget.Radio (I).Label.Create (Form       => Form (Row, Adjusted (Row, Column) ),
                                        Label_For  => Widget.Radio (I).Button,
                                        Content    => To_String (Label (I) ),
                                        Auto_Place => False);

         if I < Label'Last and Orientation = Vertical then
            Form (Row, Adjusted (Row, Column) ).New_Line;
         end if;
      end loop All_Buttons;

      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Radio_Buttons;

   function New_Selection_List (Row             : Positive  := 1;
                                Column          : Positive  := 1;
                                Text            : Text_List := (1 .. 0 => <>);
                                Break_Before    : Boolean  := False;
                                Height          : Positive := 1;
                                Multiple_Select : Boolean  := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Selection_List);
   begin -- New_Selection_List
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Selector := new Gnoga.Gui.Element.Form.Selection_Type;
      Widget.Selector.Create (Form            => Form (Row, Adjusted (Row, Column) ),
                              Multiple_Select => Multiple_Select,
                              Visible_Lines   => Height,
                              ID              => ID.Value'Image);
      Widget.Multi := Multiple_Select;
      Widget_List.Append (New_Item => Widget);

      Add_Options : for I in Text'Range loop
         Widget.Selector.Add_Option (Value => To_String (Text (I) ), Text => To_String (Text (I) ) );
      end loop Add_Options;

      return ID;
   end New_Selection_List;

   function New_Text_Area (Row          : Positive := 1;
                           Column       : Positive := 1;
                           Text         : String   := "";
                           Break_Before : Boolean  := False;
                           Width        : Positive := 20;
                           Height       : Positive := 2)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Text_Area);
   begin -- New_Text_Area
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Area := new Gnoga.Gui.Element.Form.Text_Area_Type;
      Widget.Area.Create
         (Form => Form (Row, Adjusted (Row, Column) ), Columns => Width, Rows => Height, Value => Text, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Text_Area;

   function New_Text_Box (Row          : Positive := 1;
                          Column       : Positive := 1;
                          Text         : String   := "";
                          Break_Before : Boolean  := False;
                          Label        : String   := "";
                          Placeholder  : String   := "";
                          Width        : Positive := 20)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Text_Box);
   begin -- New_Text_Box
      Break (Desired => Break_Before, Row => Row, Column => Adjusted (Row, Column) );
      Widget.Box := new Gnoga.Gui.Element.Form.Text_Type;
      Widget.Box.Create (Form => Form (Row, Adjusted (Row, Column) ), Size => Width, Value => Text, ID => ID.Value'Image);
      Widget.Box_Label := new Gnoga.Gui.Element.Form.Label_Type;
      Widget.Box_Label.Create (Form => Form (Row, Adjusted (Row, Column) ), Label_For => Widget.Box.all, Content => Label);

      if Placeholder /= "" then
         Widget.Box.Place_Holder (Value => Placeholder);
      end if;

      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Text_Box;


   procedure Set_Title (Title : in String) is
      -- Empty
   begin -- Set_Title
      Window.Document.Title (Value => Title);
   end Set_Title;

   procedure Show_Message_Box (Text : in String) is
      -- Empty
   begin -- Show_Message_Box
      Window.Alert (Message => Text);
   end Show_Message_Box;

   protected Dialog_Control is -- Controls multiple calls to dialogs and calls to Next_Event while a call to a dialog is in progress
      procedure Set_Ongoing (Value : in Boolean);
      -- Makes Ongoing return Value

      function Ongoing return Boolean;
      -- Returns the last value passed to Set_Ongoing; False initially

      entry Block;
      -- Blocks the caller until not Ongoing
   private -- Dialog_Control
      In_Progress : Boolean := False;
   end Dialog_Control;

   function Next_Event (Timeout : Duration := Duration'Last) return Next_Result_Info is separate;

   procedure Set_Visibility (ID : in Widget_ID; Visible : in Boolean := True) is
      Widget : Widget_Info renames Widget_List (ID.Value);
   begin -- Set_Visibility
      case Widget.Kind is
      when Audio_Player =>
         Widget.Audio.Visible (Value => Visible);
      when Background_Text =>
         Widget.Background.Visible (Value => Visible);
      when Button =>
         Widget.Switch.Visible (Value => Visible);
      when Check_Box =>
         Widget.Check.Visible (Value => Visible);
         Widget.Check_Label.Visible (Value => Visible);
      when Graphic_Area =>
         Widget.Canvas.Visible (Value => Visible);
      when Password_Box =>
         Widget.Password.Visible (Value => Visible);
         Widget.Password_Label.Visible (Value => Visible);
      when Radio_Buttons =>
         All_Buttons : for I in Widget.Radio'Range loop
            Widget.Radio (I).Button.Visible (Value => Visible);
            Widget.Radio (I).Label.Visible (Value => Visible);
         end loop All_Buttons;
      when Selection_List =>
         Widget.Selector.Visible (Value => Visible);
      when Text_Area =>
         Widget.Area.Visible (Value => Visible);
      when Text_Box =>
         Widget.Box.Visible (Value => Visible);
         Widget.Box_Label.Visible (Value => Visible);
      end case;
   end Set_Visibility;

   procedure Log (Message : in String) renames Gnoga.Log;

   package body Dialogs is separate;

   protected body Dialog_Control is
      procedure Set_Ongoing (Value : in Boolean) is
         -- Empty
      begin -- Set_Ongoing
         In_Progress := Value;
      end Set_Ongoing;

      function Ongoing return Boolean is (In_Progress);

      entry Block when not In_Progress is
         -- Empty
      begin -- Block
         null;
      end Block;
   end Dialog_Control;


   procedure Set_Source (ID : in Widget_ID; Source : in String) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Source
      Widget.Audio.Media_Source (Source => Source);
   end Set_Source;

   function Source (ID : Widget_ID) return String is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Source
      return Widget.Audio.Media_Source;
   end Source;

   function Ready (ID : Widget_ID) return Boolean is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Ready
      return Widget.Audio.Ready_To_Play;
   end Ready;

   procedure Play (ID : in Widget_ID) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Play
      Widget.Audio.Play;
   end Play;

   procedure Pause (ID : in Widget_ID) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Pause
      Widget.Audio.Pause;
   end Pause;

   function Paused (ID : Widget_ID) return Boolean is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Paused
      return Widget.Audio.Paused;
   end Paused;

   function Playback_Ended (ID : Widget_ID) return Boolean is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Playback_Ended
      return Widget.Audio.Playback_Ended;
   end Playback_Ended;

   function Length (ID : Widget_ID) return Float is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Length
      return Widget.Audio.Media_Duration;
   end Length;

   procedure Set_Position (ID : in Widget_ID; Position : in Float) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Position
      Widget.Audio.Media_Position (Seconds => Position);
   end Set_Position;

   function Position (ID : Widget_ID) return Float is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Position
      return Widget.Audio.Media_Position;
   end Position;

   procedure Set_Text (ID : in Widget_ID; Text : in String) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Text
      case Widget.Kind is
      when Background_Text =>
         Widget.Background.Inner_HTML (Value => Text);
      when Button =>
         Widget.Switch.Text (Value => Text);
      when Password_Box =>
         Widget.Password.Value (Value => Text);
      when Text_Area =>
         Widget.Area.Value (Value => Text);
      when Text_Box =>
         Widget.Box.Value (Value => Text);
      when others =>
         raise Program_Error;
      end case;
   end Set_Text;

   procedure Set_Text_Alignment (ID : in Widget_ID; Alignment : in Alignment_ID) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Text_Alignment
      case Widget.Kind is
      when Background_Text =>
         Widget.Background.Text_Alignment (Value => Converted (Alignment) );
      when Button =>
         Widget.Switch.Text_Alignment (Value => Converted (Alignment) );
      when Password_Box =>
         Widget.Password.Text_Alignment (Value => Converted (Alignment) );
      when Selection_List =>
         Widget.Selector.Text_Alignment (Value => Converted (Alignment) );
      when Text_Area =>
         Widget.Area.Text_Alignment (Value => Converted (Alignment) );
      when Text_Box =>
         Widget.Box.Text_Alignment (Value => Converted (Alignment) );
      when others =>
         raise Program_Error;
      end case;
   end Set_Text_Alignment;

   procedure Set_Text_Font_Kind (ID : in Widget_ID; Kind : in Font_Kind_ID) is
      function Family return String is
         (if Kind = Proportional then "sans-serif" else "monospace");

      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Text_Font_Kind
      case Widget.Kind is
      when Background_Text =>
         Widget.Background.Font (Family => Family);
      when Button =>
         Widget.Switch.Font (Family => Family);
      when Password_Box =>
         Widget.Password.Font (Family => Family);
      when Selection_List =>
         Widget.Selector.Font (Family => Family);
      when Text_Area =>
         Widget.Area.Font (Family => Family);
      when Text_Box =>
         Widget.Box.Font (Family => Family);
      when others =>
         raise Program_Error;
      end case;
   end Set_Text_Font_Kind;

   procedure Set_Label (ID : in Widget_ID; Text : in String) is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Set_Label
      case Widget.Kind is
      when Check_Box =>
         Widget.Check_Label.Inner_HTML (Value => Text);
      when Password_Box =>
         Widget.Password_Label.Inner_HTML (Value => Text);
      when Text_Box =>
         Widget.Box_Label.Inner_HTML (Value => Text);
      when others =>
         raise Program_Error;
      end case;
   end Set_Label;

   procedure Set_Read_Only (ID : in Widget_ID; Read_Only : in Boolean := True) is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Set_Read_Only
      case Widget.Kind is
      when Text_Area =>
         Widget.Area.Read_Only (Value => Read_Only);
      when Text_Box =>
         Widget.Box.Read_Only (Value => Read_Only);
      when others =>
         raise Program_Error;
      end case;
   end Set_Read_Only;

   function Multiple_Select (ID : Widget_ID) return Boolean is (Widget_List (ID.Value).Element.Multi);

   function Text (ID : Widget_ID) return String is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Text
      case Widget.Kind is
      when Background_Text =>
         return Widget.Background.Text;
      when Button =>
         return Widget.Switch.Text;
      when Password_Box =>
         return Widget.Password.Value;
      when Selection_List =>
         return Widget.Selector.Value;
      when Text_Area =>
         return Widget.Area.Value;
      when Text_Box =>
         return Widget.Box.Value;
      when others =>
         raise Program_Error;
      end case;
   end Text;

   procedure Set_Active (ID : in Widget_ID; Active : in Boolean) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Active
      Widget.Check.Checked (Value => Active);
   end Set_Active;

   function Active (ID : Widget_ID) return Boolean is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Active
      return Widget.Check.Checked;
   end Active;

   function AG_Color (Color : Gnoga.RGBA_Type) return Color_Info is
      (Red   => RGB_Value (Color.Red),
       Green => RGB_Value (Color.Green),
       Blue  => RGB_Value (Color.Blue),
       Alpha => Alpha_Value (Color.Alpha) );

   function To_Color (Color : Color_ID) return Color_Info is
      (AG_Color (Gnoga.Colors.To_RGBA (Gnoga.Colors.Color_Enumeration'Val (Color_ID'Pos (Color) ) ) ) );

   function Gnoga_Color (Color : Color_Info) return Gnoga.RGBA_Type is
      (Red   => Gnoga.Color_Type (Color.Red),
       Green => Gnoga.Color_Type (Color.Green),
       Blue  => Gnoga.Color_Type (Color.Blue),
       Alpha => Gnoga.Alpha_Type (Color.Alpha) );

   function To_ID (Color : Color_Info) return Color_ID is
      (Color_ID'Val (Gnoga.Colors.Color_Enumeration'Pos (Gnoga.Colors.To_Color_Enumeration (Gnoga_Color (Color) ) ) ) );

   function Gnoga_Pixel (Color : Color_Info) return Gnoga.Pixel_Type is
      (Red   => Gnoga.Color_Type (Color.Red),
       Green => Gnoga.Color_Type (Color.Green),
       Blue  => Gnoga.Color_Type (Color.Blue),
       Alpha => Gnoga.Color_Type (255.0 * Color.Alpha) );

   procedure Set_Background_Color (Color : in Color_Info) is
      -- Empty
   begin -- Set_Background_Color
      View.Background_Color (RGBA => Gnoga_Color (Color) );

      All_Rows : for Row in Form'Range (1) loop
         All_Columns : for Column in Form'Range (2) loop
            if Valid (Row) (Column) then
               Form (Row, Column).Background_Color (RGBA => Gnoga_Color (Color) );
            end if;
         end loop All_Columns;
      end loop All_Rows;
   end Set_Background_Color;

   procedure Set_Background_Color (ID : Widget_ID; Color : in Color_Info := To_Color (Black) ) is
     Widget  : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Background_Color
      case Widget.Kind is
      when Audio_Player =>
         Widget.Audio.Background_Color (RGBA => Gnoga_Color (Color) );
      when Background_Text =>
         Widget.Background.Background_Color (RGBA => Gnoga_Color (Color) );
      when Button =>
         Widget.Switch.Background_Color (RGBA => Gnoga_Color (Color) );
      when Check_Box =>
         Widget.Check.Background_Color (RGBA => Gnoga_Color (Color) );
         Widget.Check_Label.Background_Color (RGBA => Gnoga_Color (Color) );
      when Graphic_Area =>
         Widget.Canvas.Background_Color (RGBA => Gnoga_Color (Color) );
      when Password_Box =>
         Widget.Password.Background_Color (RGBA => Gnoga_Color (Color) );
         Widget.Password_Label.Background_Color (RGBA => Gnoga_Color (Color) );
      when Radio_Buttons =>
         All_Buttons : for I in Widget.Radio'Range loop
            Widget.Radio (I).Button.Background_Color (RGBA => Gnoga_Color (Color) );
            Widget.Radio (I).Label.Background_Color (RGBA => Gnoga_Color (Color) );
         end loop All_Buttons;
      when Selection_List =>
         Widget.Selector.Background_Color (RGBA => Gnoga_Color (Color) );
      when Text_Area =>
         Widget.Area.Background_Color (RGBA => Gnoga_Color (Color) );
      when Text_Box =>
          Widget.Box.Background_Color (RGBA => Gnoga_Color (Color) );
          Widget.Box_Label.Background_Color (RGBA => Gnoga_Color (Color) );
      end case;
   end Set_Background_Color;

   procedure Set_Foreground_Color (ID : Widget_ID; Color : in Color_Info := To_Color (Black) ) is
     Widget  : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Foreground_Color
      case Widget.Kind is
      when Audio_Player =>
         Widget.Audio.Color (RGBA => Gnoga_Color (Color) );
      when Background_Text =>
         Widget.Background.Color (RGBA => Gnoga_Color (Color) );
      when Button =>
         Widget.Switch.Color (RGBA => Gnoga_Color (Color) );
      when Check_Box =>
         Widget.Check.Color (RGBA => Gnoga_Color (Color) );
         Widget.Check_Label.Color (RGBA => Gnoga_Color (Color) );
      when Graphic_Area =>
         Widget.Canvas.Color (RGBA => Gnoga_Color (Color) );
      when Password_Box =>
         Widget.Password.Color (RGBA => Gnoga_Color (Color) );
         Widget.Password_Label.Color (RGBA => Gnoga_Color (Color) );
      when Radio_Buttons =>
         All_Buttons : for I in Widget.Radio'Range loop
            Widget.Radio (I).Button.Color (RGBA => Gnoga_Color (Color) );
            Widget.Radio (I).Label.Color (RGBA => Gnoga_Color (Color) );
         end loop All_Buttons;
      when Selection_List =>
         Widget.Selector.Color (RGBA => Gnoga_Color (Color) );
      when Text_Area =>
         Widget.Area.Color (RGBA => Gnoga_Color (Color) );
      when Text_Box =>
          Widget.Box.Color (RGBA => Gnoga_Color (Color) );
          Widget.Box_Label.Color (RGBA => Gnoga_Color (Color) );
      end case;
   end Set_Foreground_Color;

   procedure Set_Pixel (ID : in Widget_ID; X : in Integer; Y : in Integer; Color : in Color_Info := To_Color (Black) ) is
      G_Color : constant Gnoga.Pixel_Type := Gnoga_Pixel (Color);

      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Set_Pixel
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Pixel (X => X, Y => Y, Color => G_Color);
   end Set_Pixel;

   function AG_Color (Color : Gnoga.Pixel_Type) return Color_Info is
      (Red   => RGB_Value (Color.Red),
       Green => RGB_Value (Color.Green),
       Blue  => RGB_Value (Color.Blue),
       Alpha => Float (Color.Alpha) / 255.0);

   function Pixel (ID : Widget_ID; X : Integer; Y : Integer) return Color_Info is
      Widget : constant Widget_Info := Widget_List (ID.Value);

      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      G_Color : Gnoga.Pixel_Type;
   begin -- Pixel_Type
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      G_Color := Context.Pixel (X, Y);

      return AG_Color (G_Color);
   end Pixel;

   procedure Draw_Line (ID     : in Widget_ID;
                        From_X : in Integer;
                        From_Y : in Integer;
                        To_X   : in Integer;
                        To_Y   : in Integer;
                        Width  : in Positive := 1;
                        Color  : in Color_Info := To_Color (Black) )
   is
      G_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Color);

      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Draw_Line
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Stroke_Color (Value => G_Color);
      Context.Line_Width (Value => Width);
      Context.Begin_Path;
      Context.Move_To (X => From_X, Y => From_Y);
      Context.Line_To (X => To_X, Y => To_Y);
      Context.Stroke;
   end Draw_Line;

   procedure Draw_Rectangle (ID         : in Widget_ID;
                             From_X     : in Integer;
                             From_Y     : in Integer;
                             To_X       : in Integer;
                             To_Y       : in Integer;
                             Line_Color : in Optional_Color := (None => False, Color => To_Color (Black) );
                             Fill_Color : in Optional_Color := (None => True) )
   is
      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Draw_Rectangle
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Line_Width (Value => 1);
      Context.Begin_Path;
      Context.Rectangle (Rectangle => (X      => Integer'Min (From_X, To_X),
                                       Y      => Integer'Min (From_Y, To_Y),
                                       Width  => abs (From_X - To_X) + 1,
                                       Height => abs (From_Y - To_Y) + 1) );

      if not Fill_Color.None then
         Convert_Fill : declare
            F_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Fill_Color.Color);
         begin -- Convert_Fill
            Context.Fill_Color (Value => F_Color);
            Context.Fill;
         end Convert_Fill;
      end if;

      if not Line_Color.None then
         Convert_Line : declare
            L_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Line_Color.Color);
         begin -- Convert_Line
            Context.Stroke_Color (Value => L_Color);
            Context.Stroke;
         end Convert_Line;
      end if;
   end Draw_Rectangle;

   procedure Draw_Arc (ID                : in Widget_ID;
                       X                 : in Integer;
                       Y                 : in Integer;
                       Radius            : in Positive;
                       Start             : in Float;
                       Stop              : in Float;
                       Counter_Clockwise : in Boolean := False;
                       Line_Color        : in Optional_Color := (None => False, Color => To_Color (Black) );
                       Fill_Color        : in Optional_Color := (None => True) )
   is
      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Draw_Arc
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Line_Width (Value => 1);
      Context.Begin_Path;

      if not Fill_Color.None then
         Context.Move_To (X => X, Y => Y);
         Context.Line_To (X => X + Integer (Float (Radius) * Ada.Numerics.Elementary_Functions.Cos (Start) ),
                          Y => Y + Integer (Float (Radius) * Ada.Numerics.Elementary_Functions.Sin (Start) ) );
      end if;

      Context.Arc_Radians
         (X => X, Y => Y, Radius => Radius, Starting_Angle => Start, Ending_Angle => Stop, Counter_Clockwise => Counter_Clockwise);

      if not Fill_Color.None then
         Convert_Fill : declare
            F_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Fill_Color.Color);
         begin -- Convert_Fill
            Context.Close_Path;
            Context.Fill_Color (Value => F_Color);
            Context.Fill;
         end Convert_Fill;
      end if;

      if not Line_Color.None then
         Convert_Line : declare
            L_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Line_Color.Color);
         begin -- Convert_Line
            Context.Stroke_Color (Value => L_Color);
            Context.Stroke;
         end Convert_Line;
      end if;
   end Draw_Arc;

   procedure Draw_Text (ID         : in Widget_ID;
                        X          : in Integer;
                        Y          : in Integer;
                        Text       : in String;
                        Height     : in Positive       := 20;
                        Line_Color : in Optional_Color := (None => True);
                        Fill_Color : in Optional_Color := (None => False, Color => To_Color (Black) ) )
   is
      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Draw_Text
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Font (Height => Height'Image & "px");
      Context.Line_Width (Value => 1);

      if not Fill_Color.None then
         Convert_Fill : declare
            F_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Fill_Color.Color);
         begin -- Convert_Fill
            Context.Fill_Color (Value => F_Color);
            Context.Fill_Text (Text => Text, X => X, Y => Y);
         end Convert_Fill;
      end if;

      if not Line_Color.None then
         Convert_Line : declare
            L_Color : constant Gnoga.RGBA_Type := Gnoga_Color (Line_Color.Color);
         begin -- Convert_Line
            Context.Stroke_Color (Value => L_Color);
            Context.Stroke_Text (Text => Text, X => X, Y => Y);
         end Convert_Line;
      end if;
   end Draw_Text;

   procedure Replace_Pixels (ID : in Widget_ID; Image : in Widget_ID; X : in Integer; Y : in Integer) is
      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Replace_Pixels
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Draw_Image (Image => Widget_List (Image.Value).Canvas.all, X => X, Y => Y);
   end Replace_Pixels;

   function Num_Buttons (ID : Widget_ID) return Positive is
      (Widget_List (ID.Value).Radio'Length);

   procedure Set_Active (ID : in Widget_ID; Index : in Positive; Active : in Boolean) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Active
      Widget.Radio (Index).Button.Checked (Value => Active);
   end Set_Active;

   function Active (ID : Widget_ID; Index : Positive) return Boolean is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Active
      return Widget.Radio (Index).Button.Checked;
   end Active;

   function Active (ID : Widget_ID) return Positive is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Active
      Check : for I in Widget.Radio'Range loop
         if Widget.Radio (I).Button.Checked then
            return I;
         end if;
      end loop Check;

      return Widget.Radio'Length + 1;
   end Active;

   procedure Set_Label (ID : in Widget_ID; Index : in Positive; Text : in String) is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Set_Label
      Widget.Radio (Index).Label.Inner_HTML (Value => Text);
   end Set_Label;

   function Length (ID : Widget_ID) return Natural is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Length
      return Widget.Selector.Length;
   end Length;

   procedure Clear (ID : in Widget_ID) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Clear
      Widget.Selector.Empty_Options;
   end Clear;

   procedure Set_Selected (ID : in Widget_ID; Index : in Positive; Selected : in Boolean := True) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Set_Selected
      Widget.Selector.Selected (Index => Index, Value => Selected);
   end Set_Selected;

   function Selected (ID : Widget_ID) return Natural is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Selected
      return Widget.Selector.Selected_Index;
   end Selected;

   function Selected (ID : Widget_ID; Index : Positive) return Boolean is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Selected
      return Widget.Selector.Selected (Index);
   end Selected;

   function Text (ID : Widget_ID; Index : Positive) return String is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Text
      return Widget.Selector.all.Value (Index);
   end Text;

   procedure Insert (ID : in Widget_ID; Text : in String; Before : in Positive := Integer'Last) is
      Widget : Widget_Info := Widget_List (ID.Value);

      Index : constant Natural := (if Before > Widget.Selector.Length then 0 else Before);
   begin -- Insert
      Widget.Selector.Add_Option (Value => Text, Text => Text, Index => Index);
   end Insert;

   procedure Delete (ID : in Widget_ID; Index : in Positive) is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Delete
      Widget.Selector.Remove_Option (Index => Index);
   end Delete;

   procedure End_GUI is
      -- Empty
   begin -- End_GUI
      Gnoga.Application.End_Application;
      Setup := False;
   end End_GUI;
end Ada_Gui;

-- Proof of concept of an Ada-oriented GUI interface, with only those things needed to implement Random_Int
-- Quick and dirty implementation on top of Gnoga, not ready for prime time
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Window;
with Gnoga.Types.Colors;

package body Ada_GUI is
   type Form_Set is array (Positive range <>, Positive range <>) of Gnoga.Gui.Element.Form.Form_Type;
   type Form_Ptr is access Form_Set;

   Window : Gnoga.Gui.Window.Window_Type;
   Grid   : Gnoga.Gui.View.Grid.Grid_View_Type;
   Form   : Form_Ptr;
   Setup  : Boolean := False with Atomic;

   function Set_Up return Boolean is (Setup);

   procedure Set_Up (Grid : in Grid_Set := (1 => (1 => Center) ) ) is
      function Convert (Alignment : Alignment_ID) return Gnoga.Gui.Element.Alignment_Type is
      (case Alignment is
       when Left   => Gnoga.Gui.Element.Left,
       when Center => Gnoga.Gui.Element.Center,
       when Right  => Gnoga.Gui.Element.Right);
   begin -- Set_Up
      Ada_GUI.Grid.Create (Parent => Window, Layout => (Grid'Range (1) => (Grid'Range (2) => Gnoga.Gui.View.Grid.COL) ) );
      Form := new Form_Set (Grid'Range (1), Grid'Range (2) );

      All_Rows : for I in Form'Range (1) loop
         All_Columns : for J in Form'Range (2) loop
            Form (I, J).Create (Parent => Ada_GUI.Grid.Panel (I, J).all);
            Form (I, J).Text_Alignment (Value => Convert (Grid (I, J) ) );
         end loop All_Columns;
      end loop All_Rows;

      Setup := True;
   end Set_Up;

   type Radio_Info is record
      Button : Gnoga.Gui.Element.Form.Radio_Button_Type;
      Label  : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   type Radio_List is array (Positive range <>) of Radio_Info;
   type Radio_List_Ptr is access Radio_List;

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
      when Radio_Buttons =>
         Radio : Radio_List_Ptr;
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

   function Kind (ID : Widget_ID) return Widget_Kind_ID is (Widget_List (ID.Value).Element.Kind);

   package Event_Queue_IF is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Event_Info);
   package Event_Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Event_Queue_IF);

   Event_Queue : Event_Queues.Queue;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      ID     : constant Widget_ID   := (Value => Integer'Value (Object.ID) );
      Widget : constant Widget_Info := Widget_List (ID.Value);

      Event : Event_Info (Widget_Kind => Widget.Kind);
   begin -- On_Click
      if Widget.Kind not in Button | Selection_List then
         return;
      end if;

      Event.ID := ID;
      Event.Event_Kind := Left_Click;
      Event_Queue.Enqueue (New_Item => Event);
   end On_Click;

   procedure Break (Desired : in Boolean; Row : in Positive; Column : in Positive) is
      -- Empty
   begin -- Break
      if Desired then
         Form (Row, Column).New_Line;
      end if;
   end Break;


   function New_Audio_Player (Row          : Positive;
                              Column       : Positive;
                              Break_Before : Boolean := False;
                              Source       : String  := "";
                              Controls     : Boolean := True)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Audio_Player);
   begin -- New_Audio_Player
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Audio := new Gnoga.Gui.Element.Multimedia.Audio_Type;
      Widget.Audio.Create
         (Parent => Form (Row, Column), Source => Source, Controls => Controls, Preload => True, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Audio_Player;

   function New_Background_Text (Row : Positive; Column : Positive; Text : String; Break_Before : Boolean := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Background_Text);
   begin -- New_Background_Text
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Background := new Gnoga.Gui.Element.Common.Span_Type;
      Widget.Background.Create (Parent => Form (Row, Column), Content => Text, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Background_Text;

   function New_Button (Row : Positive; Column : Positive; Text : String; Break_Before : Boolean := False) return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Button);
   begin -- New_Button
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Switch := new Gnoga.Gui.Element.Common.Button_Type;
      Widget.Switch.Create (Parent => Form (Row, Column), Content => Text, ID => ID.Value'Image);
      Widget.Switch.On_Click_Handler (Handler => On_Click'Access);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Button;

   function New_Check_Box
      (Row : Positive; Column : Positive; Label : String; Break_Before : Boolean := False; Active : Boolean := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Check_Box);
   begin -- New_Check_Box
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Check := new Gnoga.Gui.Element.Form.Check_Box_Type;
      Widget.Check.Create (Form => Form (Row, Column), ID => ID.Value'Image);
      Widget.Check.Checked (Value => Active);
      Widget.Check_Label := new Gnoga.Gui.Element.Form.Label_Type;
      Widget.Check_Label.Create (Form => Form (Row, Column), Label_For => Widget.Check.all, Content => Label, Auto_Place => False);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Check_Box;

   function New_Graphic_Area
      (Row : Positive; Column : Positive; Width : Positive; Height : Positive; Break_Before : Boolean := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Graphic_Area);
   begin -- New_Graphic_Area
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Canvas := new Gnoga.Gui.Element.Canvas.Canvas_Type;
      Widget.Canvas.Create (Parent => Form (Row, Column), Width => Width, Height => Height, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Graphic_Area;

   use Ada.Strings.Unbounded;

   Next_Button : Positive := 1;

   function New_Radio_Buttons (Row          : Positive;
                               Column       : Positive;
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
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Radio := new Radio_List (Label'Range);

      All_Buttons : for I in Label'Range loop
         Widget.Radio (I).Button.Create
            (Form => Form (Row, Column), Checked => I = Label'First, Name => Name, ID => I'Image & 'R' & ID.Value'Image);
         Widget.Radio (I).Label.Create (Form       => Form (Row, Column),
                                        Label_For  => Widget.Radio (I).Button,
                                        Content    => To_String (Label (I) ),
                                        Auto_Place => False);

         if I < Label'Last and Orientation = Vertical then
            Form (Row, Column).New_Line;
         end if;
      end loop All_Buttons;

      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Radio_Buttons;

   function New_Selection_List (Row             : Positive;
                                Column          : Positive;
                                Text            : Text_List;
                                Break_Before    : Boolean  := False;
                                Height          : Positive := 1;
                                Multiple_Select : Boolean  := False)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Selection_List);
   begin -- New_Selection_List
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Selector := new Gnoga.Gui.Element.Form.Selection_Type;
      Widget.Selector.Create
         (Form => Form (Row, Column), Multiple_Select => Multiple_Select, Visible_Lines => Height, ID => ID.Value'Image);
      Widget.Selector.On_Click_Handler (Handler => On_Click'Access);
      Widget.Multi := Multiple_Select;
      Widget_List.Append (New_Item => Widget);

      Add_Options : for I in Text'Range loop
         Widget.Selector.Add_Option (Value => To_String (Text (I) ), Text => To_String (Text (I) ) );
      end loop Add_Options;

      return ID;
   end New_Selection_List;

   function New_Text_Area (Row          : Positive;
                           Column       : Positive;
                           Text         : String   := "";
                           Break_Before : Boolean  := False;
                           Width        : Positive := 20;
                           Height       : Positive := 2)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Text_Area);
   begin -- New_Text_Area
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Area := new Gnoga.Gui.Element.Form.Text_Area_Type;
      Widget.Area.Create (Form => Form (Row, Column), Columns => Width, Rows => Height, Value => Text, ID => ID.Value'Image);
      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Text_Area;

   function New_Text_Box (Row          : Positive;
                          Column       : Positive;
                          Text         : String;
                          Break_Before : Boolean  := False;
                          Label        : String   := "";
                          Placeholder  : String   := "";
                          Width        : Positive := 20)
   return Widget_ID is
      ID : constant Widget_ID := (Value => Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Text_Box);
   begin -- New_Text_Box
      Break (Desired => Break_Before, Row => Row, Column => Column);
      Widget.Box := new Gnoga.Gui.Element.Form.Text_Type;
      Widget.Box.Create (Form => Form (Row, Column), Size => Width, Value => Text, ID => ID.Value'Image);
      Widget.Box_Label := new Gnoga.Gui.Element.Form.Label_Type;
      Widget.Box_Label.Create (Form => Form (Row, Column), Label_For => Widget.Box.all, Content => Label);

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

   function Next_Event (Timeout : Duration := Duration'Last) return Next_Result_Info is
      Event : Event_Info;
   begin -- Next_Event
      select
         Event_Queue.Dequeue (Element => Event);

         return (Timed_Out => False, Event => Event);
      or
         delay Timeout;

         return (Timed_Out => True);
      end select;
   end Next_Event;

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
         Widget.Background.Text (Value => Text);
      when Button =>
         Widget.Switch.Text (Value => Text);
      when Text_Area =>
         Widget.Area.Value (Value => Text);
      when Text_Box =>
         Widget.Box.Value (Value => Text);
      when others =>
         raise Program_Error;
      end case;
   end Set_Text;

   function Multiple_Select (ID : Widget_ID) return Boolean is (Widget_List (ID.Value).Element.Multi);

   function Text (ID : Widget_ID) return String is
      Widget : constant Widget_Info := Widget_List (ID.Value);
   begin -- Text
      case Widget.Kind is
      when Background_Text =>
         return Widget.Background.Text;
      when Button =>
         return Widget.Switch.Text;
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

   function To_Color (Color : Color_ID) return Color_Info is
      G_Color : constant Gnoga.Types.RGBA_Type :=
         Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Color_Enumeration'Val (Color_ID'Pos (Color) ) );
   begin -- To_Color
      return (Red => RGB_Value (G_Color.Red), Green => RGB_Value (G_Color.Green), Blue => RGB_Value (G_Color.Blue) );
   end To_Color;

   function To_ID (Color : Color_Info) return Color_ID is
      G_Color : constant Gnoga.Types.RGBA_Type := (Red   => Gnoga.Types.Color_Type (Color.Red),
                                                   Green => Gnoga.Types.Color_Type (Color.Green),
                                                   Blue  => Gnoga.Types.Color_Type (Color.Blue),
                                                   Alpha => 1.0);
   begin -- To_ID
      return Color_ID'Val (Gnoga.Types.Colors.Color_Enumeration'Pos (Gnoga.Types.Colors.To_Color_Enumeration (G_Color) ) );
   end To_ID;

   procedure Set_Pixel (ID : in Widget_ID; X : in Integer; Y : in Integer; Color : in Color_Info := To_Color (Black) ) is
      G_Color : constant Gnoga.Types.Pixel_Type := (Red   => Gnoga.Types.Color_Type (Color.Red),
                                                    Green => Gnoga.Types.Color_Type (Color.Green),
                                                    Blue  => Gnoga.Types.Color_Type (Color.Blue),
                                                    Alpha => 255);

      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Set_Pixel
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Pixel (X => X, Y => Y, Color => G_Color);
   end Set_Pixel;

   function Pixel (ID : Widget_ID; X : Integer; Y : Integer) return Color_Info is
      Widget : constant Widget_Info := Widget_List (ID.Value);

      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      G_Color : Gnoga.Types.Pixel_Type;
   begin -- Pixel_Type
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      G_Color := Context.Pixel (X, Y);

      return (Red => RGB_Value (G_Color.Red), Green => RGB_Value (G_Color.Green), Blue => RGB_Value (G_Color.Blue) );
   end Pixel;

   procedure Draw_Line (ID     : in Widget_ID;
                        From_X : in Integer;
                        From_Y : in Integer;
                        To_X   : in Integer;
                        To_Y   : in Integer;
                        Color  : in Color_Info := To_Color (Black) )
   is
      G_Color : constant Gnoga.Types.RGBA_Type := (Red   => Gnoga.Types.Color_Type (Color.Red),
                                                   Green => Gnoga.Types.Color_Type (Color.Green),
                                                   Blue  => Gnoga.Types.Color_Type (Color.Blue),
                                                   Alpha => 1.0);

      Widget  : Widget_Info := Widget_List (ID.Value);
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Draw_Line
      Context.Get_Drawing_Context_2D (Canvas => Widget.Canvas.all);
      Context.Stroke_Color (Value => G_Color);
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
      Context.Begin_Path;
      Context.Rectangle (Rectangle => (X      => Integer'Min (From_X, To_X),
                                       Y      => Integer'Min (From_Y, To_Y),
                                       Width  => abs (From_X - To_X) + 1,
                                       Height => abs (From_Y - To_Y) + 1) );

      if not Fill_Color.None then
         Convert_Fill : declare
            F_Color : constant Gnoga.Types.RGBA_Type := (Red   => Gnoga.Types.Color_Type (Fill_Color.Color.Red),
                                                         Green => Gnoga.Types.Color_Type (Fill_Color.Color.Green),
                                                         Blue  => Gnoga.Types.Color_Type (Fill_Color.Color.Blue),
                                                         Alpha => 1.0);
         begin -- Convert_Fill
            Context.Fill_Color (Value => F_Color);
            Context.Fill;
         end Convert_Fill;
      end if;

      if not Line_Color.None then
         Convert_Line : declare
            L_Color : constant Gnoga.Types.RGBA_Type := (Red   => Gnoga.Types.Color_Type (Line_Color.Color.Red),
                                                         Green => Gnoga.Types.Color_Type (Line_Color.Color.Green),
                                                         Blue  => Gnoga.Types.Color_Type (Line_Color.Color.Blue),
                                                         Alpha => 1.0);
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
            F_Color : constant Gnoga.Types.RGBA_Type := (Red   => Gnoga.Types.Color_Type (Fill_Color.Color.Red),
                                                         Green => Gnoga.Types.Color_Type (Fill_Color.Color.Green),
                                                         Blue  => Gnoga.Types.Color_Type (Fill_Color.Color.Blue),
                                                         Alpha => 1.0);
         begin -- Convert_Fill
            Context.Close_Path;
            Context.Fill_Color (Value => F_Color);
            Context.Fill;
         end Convert_Fill;
      end if;

      if not Line_Color.None then
         Convert_Line : declare
            L_Color : constant Gnoga.Types.RGBA_Type := (Red   => Gnoga.Types.Color_Type (Line_Color.Color.Red),
                                                         Green => Gnoga.Types.Color_Type (Line_Color.Color.Green),
                                                         Blue  => Gnoga.Types.Color_Type (Line_Color.Color.Blue),
                                                         Alpha => 1.0);
         begin -- Convert_Line
            Context.Stroke_Color (Value => L_Color);
            Context.Stroke;
         end Convert_Line;
      end if;
   end Draw_Arc;

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

      return Widget.Radio'Last + 1;
   end Active;

   function Length (ID : Widget_ID) return Natural is
      Widget : Widget_Info := Widget_List (ID.Value);
   begin -- Length
      return Widget.Selector.Length;
   end Length;

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
      View : Gnoga.Gui.Element.Form.Form_Type;
   begin -- End_GUI
      Grid.Remove;
      View.Create (Parent => Window);
      View.Put_Line (Message => Window.Document.Title & " ended");
      Gnoga.Application.Singleton.End_Application;
      Setup := False;
   end End_GUI;

   task GUI_Thread;

   Gnoga_Running : Boolean := False with Atomic;

   task body GUI_Thread is
      -- Empty
   begin -- GUI_Thread
      Gnoga.Application.Open_URL;
      Gnoga.Application.Singleton.Initialize (Main_Window => Window);
      Gnoga.Application.HTML_On_Close (HTML => "Application ended");
      Gnoga_Running := True;
      Gnoga.Application.Singleton.Message_Loop;
   end GUI_Thread;
begin -- Ada_Gui
   Wait : loop -- Delay environment task until GUI_Thread has initialized Window
      exit Wait when Gnoga_Running;

      delay 0.1;
   end loop Wait;
end Ada_Gui;

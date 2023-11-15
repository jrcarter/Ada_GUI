-- A program to show all of the Ada_GUI widgets
-- An Ada_GUI demo program
--
-- Copyright (C) 2023 by PragmAda Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
--
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada_GUI;

procedure Show_All is
   Audio      : Ada_GUI.Widget_ID;
   Background : Ada_GUI.Widget_ID;
   Visible    : Ada_GUI.Widget_ID;
   Quit       : Ada_GUI.Widget_ID;
   Check      : Ada_GUI.Widget_ID;
   Graphic    : Ada_GUI.Widget_ID;
   Change     : Ada_GUI.Widget_ID;
   Password   : Ada_GUI.Widget_ID;
   Radio      : Ada_GUI.Widget_ID;
   Selection  : Ada_GUI.Widget_ID;
   Area       : Ada_GUI.Widget_ID;
   Box        : Ada_GUI.Widget_ID;
   Pressed    : Ada_GUI.Widget_ID;
   Sel_File   : Ada_GUI.Widget_ID;
   Sel_Yes_No : Ada_GUI.Widget_ID;
   Selected   : Ada_GUI.Widget_ID;
   Progress   : Ada_GUI.Widget_ID;
   One        : Ada_GUI.Widget_ID;
   Two        : Ada_GUI.Widget_ID;
   Three      : Ada_GUI.Widget_ID;
   Hider      : Ada_GUI.Widget_ID;
   Curve      : Ada_GUI.Widget_ID;
   Image      : Ada_GUI.Widget_ID;
   Event      : Ada_GUI.Next_Result_Info;
   File_Info  : Ada_GUI.Dialogs.File_Result_Info;
   Shrink     : Boolean := True;
   Value      : Natural := 0;
   Inc        : Boolean := True;

   use Ada.Strings.Unbounded;
   use type Ada_GUI.Event_Kind_ID;
   use type Ada_GUI.Widget_ID;
begin -- Show_All
   Ada_GUI.Set_Up (Title => "Show All Ada_GUI Widgets");
   Ada_GUI.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Light_Blue) );
   Audio := Ada_GUI.New_Audio_Player (Source => "glass.ogg");
   Audio.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Audio.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Background := Ada_GUI.New_Background_Text (Text => "Background_Text can <br><font color=" & '"' & "Green" & '"' &
                                                      ">have</font> <b>at</b><i>tri</i><u>butes</u> &euro; " &
                                                      Ada.Characters.Latin_1.LC_E_Acute,
                                              Break_Before => True);
   Background.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Background.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Visible := Ada_GUI.New_Check_Box (Label => "Visible &euro;", Active => True);
   Quit := Ada_GUI.New_Button (Text => "Quit &euro;", Break_Before => True);
   Quit.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Quit.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Check := Ada_GUI.New_Check_Box (Label => "<b>Check</b>_Box:", Break_Before => True);
   Check.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Check.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic := Ada_GUI.New_Graphic_Area (Width => 100, Height => 100, Break_Before => True);
   Graphic.Draw_Rectangle (From_X     =>  0,
                           From_Y     =>  0,
                           To_X       => 99,
                           To_Y       => 99,
                           Line_Color => (None => True),
                           Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.Yellow) ) );
   Graphic.Draw_Rectangle (From_X     => 10,
                           From_Y     => 10,
                           To_X       => 90,
                           To_Y       => 90,
                           Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
   Graphic.Draw_Line (From_X => 0, From_Y => 0, To_X => 100, To_Y => 100, Width => 2, Style => Ada_GUI.Dashed);
   Graphic.Draw_Line (From_X => 0, From_Y => 100, To_X => 100, To_Y => 0, Width => 2, Color => Ada_GUI.To_Color (Ada_GUI.Green) );
   Graphic.Set_Pixel (X => 75, Y => 25, Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic.Set_Pixel (X => 76, Y => 25, Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic.Set_Pixel (X => 75, Y => 26, Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic.Set_Pixel (X => 76, Y => 26, Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic.Draw_Text (X          => 10,
                      Y          => 75,
                      Text       => "Texty",
                      Line_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.Yellow) ),
                      Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.Blue) ) );
   Graphic.Draw_Line (From_X => 0, From_Y => 75, To_X => 100, To_Y => 75, Style => Ada_GUI.Dotted);
   Change := Ada_GUI.New_Button (Text => "Change size");
   Password := Ada_GUI.New_Password_Box (Break_Before => True, Label => "<i>Password</i>_Box:");
   Password.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Password.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Radio := Ada_GUI.New_Radio_Buttons (Label => (To_Unbounded_String ("Yes"),
                                                 To_Unbounded_String ("No"),
                                                 To_Unbounded_String ("Maybe") ),
                                       Break_Before => True);
   Radio.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Radio.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Selection := Ada_GUI.New_Selection_List (Text => (To_Unbounded_String ("Yes"),
                                                     To_Unbounded_String ("No"),
                                                     To_Unbounded_String ("Maybe") ),
                                            Break_Before => True);
   Selection.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Selection.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Selection.Set_Text_Alignment (Alignment => Ada_GUI.Right);
   Area := Ada_GUI.New_Text_Area (Text => "Some text" & Ada.Characters.Latin_1.LF & "Another line", Break_Before => True);
   Area.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Area.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Box := Ada_GUI.New_Text_Box (Break_Before => True, Label => "Text_Box:", Placeholder => "Placeholder");
   Box.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Box.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Pressed := Ada_GUI.New_Text_Area (Break_Before => True);
   Pressed.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Light_Green) );
   Pressed.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Dark_Blue) );
   Sel_File := Ada_GUI.New_Button (Text => "Select File", Break_Before => True);
   Sel_Yes_No := Ada_GUI.New_Button (Text => "Yes/No");
   Selected := Ada_GUI.New_Text_Box (Break_Before => True, Label => "Selected:");
   Selected.Set_Read_Only;
   Progress := Ada_GUI.New_Progress_Bar (Break_Before => True);
   One := Ada_GUI.New_Button (Text => "One", Break_Before => True);
   Two := Ada_GUI.New_Button (Text => "Two");
   Three := Ada_GUI.New_Button (Text => "Three");
   Hider := Ada_GUI.New_Radio_Buttons (Label => (To_Unbounded_String ("Normal"),
                                                 To_Unbounded_String ("Invisible"),
                                                 To_Unbounded_String ("Hidden") ),
                                       Break_Before => True,
                                       Orientation  => Ada_GUI.Horizontal);
   Curve := Ada_GUI.New_Graphic_Area (Width => 100, Height => 100, Break_Before => True);
   Curve.Draw_Rectangle (From_X     =>  0,
                         From_Y     =>  0,
                         To_X       => 99,
                         To_Y       => 99,
                         Line_Color => (None => True),
                         Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.Light_Blue) ) );

   Plot_Graph : declare
      Plot : constant Ada_GUI.Plotting.Plot_Info :=
         Ada_GUI.Plotting.New_Plot (ID => Curve, X_Min => -2.5, X_Max => 2.5, Y_Min => -0.5, Y_Max => 4.5);

      Prev_X : Float := -2.5;
      Prev_Y : Float := Prev_X ** 2;
      X      : Float;
      Y      : Float;
   begin -- Plot_Graph
      Plot.Draw_Axes (Interval => 2.0, Length => 5);

      Draw_Lines : loop
         exit Draw_Lines when Prev_X > 2.5;

         X := Prev_X + 0.5;
         Y := X ** 2;
         Plot.Draw_Line (From_X => Prev_X,
                         From_Y => Prev_Y,
                         To_X   => X,
                         To_Y   => Y,
                         Color  => Ada_GUI.To_Color (Ada_GUI.Red),
                         Style  => Ada_GUI.Dot_Dash);
         Prev_X := X;
         Prev_Y := Y;
      end loop Draw_Lines;
   end Plot_Graph;

   Image := Ada_GUI.New_Image (Source => "rgb.jpg", Description => "Your image here", Break_Before => True);

   Wait_To_Quit : loop
      Event := Ada_GUI.Next_Event (Timeout => 0.1);

      if Event.Timed_Out then
         Value := Value + (if Inc then 1 else -1);
         Progress.Set_Value (Value => Value);

         if (Inc and Value = 100) or (not Inc and Value = 0) then
            Inc := not Inc;
         end if;
      else
         exit Wait_To_Quit when Event.Event.Kind = Ada_GUI.Window_Closed;

         if Event.Event.Kind = Ada_GUI.Key_Press then
            Handle_Invalid : declare
               Text : constant String := Pressed.Text;
            begin -- Handle_Invalid
               Pressed.Set_Text
                  (Text =>  Text (1 .. Text'Last - 1) & Character'Val (Wide_Character'Pos (Event.Event.Key.Key_Char) ) & " &euro;");
            exception -- Handle_Invalid
            when others => -- Key_Char not in Character
               Pressed.Set_Text (Text => Pressed.Text & " ?");
            end Handle_Invalid;
         else
            exit Wait_To_Quit when Event.Event.ID = Quit;

            if Event.Event.ID = Visible then
               Background.Set_Visibility (Visible => Visible.Active);

               if Visible.Active then
                  Background.Set_Text (Text => "New contents can also <b>have</b> attributes &euro;");
                  Visible.Set_Label (Text => "<b>Visi</b>ble &euro;");
                  Radio.Set_Label (Index => 3, Text => "<i>Don't</i> know");
                  Quit.Set_Text (Text => "<b>Depart</b> &euro;");
               end if;
            elsif Event.Event.ID = Sel_File then
               File_Info := Ada_GUI.Dialogs.Selected_File;
               Selected.Set_Text (Text => (if File_Info.Picked then To_String (File_Info.Value) else "") );
            elsif Event.Event.ID = Sel_Yes_No then
               Selected.Set_Text
                  (Text => Ada_GUI.Dialogs.Yes_Or_No (Title => "Yes/No Dialog", Text => "Do you want 'Yes' to appear?") );
            elsif Event.Event.ID = Hider then
               case Positive'(Hider.Active) is
               when 1 => -- Normal
                  Two.Set_Hidden (Hidden => False);
                  Two.Set_Visibility (Visible => True);
               when 2 => --Invisible
                  Two.Set_Hidden (Hidden => False);
                  Two.Set_Visibility (Visible => False);
               when 3 => -- Hidden
                  Two.Set_Hidden (Hidden => True);
                  Two.Set_Visibility (Visible => False);
               when others =>
                  raise Program_Error with "Invalid Hider button index";
               end case;
            elsif Event.Event.ID = Change then
               if Shrink then
                  Graphic.Set_Size (Width => Graphic.Width / 2, Height => Graphic.Height / 2);
               else
                  Graphic.Set_Size (Width => 2 * Graphic.Width, Height => 2 * Graphic.Height);
               end if;

               Shrink := not Shrink;
            else
               null;
            end if;
         end if;
      end if;
   end loop Wait_To_Quit;

   Ada_GUI.End_GUI;
end Show_All;

-- A program to show all of the Ada_GUI widgets
-- An Ada_GUI demo program
--
-- Copyright (C) 2022 by PragmAda Software Engineering
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
   Password   : Ada_GUI.Widget_ID;
   Radio      : Ada_GUI.Widget_ID;
   Selection  : Ada_GUI.Widget_ID;
   Area       : Ada_GUI.Widget_ID;
   Box        : Ada_GUI.Widget_ID;
   Pressed    : Ada_GUI.Widget_ID;
   Sel_File   : Ada_GUI.Widget_ID;
   Sel_Yes_No : Ada_GUI.Widget_ID;
   Selected   : Ada_GUI.Widget_ID;
   Event      : Ada_GUI.Next_Result_Info;
   File_Info  : Ada_GUI.Dialogs.File_Result_Info;

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
                                                      ">have</font> <b>at</b><i>tri</i><u>butes</u> &euro;",
                                              Break_Before => True);
   Background.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Background.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Visible := Ada_GUI.New_Check_Box (Label => "Visible &euro;", Active => True);
   Quit := Ada_GUI.New_Button (Text => "Quit", Break_Before => True);
   Quit.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Quit.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Check := Ada_GUI.New_Check_Box (Label => "<b>Check</b>_Box:", Break_Before => True);
   Check.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Check.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic := Ada_GUI.New_Graphic_Area (Width => 100, Height => 100, Break_Before => True);
   Graphic.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Yellow) );
   Graphic.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Graphic.Draw_Rectangle (From_X     => 10,
                           From_Y     => 10,
                           To_X       => 90,
                           To_Y       => 90,
                           Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
   Graphic.Draw_Line (From_X => 0, From_Y => 0, To_X => 100, To_Y => 100, Width => 2);
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
   Graphic.Draw_Line (From_X => 0, From_Y => 75, To_X => 100, To_Y => 75);
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
   Selection.Set_Text_Aligbnment (Alignment => Ada_GUI.Right);
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

   Wait_To_Quit : loop
      Event := Ada_GUI.Next_Event (Timeout => 1.0);

      if not Event.Timed_Out then
         exit Wait_To_Quit when Event.Event.Kind = Ada_GUI.Window_Closed;

         if Event.Event.Kind = Ada_GUI.Key_Press then
            Handle_Invalid : begin
               Pressed.Set_Text (Text => Pressed.Text & ' ' & Character'Val (Wide_Character'Pos (Event.Event.Key.Key_Char) ) );
            exception -- Handle_Invalid
            when others => -- Key_Char not in Character
               Pressed.Set_Text (Text => Pressed.Text & " ?");
            end Handle_Invalid;
         else
            exit Wait_To_Quit when Event.Event.ID = Quit;

            if Event.Event.ID = Visible then
               Background.Set_Visibility (Visible => Visible.Active);

               if Visible.Active then
                  Background.Set_Text (Text => "New contents cannot <b>have</b> attributes &euro;");
               end if;
            elsif Event.Event.ID = Sel_File then
               File_Info := Ada_GUI.Dialogs.Selected_File;
               Selected.Set_Text (Text => (if File_Info.Picked then To_String (File_Info.Value) else "") );
            elsif Event.Event.ID = Sel_Yes_No then
               Selected.Set_Text
                  (Text => Ada_GUI.Dialogs.Yes_Or_No (Title => "Yes/No Dialog", Text => "Do you want 'Yes' to appear?") );
            else
               null;
            end if;
         end if;
      end if;
   end loop Wait_To_Quit;

   Ada_GUI.End_GUI;
end Show_All;

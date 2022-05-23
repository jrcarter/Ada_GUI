-- A program for generating Luhn checksums with an Ada_GUI UI
-- An Ada_GUI demo program
--
-- Copyright (C) 2022 by PragmAda Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
--
with Ada.Exceptions;

with Ada_GUI;
with PragmARC.Luhn_Generation;

procedure Luhn_Gen is
   Input    : Ada_GUI.Widget_ID;
   Err      : Ada_GUI.Widget_ID;
   Checksum : Ada_GUI.Widget_ID;
   Gen      : Ada_GUI.Widget_ID;
   Quit     : Ada_GUI.Widget_ID;

   procedure Generate;

   Err_Msg : constant String := "Enter some digits";

   procedure Generate is
      -- Empty
   begin -- Generate
      Err.Set_Visibility (Visible => False);
      Checksum.Set_Text (Text => Integer'Image (PragmARC.Luhn_Generation.Checksum (Input.Text) ) );
   exception -- Generate
   when E : others =>
      Ada_GUI.Log (Message => "Generate: " & Ada.Exceptions.Exception_Information (E) );
      Err.Set_Visibility (Visible => True);
      Checksum.Set_Text (Text => "");
   end Generate;

   Event : Ada_GUI.Next_Result_Info;

   use type Ada_GUI.Event_Kind_ID;
   use type Ada_GUI.Widget_ID;
begin -- Luhn_Gen
   Ada_GUI.Set_Up (Title => "Luhn Checksum Generator");

   Input := Ada_GUI.New_Text_Box (Label => "Input :", Placeholder => Err_Msg);
   Err := Ada_GUI.New_Background_Text (Text => Err_Msg, Break_Before => True);
   Err.Set_Foreground_Color (Color => Ada_GUI.To_Color (Ada_GUI.Red) );
   Err.Set_Visibility (Visible => False);
   Checksum := Ada_GUI.New_Text_Box (Label =>  "Checksum :", Break_Before => True);
   Gen := Ada_GUI.New_Button (Text => "Generate", Break_Before => True);
   Quit := Ada_GUI.New_Button (Text => "Quit", Break_Before => True);

   All_Events : loop
      Event := Ada_GUI.Next_Event;

      if not Event.Timed_Out then
         exit All_Events when Event.Event.Kind = Ada_GUI.Window_Closed;

         if Event.Event.Kind = Ada_GUI.Left_Click then
            if Event.Event.ID = Gen then
               Generate;
            end if;

            exit All_Events when Event.Event.ID = Quit;
         end if;
      end if;
   end loop All_Events;

   Ada_GUI.End_GUI;
exception -- Luhn_Gen
when E : others =>
   Ada_GUI.Log (Message => Ada.Exceptions.Exception_Information (E) );
end Luhn_Gen;

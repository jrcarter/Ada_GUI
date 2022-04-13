-- A program for generating Luhn checksums with an Ada_GUI UI
-- An Ada_GUI demo program
--
-- Copyright (C) 2022 by PragmAda Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
--
with Ada.Exceptions;
with Ada.Text_IO;

with Ada_GUI;

procedure Luhn_Gen is
   Input    : Ada_GUI.Widget_ID;
   Err      : Ada_GUI.Widget_ID;
   Checksum : Ada_GUI.Widget_ID;
   Gen      : Ada_GUI.Widget_ID;
   Quit     : Ada_GUI.Widget_ID;

   procedure Generate;

   Err_Msg : constant String := "Enter some digits";

   procedure Generate is
      subtype Digit is Character range '0' .. '9';

      function Reversed (Value : String) return String;
      -- Reverses Value.

      function Squeezed (Value : String) return String;
      -- Keeps the Digits of Value and discards any other characters.

      function D2N (D : Digit) return Natural is (Character'Pos (D) - Character'Pos ('0') );

      function Reversed (Value : String) return String is
         Result : String (Value'Range);
         Last   : Natural := Value'Last;
      begin -- Reversed
         if Value = "" then
            return "";
         end if;

         Swap : for First in Value'First .. Value'First + (Value'Length - 1) / 2 loop
            Result (First) := Value (Last);
            Result (Last)  := Value (First);
            Last := Last - 1;
         end loop Swap;

         return Result;
      end Reversed;

      function Squeezed (Value : String) return String is
         Result : String (1 .. Value'Length);
         Last   : Natural := 0;
      begin -- Squeezed
         All_Chars : for I in Value'Range loop
            if Value (I) in Digit then
               Last := Last + 1;
               Result (Last) := Value (I);
            end if;
         end loop All_Chars;

         return Result (1 .. Last);
      end Squeezed;

      Forward : constant String := Input.Text;
      Value   : constant String := Squeezed (Reversed (Forward) );

      Sum : Natural := 0;
      D   : Natural;
   begin -- Generate
      Err.Set_Text (Text => "");
      Err.Set_Visibility (Visible => False);
      Checksum.Set_Text (Text => "");

      if Value'Length = 0 then
         Err.Set_Visibility (Visible => True);
         Err.Set_Text (Text => Err_Msg);

         return;
      end if;

      All_Digits : for I in Value'Range loop
         D := D2N (Value (I) );

         if I rem 2 = 1 then
            D := 2 * D;

            if D > 9 then
               D := D - 9;
            end if;
         end if;

         Sum := Sum + D;
      end loop All_Digits;

      Checksum.Set_Text (Text => Integer'Image ( (9 * Sum) rem 10) );
   exception -- Generate
   when E : others =>
      Ada.Text_IO.Put_Line (Item => "Generate: " & Ada.Exceptions.Exception_Information (E) );
   end Generate;

   Event : Ada_GUI.Next_Result_Info;

   use type Ada_GUI.Event_Kind_ID;
   use type Ada_GUI.Widget_ID;
begin -- Luhn_Gen
   Ada_GUI.Set_Up (Title => "Luhn Checksum Generator");

   Input := Ada_GUI.New_Text_Box (Label => "Input :", Placeholder => Err_Msg);
   Err := Ada_GUI.New_Background_Text (Break_Before => True);
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
   Ada.Text_IO.Put_Line (Item => Ada.Exceptions.Exception_Information (E) );
end Luhn_Gen;

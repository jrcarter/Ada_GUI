-- Ada_GUI version of Random_Int
--
-- Copyright (C) 2021 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause
--
-- The (application-specific) user interface

with Ada.Strings.Fixed;
with Ada_GUI;

package body Random_Int.UI is
   Min_Entry : Ada_GUI.Widget_ID;
   Max_Entry : Ada_GUI.Widget_ID;
   Result    : Ada_GUI.Widget_ID;
   Generator : Ada_GUI.Widget_ID;
   Quitter   : Ada_GUI.Widget_ID;

   Finished : Boolean := False with Atomic;

   function Ended return Boolean is (Finished);

   task Event_Handler is
      entry Start; -- Delay the task until the GUI is set up

      entry Get (Event : out Event_ID);
   end Event_Handler;

   function Next_Event return Event_ID is
      Result : Event_ID;
   begin -- Next_Event
      Event_Handler.Get (Event => Result);

      return Result;
   end Next_Event;

   function Min_Text return String Is (Min_Entry.Text);

   function Max_Text return String Is (Max_Entry.Text);

   function Image (Value : Integer) return String is (Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Both) );

   procedure Set_Min (Value : in Integer) is
      -- Empty
   begin -- Set_Min
      Min_Entry.Set_Text (Text => Image (Value) );
   end Set_Min;

   procedure Set_Max (Value : in Integer) is
      -- Empty
   begin -- Set_Max
      Max_Entry.Set_Text (Text => Image (Value) );
   end Set_Max;

   procedure Min_Error is
      -- Empty
   begin -- Min_Error
      Min_Entry.Set_Text (Text => "");
   end Min_Error;

   procedure Max_Error is
      -- Empty
   begin -- Max_Error
      Max_Entry.Set_Text (Text => "");
   end Max_Error;

   procedure Show_Result (Value : in Integer) is
      -- Empty
   begin -- Show_Result
      Result.Set_Text (Text => Image (Value) );
   end Show_Result;

   task body Event_Handler is
      Event : Ada_GUI.Next_Result_Info;
      ID    : Ada_GUI.Widget_ID;

      use type Ada_GUI.Event_Kind_ID;
      use type Ada_GUI.Widget_ID;
   begin -- Event_Handler
      accept Start;

      Forever : loop
         Event := Ada_GUI.Next_Event (Timeout => 1.0);

         if not Event.Timed_Out then
            if Event.Event.Kind = Ada_GUI.Window_Closed then
               accept Get (Event : out Event_ID) do
                  Event := Quit;
               end Get;

               Finished := True;

               exit Forever;
            elsif Event.Event.Kind = Ada_GUI.Left_Click then
               ID := Event.Event.ID;

               if ID = Generator or ID = Quitter then
                  accept Get (Event : out Event_ID) do
                     if ID = Generator then
                        Event := Generate;
                     else
                        Event := Quit;
                        Finished := True;
                     end if;
                  end Get;
               end if;

               exit Forever when ID = Quitter;
            end if;
         end if;
      end loop Forever;

      Ada_GUI.End_GUI;
   end Event_Handler;

   Placeholder : constant String := "Enter an integer";
begin -- Random_Int.UI
   Ada_GUI.Set_Up (Grid => (1 => (1 .. 2 => (Kind => Ada_GUI.Area, Alignment => Ada_GUI.Right) ) ), Title => "Random Integers");
   Min_Entry := Ada_GUI.New_Text_Box (Label => "Minimum value", Placeholder => Placeholder);
   Min_Entry.Set_Text_Alignment (Alignment => Ada_GUI.Right);
   Max_Entry := Ada_GUI.New_Text_Box (Break_Before => True, Label => "Maximum value", Placeholder => Placeholder);
   Max_Entry.Set_Text_Alignment (Alignment => Ada_GUI.Right);
   Result := Ada_GUI.New_Text_Box (Break_Before => True);
   Result.Set_Text_Alignment (Alignment => Ada_GUI.Right);
   Generator := Ada_GUI.New_Button (Text => "Generate");
   Quitter   := Ada_GUI.New_Button (Text => "Quit", Break_Before => True);
   Event_Handler.Start;
end Random_Int.UI;

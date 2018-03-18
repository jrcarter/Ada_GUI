-- Ada_GUI version of Random_Int
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause
--
-- The (application-specific) user interface

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
      entry Get (Event : out Event_ID);
   end Event_Handler;

   function Next_Event return Event_ID is
      Result : Event_ID;
   begin -- Next_Event
      Event_Handler.Get (Event => Result);

      return Result;
   end Next_Event;

   function Min_Text return String Is (Ada_GUI.Text (Min_Entry) );

   function Max_Text return String Is (Ada_GUI.Text (Max_Entry) );

   procedure Set_Min (Value : in Integer) is
      -- Empty
   begin -- Set_Min
      Ada_GUI.Set_Text (ID => Min_Entry, Text => Value'Image);
   end Set_Min;

   procedure Set_Max (Value : in Integer) is
      -- Empty
   begin -- Set_Max
      Ada_GUI.Set_Text (ID => Max_Entry, Text => Value'Image);
   end Set_Max;

   Placeholder : constant String := "Enter an integer";

   procedure Min_Error is
      -- Empty
   begin -- Min_Error
      Ada_GUI.Set_Text (ID => Min_Entry, Text => Placeholder);
   end Min_Error;

   procedure Max_Error is
      -- Empty
   begin -- Max_Error
      Ada_GUI.Set_Text (ID => Max_Entry, Text => Placeholder);
   end Max_Error;

   procedure Show_Result (Value : in Integer) is
      -- Empty
   begin -- Show_Result
      Ada_GUI.Set_Text (ID => Result, Text => Value'Image);
   end Show_Result;

   task body Event_Handler is
      Event : Ada_GUI.Next_Result_Info;
      ID    : Ada_GUI.Widget_ID;

      use type Ada_GUI.Widget_Kind_ID;
      use type Ada_GUI.Widget_ID;
      use type Ada_GUI.Event_Kind_ID;
   begin -- Event_Handler
      Forever : loop
         Event := Ada_GUI.Next_Event;

         If not Event.Timed_Out and then
            Event.Event.Widget_Kind = Ada_GUI.Button and then
            Event.Event.Event_Kind = Ada_GUI.Left_Click
         then
            ID := Event.Event.ID;

            if ID = Generator or ID = Quitter then
               accept Get (Event : out Event_ID) do
                  if ID = Generator then
                     Event := Generate;
                  else
                     Event := Quit;
                  end if;
               end Get;
            end if;

            if ID = Quitter then
               Ada_GUI.End_GUI;
               Finished := True;

               exit Forever;
            end if;
         end if;
      end loop Forever;
   end Event_Handler;
begin -- Random_Int.UI
   Ada_GUI.Set_Title (Title => "Random Integers");
   Min_Entry := Ada_GUI.New_Item (Kind => Ada_GUI.Text_Box, Text => Placeholder, Label => "Minimum value");
   Max_Entry := Ada_GUI.New_Item (Kind => Ada_GUI.Text_Box, Text => Placeholder, Break_Before => True, Label => "Maximum value");
   Result    := Ada_GUI.New_Item (Kind => Ada_GUI.Text_Box, Text => "", Break_Before => True);
   Generator := Ada_GUI.New_Item (Kind => Ada_GUI.Button, Text => "Generate");
   Quitter   := Ada_GUI.New_Item (Kind => Ada_GUI.Button, Text => "Quit", Break_Before => True);
end Random_Int.UI;

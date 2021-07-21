-- Ada_GUI version of Random_Int
--
-- Copyright (C) 2021 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause
--
-- The main-program procedure

with Ada.Numerics.Discrete_Random;

with Random_Int.UI;

procedure Random_Int.Program is
   procedure Generate_One is
      Low   : Integer;
      High  : Integer;
      Value : Integer;
   begin -- Generate_One
      Min_Error : begin
         Low := Integer'Value (UI.Min_Text);
      exception -- Min_Error
      when others =>
         UI.Min_Error;

         return;
      end Min_Error;

      Max_Error : begin
         High := Integer'Value (UI.Max_Text);
      exception -- Max_Error
      when others =>
         UI.Max_Error;

         return;
      end Max_Error;

      if High < Low then
         Value := High;
         High := Low;
         Low := Value;
      end if;

      UI.Set_Min (Value => Low);
      UI.Set_Max (Value => High);

      Get_Value : declare
         subtype Desired is Integer range Low .. High;

         package Random is new Ada.Numerics.Discrete_Random (Result_Subtype => Desired);

         Gen : Random.Generator;
      begin -- Get_Value
         Random.Reset (Gen => Gen);
         Value := Random.Random (Gen);
         UI.Show_Result (Value => Value);
      end Get_Value;
   end Generate_One;

   Event : UI.Event_ID;
begin -- Random_Int.Program
   All_Events : loop
      Event := UI.Next_Event;

      case Event is
      when UI.Generate =>
         Generate_One;
      when UI.Quit =>
         exit All_Events;
      end case;
   end loop All_Events;
end Random_Int.Program;

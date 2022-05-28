-- An Ada-oriented GUI library
-- Implementation derived from Gnoga
--
-- Copyright (C) 2022 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

separate (Ada_GUI)
function Next_Event (Timeout : Duration := Duration'Last) return Next_Result_Info is
   function Parse_Mouse_Event (Message : String) return Mouse_Event_Info is
      use Ada.Strings.Fixed;

      Event  : Mouse_Event_Info;
      S      : Positive := Message'First;
      F      : Natural  := Message'First - 1;
      Button : Integer;

      function Split return String;
      function Split return Integer;
      function Split return Boolean;
         -- Split string and extract values

      function Split return String is
      begin
         S := F + 1;
         F := Index (Message, "|", From => S);

         return Message (S .. F - 1);
      end Split;

      function Split return Integer is
         S : constant String := Split;
      begin
         if Index (S, ".") > 0 then
            return Integer (Float'Value (S) );
         else
            return Integer'Value (S);
         end if;
      exception
      when E : others =>
         Gnoga.Log (Message => "Error Parse_Mouse_Event converting " & S & " to Integer (forced to 0).");
         Gnoga.Log (Message => Ada.Exceptions.Exception_Information (E));

         return 0;
      end Split;

      function Split return Boolean is
      begin
         return Split = "true";
      end Split;

      Left_Button   : constant := 1;
      Middle_Button : constant := 2;
      Right_Button  : constant := 3;
   begin
      Event.X := Split;
      Event.Y := Split;
      Event.Screen_X := Split;
      Event.Screen_Y := Split;

      Button := Split;
      Event.Left_Button   := Button = Left_Button;
      Event.Middle_Button := Button = Middle_Button;
      Event.Right_Button  := Button = Right_Button;

      Event.Alt     := Split;
      Event.Control := Split;
      Event.Shift   := Split;
      Event.Meta    := Split;

      return Event;
   end Parse_Mouse_Event;

   function Parse_Keyboard_Event (Message : String) return Keyboard_Event_Info is
      use Ada.Strings.Fixed;

      Event  : Keyboard_Event_Info;
      S      : Integer := Message'First;
      F      : Integer := Message'First - 1;

      function Split return String;
      function Split return Integer;
      function Split return Boolean;
      function Split return Wide_Character;

      function Split return String is
      begin
         S := F + 1;
         F := Index (Message, "|", From => S);

         return Message (S .. (F - 1));
      end Split;

      function Split return Integer is
         S : constant String := Split;
      begin
         if Index (S, ".") > 0 then
            return Integer (Float'Value (S));
         else
            return Integer'Value (S);
         end if;
      exception
      when E : others =>
         Gnoga.Log (Message => "Error Parse_Keyboard_Event converting " & S & " to Integer (forced to 0).");
            Gnoga.Log (Message => Ada.Exceptions.Exception_Information (E));

         return 0;
      end Split;

      function Split return Boolean is
      begin
         return Split = "true";
      end Split;

      function Split return Wide_Character is
      begin
         return Wide_Character'Val (Split);
      end Split;
   begin
      Event.Key_Code := Split;
      Event.Key_Char := Split;
      Event.Alt := Split;
      Event.Control := Split;
      Event.Shift := Split;
      Event.Meta := Split;

      return Event;
   end Parse_Keyboard_Event;

   Resize_Text : constant String := "resize";
   Left_Text   : constant String := "click";
   Right_Text  : constant String := "contextmenu";
   Double_Text : constant String := "dblclick";
   Key_Text    : constant String := "keypress";

   Final_Time : Ada.Real_Time.Time;
   Event      : Gnoga.Gui.Event_Info;

   use type Ada.Real_Time.Time;
begin -- Next_Event
   Get_Final : begin
      Final_Time := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Timeout);
   exception -- Get_Final
   when others =>
      Final_Time := Ada.Real_Time.Time_Last;
   end Get_Final;

   select
      Dialog_Control.Block;
   or
      delay until Final_Time;

      return (Timed_Out => True);
   end select;

   Skip_Invalid : loop
      select
         Gnoga.Gui.Event_Queue.Dequeue (Element => Event);

         if Event.Event = Resize_Text and then Event.Object.all in Gnoga.Gui.Window.Window_Type then
            Event.Object.Flush_Buffer;
            Gnoga.Gui.Window.Window_Type (Event.Object.all).On_Resize;
            Event.Object.Flush_Buffer;
         elsif Event.Event = Left_Text or Event.Event = Right_Text or Event.Event = Double_Text or Event.Event = Key_Text then
            Make_Event : declare
               ID    : constant String  := Event.Object.ID;
               Index : constant Natural := Ada.Strings.Fixed.Index (ID, "R"); -- R indicates a radio button

               Local : Event_Info (Kind => (if Event.Event = Left_Text then Left_Click
                                            elsif Event.Event = Right_Text then Right_Click
                                            elsif Event.Event = Double_Text then Double_Click
                                            else Key_Press) );
            begin -- Make_Event
               Local.ID   := (Value => Integer'Value ( (if Index > 0 then ID (Index + 1 .. ID'Last) else ID) ) );
               Local.Data := Event.Data;

               if Local.Kind in Left_Click | Right_Click | Double_Click then
                  Local.Mouse := Parse_Mouse_Event (Ada.Strings.Unbounded.To_String (Event.Data) );
               else
                  Local.Key := Parse_Keyboard_Event (Ada.Strings.Unbounded.To_String (Event.Data) );
               end if;

               return (Timed_Out => False, Event => Local);
            exception -- Make_Event
            when others => -- Event.Object.ID is not the image of an ID
               null;
            end Make_Event;
         elsif Event.Event = Closed_Text then
            return (Timed_Out => False, Event => (Kind => Window_Closed, others => <>) );
         else
            null; -- Ignore event
         end if;
      or
         delay until Final_Time;

         return (Timed_Out => True);
      end select;
   end loop Skip_Invalid;
end Next_Event;

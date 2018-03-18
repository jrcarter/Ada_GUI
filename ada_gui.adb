-- Proof of concept of an Ada-oriented GUI interface, with only those things needed to implement Random_Int
-- Quick and dirty implementation on top of Gnoga, not ready for prime time
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Window;

package body Ada_GUI is
   Window : Gnoga.Gui.Window.Window_Type;
   Form   : Gnoga.Gui.Element.Form.Form_Type;
   Ended  : Boolean := False with Atomic;

   function Program_Finished return Boolean is (Ended);

   type Widget_Info (Kind : Widget_Kind_ID := Text_Box) is record
      case Kind is
      when Text_Box =>
         Box   : Gnoga.Gui.Element.Form.Text_Access;
         Label : Gnoga.Gui.Element.Form.Label_Access;
      when Button =>
         Switch : Gnoga.Gui.Element.Common.Button_Access;
      end case;
   end record;

   package Widget_Lists is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Widget_Info);

   Widget_List : Widget_Lists.Vector;

   package Event_Queue_IF is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Event_Info);
   package Event_Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Event_Queue_IF);

   Event_Queue : Event_Queues.Queue;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      ID     : constant Widget_ID   := Widget_ID'Value (Object.ID);
      Widget : constant Widget_Info := Widget_List (Natural (ID) );

      Event : Event_Info (Widget_Kind => Widget.Kind);
   begin -- On_Click
      if Widget.Kind /= Button then
         return;
      end if;

      Event.ID := ID;
      Event.Event_Kind := Left_Click;
      Event_Queue.Enqueue (New_Item => Event);
   end On_Click;

   function New_Item (Kind : Widget_Kind_ID; Text : in String; Break_Before : Boolean := False; Label : in String := "")
   return Widget_ID is
      ID : constant Widget_ID := Widget_ID (Widget_List.Last_Index + 1);

      Widget : Widget_Info (Kind => Kind);
   begin -- New_Item
      if Break_Before then
         Form.New_Line;
      end if;

      case Kind is
      when Text_Box =>
         Widget.Box := new Gnoga.Gui.Element.Form.Text_Type;
         Widget.Box.Create (Form => Form, Value => Text, ID => ID'Image);
         Widget.Label := new Gnoga.Gui.Element.Form.Label_Type;
         Widget.Label.Create (Form => Form, Label_For => Widget.Box.all, Content => Label);
      when Button =>
         Widget.Switch := new Gnoga.Gui.Element.Common.Button_Type;
         Widget.Switch.Create (Parent => Form, Content => Text, ID => ID'Image);
         Widget.Switch.On_Click_Handler (Handler => On_Click'Access);
      end case;

      Widget_List.Append (New_Item => Widget);

      return ID;
   end New_Item;

   procedure Set_Title (Title : in String) is
      -- Empty
   begin -- Set_Title
      Window.Document.Title (Value => Title);
   end Set_Title;

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

   procedure Set_Text (ID : in Widget_ID; Text : in String) is
      Widget : Widget_Info := Widget_List (Natural (ID) );
   begin -- Set_Text
      case Widget.Kind is
      when Text_Box =>
         Widget.Box.Value (Value => Text);
      when Button =>
         Widget.Switch.Text (Value => Text);
      end case;
   end Set_Text;

   function Text (ID : Widget_ID) return String is
      Widget : constant Widget_Info := Widget_List (Natural (ID) );
   begin -- Text
      case Widget.Kind is
      when Text_Box =>
         return Widget.Box.Value;
      when Button =>
         return Widget.Switch.Text;
      end case;
   end Text;

   procedure End_GUI is
      View : Gnoga.Gui.Element.Form.Form_Type;
   begin -- End_GUI
      Form.Remove;
      View.Create (Parent => Window);
      View.Put_Line (Message => Window.Document.Title & " ended");
      Gnoga.Application.Singleton.End_Application;
      Ended := True;
   end End_GUI;

   task GUI_Thread;

   Set_Up : Boolean := False with Atomic;

   task body GUI_Thread is
      -- Empty
   begin -- GUI_Thread
      Gnoga.Application.Open_URL;
      Gnoga.Application.Singleton.Initialize (Main_Window => Window);
      Gnoga.Application.HTML_On_Close (HTML => "Application ended");
      Form.Create (Parent => Window);
      Form.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      Set_Up := True;
      Gnoga.Application.Singleton.Message_Loop;
   end GUI_Thread;
begin -- Ada_Gui
   Wait : loop
      exit Wait when Set_Up;

      delay 0.1;
   end loop Wait;
end Ada_Gui;

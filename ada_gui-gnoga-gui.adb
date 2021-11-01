-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . G U I . B A S E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Exceptions;

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Conversions;

with Ada_GUI.Gnoga.Server.Connection;

package body Ada_GUI.Gnoga.Gui is

   Mouse_Event_Script : constant String :=
     "(e.clientX - e.target.getBoundingClientRect().left) + '|' + " &
     "(e.clientY - e.target.getBoundingClientRect().top) + '|' + " &
     "e.screenX + '|' + " &
     "e.screenY + '|' + e.which + '|' + e.altKey + '|' + " &
     "e.ctrlKey + '|' + e.shiftKey + '|' + e.metaKey + '|'";
   --  e.buttons would be better but not supported currently outside
   --  of firefox and would always return 0 on Mac so using e.which.
   --  The use of offsetLeft and offsetTop is to correct the X and Y
   --  to the actual X,Y of the target.

   Keyboard_Event_Script : constant String :=
     "e.keyCode + '|' + e.charCode + '|' + e.altKey + '|' + e.ctrlKey + '|'" &
     " + e.shiftKey + '|' + e.metaKey + '|'";

   function Parse_Mouse_Event (Message       : String;
                               Mouse_Message : Mouse_Message_Type)
                               return Mouse_Event_Record;
   --  Parse event message in to Mouse_Event_Record

   function Parse_Keyboard_Event (Message          : String;
                                  Keyboard_Message : Keyboard_Message_Type)
                                  return Keyboard_Event_Record;
   --  Parse event message in to Keyboard_Event_Record

   function Parse_Drop_Event (X, Y : out Integer; Message : in String)
                              return String;
   --  Parse on_drop event message

   -----------------------
   -- Parse_Mouse_Event --
   -----------------------

   function Parse_Mouse_Event (Message       : String;
                               Mouse_Message : Mouse_Message_Type)
                               return Mouse_Event_Record
   is
      use Ada.Strings.Fixed;

      Event  : Mouse_Event_Record;
      S      : Integer := Message'First;
      F      : Integer := Message'First - 1;
      Button : Integer;

      function Split return String;
      function Split return Integer;
      function Split return Boolean;
      --  Split string and extract values

      function Split return String is
      begin
         S := F + 1;
         F := Index (Source  => Message,
                     Pattern => "|",
                     From    => S);
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
            Log ("Error Parse_Mouse_Event converting to Integer" &
                   " (forced to 0).");
            Log (Ada.Exceptions.Exception_Information (E));
            return 0;
      end Split;

      function Split return Boolean is
      begin
         return Split = "true";
      end Split;

      No_Button     : constant := 0;
      Left_Button   : constant := 1;
      Middle_Button : constant := 2;
      Right_Button  : constant := 3;
   begin
      Event.Message := Mouse_Message;
      Event.X := Split;
      Event.Y := Split;
      Event.Screen_X := Split;
      Event.Screen_Y := Split;

      Button := Split;
      Event.Left_Button := Button = Left_Button;
      Event.Middle_Button := Button = Middle_Button;
      Event.Right_Button := Button = Right_Button;

      Event.Alt := Split;
      Event.Control := Split;
      Event.Shift := Split;
      Event.Meta := Split;

      return Event;
   end Parse_Mouse_Event;

   --------------------------
   -- Parse_Keyboard_Event --
   --------------------------

   function Parse_Keyboard_Event (Message          : String;
                                  Keyboard_Message : Keyboard_Message_Type)
                                  return Keyboard_Event_Record
   is
      use Ada.Strings.Fixed;

      Event  : Keyboard_Event_Record;
      S      : Integer := Message'First;
      F      : Integer := Message'First - 1;

      function Split return String;
      function Split return Integer;
      function Split return Boolean;
      function Split return Wide_Character;

      function Split return String is
      begin
         S := F + 1;
         F := Index (Source  => Message,
                     Pattern => "|",
                     From    => S);
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
            Log ("Error Parse_Keyboard_Event converting to Integer" &
                   " (forced to 0).");
            Log (Ada.Exceptions.Exception_Information (E));
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
      Event.Message := Keyboard_Message;
      Event.Key_Code := Split;
      Event.Key_Char := Split;
      Event.Alt := Split;
      Event.Control := Split;
      Event.Shift := Split;
      Event.Meta := Split;

      return Event;
   end Parse_Keyboard_Event;

   ----------------------
   -- Parse_Drop_Event --
   ----------------------

   function Parse_Drop_Event (X, Y : out Integer; Message : in String)
                              return String
   is
      use Ada.Strings.Fixed;

      S      : Integer := Message'First;
      F      : Integer := Message'First - 1;

      function Split return String;
      function Split return Integer;

      function Split return String is
      begin
         S := F + 1;
         F := Index (Source  => Message,
                     Pattern => "|",
                     From    => S);
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
            Log ("Error Parse_Drop_Event converting to Integer" &
                   " (forced to 0).");
            Log (Ada.Exceptions.Exception_Information (E));
            return 0;
      end Split;
   begin
      X := Split;
      Y := Split;

      return Split;
   end Parse_Drop_Event;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Object : in out Base_Type) is
   begin
      Gnoga.Server.Connection.New_Unique_ID (Object.Unique_ID);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Object : in out Base_Type) is
   begin
      Object.On_Destroy;
      Object.Detach_From_Message_Queue;

      if not Gnoga.Server.Connection.Shutting_Down and
        Gnoga.Server.Connection.Valid (Object.Connection_ID)
      then
         if Object.Connection_ID /= No_Connection then

            if Object.ID_Type = Gnoga_ID then
               begin
                  Gnoga.Server.Connection.Execute_Script
                    (Object.Connection_ID,
                     "delete gnoga['" & Object.ID & "'];");
               exception
                  when E : Gnoga.Server.Connection.Connection_Error =>
                     --  Socket error to browser
                     Log ("Error connection " & Object.ID &
                            " socket error to browser.");
                     Log (Ada.Exceptions.Exception_Information (E));
               end;
            end if;
            Object.Connection_ID := No_Connection;
            --  Cannot call Object.Parent (null); because the parent may be finalized
            Object.Parent_Object := null;
         end if;
      end if;
   exception
      when E : others =>
         Log ("Error finalizing - " & Object.ID);
         Log (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Object : in out Base_Type) is
      Dummy_P : Pointer_To_Base_Class := Object'Unchecked_Access;
      procedure Free_Object is
        new Ada.Unchecked_Deallocation (Base_Type'Class,
                                        Pointer_To_Base_Class);
   begin
      Free_Object (Dummy_P);
   end Free;

   -------------------------------------------------------------------------
   --  Base_Type - Creation Methods
   -------------------------------------------------------------------------

   ------------------------
   -- Create_With_Script --
   ------------------------

   procedure Create_With_Script
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String;
      Script        : in     String;
      ID_Type       : in     ID_Enumeration := DOM_ID)
   is
   begin
      if Object.Connection_ID /= No_Connection then
         raise Object_Already_Created;
      end if;

      Gnoga.Server.Connection.Execute_Script (ID     => Connection_ID,
                                              Script => Script);

      Object.Attach (Connection_ID => Connection_ID,
                     ID            => ID,
                     ID_Type       => ID_Type);

      Object.On_Create;
      Object.Bind_Event (Event   => "click",
                         Message => "",
                         Script  => Mouse_Event_Script);
      Object.Bind_Event (Event   => "contextmenu",
                         Message => "",
                         Script  => Mouse_Event_Script,
                         Cancel  => True);
      Object.Bind_Event (Event   => "dblclick",
                         Message => "",
                         Script  => Mouse_Event_Script);
      Object.Bind_Event (Event   => "keypress",
                         Message => "",
                         Script  => Keyboard_Event_Script);
      Object.Bind_Event (Event   => "resize",
                         Message => "");
   end Create_With_Script;

   -------------------------
   -- Attach_Using_Parent --
   -------------------------

   procedure Attach_Using_Parent
     (Object   : in out Base_Type;
      Parent   : in     Base_Type'Class;
      ID       : in     String;
      ID_Type  : in     ID_Enumeration := DOM_ID)
   is
   begin
      Object.Attach (Connection_ID => Parent.Connection_ID,
                     ID            => ID,
                     ID_Type       => ID_Type);
   end Attach_Using_Parent;
   ------------
   -- Attach --
   ------------

   procedure Attach
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String;
      ID_Type       : in     ID_Enumeration := DOM_ID)
   is
   begin
      Object.Web_ID        := Ada.Strings.Unbounded.To_Unbounded_String (ID);
      Object.Connection_ID := Connection_ID;
      Object.ID_Type       := ID_Type;

      Object.Attach_To_Message_Queue;
   end Attach;

   -------------------------------------------------------------------------
   --  Base_Type - Properties
   -------------------------------------------------------------------------

   ---------------
   -- Unique_ID --
   ---------------

   function Unique_ID (Object : Base_Type) return Gnoga.Unique_ID is
   begin
      return Object.Unique_ID;
   end Unique_ID;

   -------------------
   -- Connection_ID --
   -------------------

   function Connection_ID (Object : Base_Type)
                           return Gnoga.Connection_ID
   is
   begin
      return Object.Connection_ID;
   end Connection_ID;

   procedure Connection_ID (Object : in out Base_Type;
                            Value  : in Gnoga.Connection_ID)
   is
   begin
      Object.Connection_ID := Value;
   end Connection_ID;

   -----------
   -- Valid --
   -----------

   function Valid (Object : Base_Type) return Boolean is
   begin
      if Object.Connection_ID = No_Connection then
         return False;
      else
         return Gnoga.Server.Connection.Valid (Object.Connection_ID);
      end if;
   end Valid;

   --------
   -- ID --
   --------

   function ID (Object : Base_Type) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Web_ID);
   end ID;

   procedure ID (Object  : in out Base_Type;
                 ID      : in     String;
                 ID_Type : in     ID_Enumeration)
   is
   begin
      Object.Web_ID := Ada.Strings.Unbounded.To_Unbounded_String (ID);
      Object.ID_Type := ID_Type;
   end ID;

   -------------
   -- ID_Type --
   -------------

   function ID_Type (Object : Base_Type) return ID_Enumeration
   is
   begin
      return Object.ID_Type;
   end ID_Type;

   ------------------
   -- DOM_Selector --
   ------------------

   function DOM_Selector (Object : Base_Type) return String is
   begin
      if Object.ID_Type = DOM_ID or Object.ID_Type = Gnoga_ID then
         return "#" & Object.ID;
      else
         return Object.ID;
      end if;
   end DOM_Selector;

   ---------------------
   -- Connection_Data --
   ---------------------

   function Connection_Data
     (Object : Base_Type)
      return Pointer_to_Connection_Data_Class
   is
   begin
      return Gnoga.Server.Connection.Connection_Data (Object.Connection_ID);
   end Connection_Data;

   ------------
   -- Parent --
   ------------

   function Parent (Object : Base_Type)
                    return Pointer_To_Base_Class
   is
   begin
      return Object.Parent_Object;
   end Parent;

   procedure Parent (Object : in out Base_Type;
                     Value  : in out Base_Type'Class)
   is
   begin
      if Object.Parent_Object /= null then
         Object.Parent_Object.On_Child_Removed (Object);
      end if;

      Object.Parent_Object := Value'Unchecked_Access;

      Value.On_Child_Added (Object);
   end Parent;

   procedure Parent (Object : in out Base_Type;
                     Value  : in Pointer_To_Base_Class)
   is
   begin
      if Object.Parent_Object /= null then
         Object.Parent_Object.On_Child_Removed (Object);
      end if;

      Object.Parent_Object := Value;

      if Value /= null then
         Value.On_Child_Added (Object);
      end if;
   end Parent;

   ------------
   -- Height --
   ------------

   procedure Height (Object : in out Base_Type; Value : in Integer) is
   begin
      Object.jQuery_Execute ("height(" & Left_Trim (Value'Img) & ");");
      Object.On_Message ("resize", "");
   end Height;

   function Height (Object : Base_Type) return Integer is
   begin
      return Object.jQuery_Execute ("height();");
   end Height;

   -----------
   -- Width --
   -----------

   procedure Width (Object : in out Base_Type; Value : in Integer) is
   begin
      Object.jQuery_Execute ("width(" & Left_Trim (Value'Img) & ");");
      Object.On_Message ("resize", "");
   end Width;

   function Width (Object : Base_Type) return Integer is
   begin
      return Object.jQuery_Execute ("width();");
   end Width;

   --------------
   -- Property --
   --------------

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     String)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "','" &
                               Escape_Quotes (Value) & "');");
   end Property;

   function Property (Object : Base_Type; Name : String) return String is
   begin
      return Object.jQuery_Execute ("prop ('" & Name & "');");
   end Property;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Integer)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "'," & Value'Img & ");");
   end Property;

   function Property (Object : Base_Type; Name : String) return Integer is
   begin
      return Object.jQuery_Execute ("prop ('" & Name & "');");
   end Property;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Float)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "'," & Value'Img & ");");
   end Property;

   function Property (Object : Base_Type; Name : String) return Float is
   begin
      return Object.jQuery_Execute ("prop ('" & Name & "');");
   end Property;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Boolean)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "'," & Value'Img & ");");
   end Property;

   function Property (Object : Base_Type; Name : String) return Boolean is
   begin
      return Object.Property (Name) = "true";
   end Property;

   -------------
   -- Dynamic --
   -------------

   procedure Dynamic (Object : in out Base_Type; Value : Boolean := True) is
   begin
      Object.Is_Dynamic := Value;
   end Dynamic;

   function Dynamic (Object : Base_Type) return Boolean is
   begin
      return Object.Is_Dynamic;
   end Dynamic;

   -------------------------------------------------------------------------
   --  Base_Type - Methods
   -------------------------------------------------------------------------

   -----------
   -- Focus --
   -----------

   procedure Focus (Object : in out Base_Type) is
   begin
      Object.Execute ("focus();");
   end Focus;

   ----------
   -- Blur --
   ----------

   procedure Blur (Object : in out Base_Type) is
   begin
      Object.Execute ("blur();");
   end Blur;

   ------------
   -- Execute--
   ------------

   procedure Execute (Object : in out Base_Type; Method : in String) is
   begin
      Object.jQuery_Execute ("get(0)." & Method);
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return String is
   begin
      return Object.jQuery_Execute ("get(0)." & Method);
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return Integer is
   begin
      return Object.jQuery_Execute ("get(0)." & Method);
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return Float is
   begin
      return Object.jQuery_Execute ("get(0)." & Method);
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return Boolean is
   begin
      return Object.Execute (Method) = "true";
   end Execute;

   -----------------------
   -- Buffer_Connection --
   -----------------------

   function Buffer_Connection (Object : Base_Type) return Boolean is
   begin
      return Gnoga.Server.Connection.Buffer_Connection (Object.Connection_ID);
   end Buffer_Connection;

   procedure Buffer_Connection (Object : in out Base_Type;
                                Value  : in     Boolean := True)
   is
   begin
      Gnoga.Server.Connection.Buffer_Connection (Object.Connection_ID, Value);
   end Buffer_Connection;

   ------------------
   -- Flush_Buffer --
   ------------------

   procedure Flush_Buffer (Object : in out Base_Type) is
   begin
      Gnoga.Server.Connection.Flush_Buffer (Object.Connection_ID);
   end Flush_Buffer;

   -------------------------------------------------------------------------
   --  Base_Type - Events
   -------------------------------------------------------------------------

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Resize_Event := Handler;
   end On_Resize_Handler;

   procedure Fire_On_Resize (Object : in out Base_Type)
   is
   begin
      if Object.On_Resize_Event /= null then
         if not Object.In_Resize then
            Object.In_Resize := True;
            Object.On_Resize_Event (Object);
            Object.In_Resize := False;
         end if;
      end if;
   end Fire_On_Resize;

   procedure On_Resize (Object : in out Base_Type) is
   begin
      Object.Fire_On_Resize;
   end On_Resize;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      if Object.On_Scroll_Event /= null then
         Object.Unbind_Event ("scroll");
      end if;

      Object.On_Scroll_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "scroll",
                            Message => "");
      end if;
   end On_Scroll_Handler;

   procedure Fire_On_Scroll (Object : in out Base_Type)
   is
   begin
      if Object.On_Scroll_Event /= null then
         Object.On_Scroll_Event (Object);
      end if;
   end Fire_On_Scroll;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Focus_Event /= null then
         Object.Unbind_Event ("focus");
      end if;

      Object.On_Focus_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "focus",
                            Message => "");
      end if;
   end On_Focus_Handler;

   procedure Fire_On_Focus (Object : in out Base_Type)
   is
   begin
      if Object.On_Focus_Event /= null then
         Object.On_Focus_Event (Object);
      end if;
   end Fire_On_Focus;

   -------------
   -- On_Blur --
   -------------

   procedure On_Blur_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Blur_Event /= null then
         Object.Unbind_Event ("blur");
      end if;

      Object.On_Blur_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "blur",
                            Message => "");
      end if;
   end On_Blur_Handler;

   procedure Fire_On_Blur (Object : in out Base_Type)
   is
   begin
      if Object.On_Blur_Event /= null then
         Object.On_Blur_Event (Object);
      end if;
   end Fire_On_Blur;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      if Object.On_Change_Event /= null then
         Object.Unbind_Event ("change");
      end if;

      Object.On_Change_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "change",
                            Message => "");
      end if;
   end On_Change_Handler;

   procedure Fire_On_Change (Object : in out Base_Type)
   is
   begin
      if Object.On_Change_Event /= null then
         Object.On_Change_Event (Object);
      end if;
   end Fire_On_Change;

   -----------------
   -- On_Focus_In --
   -----------------

   procedure On_Focus_In_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event)
   is
   begin
      if Object.On_Focus_In_Event /= null then
         Object.Unbind_Event ("focusin");
      end if;

      Object.On_Focus_In_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "focusin",
                            Message => "");
      end if;
   end On_Focus_In_Handler;

   procedure Fire_On_Focus_In (Object : in out Base_Type)
   is
   begin
      if Object.On_Focus_In_Event /= null then
         Object.On_Focus_In_Event (Object);
      end if;
   end Fire_On_Focus_In;

   ------------------
   -- On_Focus_Out --
   ------------------

   procedure On_Focus_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event)
   is
   begin
      if Object.On_Focus_Out_Event /= null then
         Object.Unbind_Event ("focusout");
      end if;

      Object.On_Focus_Out_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "focusout",
                            Message => "");
      end if;
   end On_Focus_Out_Handler;

   procedure Fire_On_Focus_Out (Object : in out Base_Type)
   is
   begin
      if Object.On_Focus_Out_Event /= null then
         Object.On_Focus_Out_Event (Object);
      end if;
   end Fire_On_Focus_Out;

   --------------
   -- On_Input --
   --------------

   procedure On_Input_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Input_Event /= null then
         Object.Unbind_Event ("input");
      end if;

      Object.On_Input_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "input",
                            Message => "");
      end if;
   end On_Input_Handler;

   procedure Fire_On_Input (Object : in out Base_Type)
   is
   begin
      if Object.On_Input_Event /= null then
         Object.On_Input_Event (Object);
      end if;
   end Fire_On_Input;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Reset_Event /= null then
         Object.Unbind_Event ("reset");
      end if;

      Object.On_Reset_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "reset",
                            Message => "",
                            Cancel  => True);
      end if;
   end On_Reset_Handler;

   procedure Fire_On_Reset (Object : in out Base_Type)
   is
   begin
      if Object.On_Reset_Event /= null then
         Object.On_Reset_Event (Object);
      end if;
   end Fire_On_Reset;

   ---------------
   -- On_Search --
   ---------------

   procedure On_Search_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      if Object.On_Search_Event /= null then
         Object.Unbind_Event ("search");
      end if;

      Object.On_Search_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "search",
                            Message => "");
      end if;
   end On_Search_Handler;

   procedure Fire_On_Search (Object : in out Base_Type)
   is
   begin
      if Object.On_Search_Event /= null then
         Object.On_Search_Event (Object);
      end if;
   end Fire_On_Search;

   ---------------
   -- On_Select --
   ---------------

   procedure On_Select_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      if Object.On_Select_Event /= null then
         Object.Unbind_Event ("select");
      end if;

      Object.On_Select_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "select",
                            Message => "");
      end if;
   end On_Select_Handler;

   procedure Fire_On_Select (Object : in out Base_Type)
   is
   begin
      if Object.On_Select_Event /= null then
         Object.On_Select_Event (Object);
      end if;
   end Fire_On_Select;

   ---------------
   -- On_Submit --
   ---------------

   procedure On_Submit_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      if Object.On_Submit_Event /= null then
         Object.Unbind_Event ("submit");
      end if;

      Object.On_Submit_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "submit",
                            Message => "",
                            Cancel  => True);
      end if;
   end On_Submit_Handler;

   procedure Fire_On_Submit (Object : in out Base_Type)
   is
   begin
      if Object.On_Submit_Event /= null then
         Object.On_Submit_Event (Object);
      end if;
   end Fire_On_Submit;

   --------------
   -- On_Click --
   --------------

   procedure On_Click_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      If Object.On_Click_Event /= null And Object.On_Mouse_Click_Event = Null then
         Object.Unbind_Event ("click");
      end if;

      Object.On_Click_Event := Handler;

      if Handler /= null then --and Object.On_Mouse_Click_Event = null then
         Object.Bind_Event (Event   => "click",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Click_Handler;

   procedure Fire_On_Click (Object : in out Base_Type)
   is
   begin
      if Object.On_Click_Event /= null then
         Object.On_Click_Event (Object);
      end if;
   end Fire_On_Click;

   --------------------
   -- On_Mouse_Click --
   --------------------

   procedure On_Mouse_Click_Handler (Object  : in out Base_Type;
                                     Handler : in     Mouse_Event)
   is
   begin
      if
        Object.On_Click_Event = null and
        Object.On_Mouse_Click_Event /= null
      then
         Object.Unbind_Event ("click");
      end if;

      Object.On_Mouse_Click_Event := Handler;

      if Handler /= null and Object.On_Click_Event = null then
         Object.Bind_Event (Event   => "click",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Mouse_Click_Handler;

   procedure Fire_On_Mouse_Click (Object   : in out Base_Type;
                                  Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Click_Event /= null then
         Object.On_Mouse_Click_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Click;

   ---------------------
   -- On_Context_Menu --
   ---------------------

   procedure On_Context_Menu_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event)
   is
   begin
      if
        Object.On_Mouse_Right_Click_Event = null and
        Object.On_Context_Menu_Event /= null
      then
         Object.Unbind_Event ("contextmenu");
      end if;

      Object.On_Context_Menu_Event := Handler;

      if Handler /= null and Object.On_Mouse_Right_Click_Event = null then
         Object.Bind_Event (Event   => "contextmenu",
                            Message => "",
                            Script  => Mouse_Event_Script,
                            Cancel  => True);
      end if;
   end On_Context_Menu_Handler;

   procedure Fire_On_Context_Menu (Object : in out Base_Type)
   is
   begin
      if Object.On_Context_Menu_Event /= null then
         Object.On_Context_Menu_Event (Object);
      end if;
   end Fire_On_Context_Menu;

   --------------------------
   -- On_Mouse_Right_Click --
   --------------------------

   procedure On_Mouse_Right_Click_Handler (Object  : in out Base_Type;
                                           Handler : in     Mouse_Event)
   is
   begin
      if
        Object.On_Mouse_Right_Click_Event /= null and
        Object.On_Context_Menu_Event = null
      then
         Object.Unbind_Event ("contextmenu");
      end if;

      Object.On_Mouse_Right_Click_Event := Handler;

      if Handler /= null and Object.On_Context_Menu_Event = null then
         Object.Bind_Event (Event   => "contextmenu",
                            Message => "",
                            Script  => Mouse_Event_Script,
                            Cancel  => True);
      end if;
   end On_Mouse_Right_Click_Handler;

   procedure Fire_On_Mouse_Right_Click (Object   : in out Base_Type;
                                        Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Right_Click_Event /= null then
         Object.On_Mouse_Right_Click_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Right_Click;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event)
   is
   begin
      if
        Object.On_Double_Click_Event /= null and
        Object.On_Mouse_Double_Click_Event = null
      then
         Object.Unbind_Event ("dblclick");
      end if;

      Object.On_Double_Click_Event := Handler;

      if Handler /= null and Object.On_Mouse_Double_Click_Event = null then
         Object.Bind_Event (Event   => "dblclick",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Double_Click_Handler;

   procedure Fire_On_Double_Click (Object : in out Base_Type)
   is
   begin
      if Object.On_Double_Click_Event /= null then
         Object.On_Double_Click_Event (Object);
      end if;
   end Fire_On_Double_Click;

   ---------------------------
   -- On_Mouse_Double_Click --
   ---------------------------

   procedure On_Mouse_Double_Click_Handler (Object  : in out Base_Type;
                                            Handler : in     Mouse_Event)
   is
   begin
      if
        Object.On_Double_Click_Event = null and
        Object.On_Mouse_Double_Click_Event /= null
      then
         Object.Unbind_Event ("dblclick");
      end if;

      Object.On_Mouse_Double_Click_Event := Handler;

      if Handler /= null and Object.On_Double_Click_Event = null then
         Object.Bind_Event (Event   => "dblclick",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Mouse_Double_Click_Handler;

   procedure Fire_On_Mouse_Double_Click (Object   : in out Base_Type;
                                         Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Double_Click_Event /= null then
         Object.On_Mouse_Double_Click_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Double_Click;

   --------------------
   -- On_Mouse_Enter --
   --------------------

   procedure On_Mouse_Enter_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event)
   is
   begin
      if Object.On_Mouse_Enter_Event /= null then
         Object.Unbind_Event ("mouseenter");
      end if;

      Object.On_Mouse_Enter_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mouseenter",
                            Message => "");
      end if;
   end On_Mouse_Enter_Handler;

   procedure Fire_On_Mouse_Enter (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Enter_Event /= null then
         Object.On_Mouse_Enter_Event (Object);
      end if;
   end Fire_On_Mouse_Enter;

   --------------------
   -- On_Mouse_Leave --
   --------------------

   procedure On_Mouse_Leave_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Mouse_Leave_Event /= null then
         Object.Unbind_Event ("mouseleave");
      end if;

      Object.On_Mouse_Leave_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mouseleave",
                            Message => "");
      end if;
   end On_Mouse_Leave_Handler;

   procedure Fire_On_Mouse_Leave (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Leave_Event /= null then
         Object.On_Mouse_Leave_Event (Object);
      end if;
   end Fire_On_Mouse_Leave;

   --------------------
   -- On_Mouse_Over --
   --------------------

   procedure On_Mouse_Over_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Mouse_Over_Event /= null then
         Object.Unbind_Event ("mouseover");
      end if;

      Object.On_Mouse_Over_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mouseover",
                            Message => "");
      end if;
   end On_Mouse_Over_Handler;

   procedure Fire_On_Mouse_Over (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Over_Event /= null then
         Object.On_Mouse_Over_Event (Object);
      end if;
   end Fire_On_Mouse_Over;

   ------------------
   -- On_Mouse_Out --
   ------------------

   procedure On_Mouse_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event)
   is
   begin
      if Object.On_Mouse_Out_Event /= null then
         Object.Unbind_Event ("mouseout");
      end if;

      Object.On_Mouse_Out_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mouseout",
                            Message => "");
      end if;
   end On_Mouse_Out_Handler;

   procedure Fire_On_Mouse_Out (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Out_Event /= null then
         Object.On_Mouse_Out_Event (Object);
      end if;
   end Fire_On_Mouse_Out;

   -------------------
   -- On_Mouse_Down --
   -------------------

   procedure On_Mouse_Down_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event)
   is
   begin
      if Object.On_Mouse_Down_Event /= null then
         Object.Unbind_Event ("mousedown");
      end if;

      Object.On_Mouse_Down_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mousedown",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Mouse_Down_Handler;

   procedure Fire_On_Mouse_Down (Object   : in out Base_Type;
                                 Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Down_Event /= null then
         Object.On_Mouse_Down_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Down;

   -----------------
   -- On_Mouse_Up --
   -----------------

   procedure On_Mouse_Up_Handler (Object  : in out Base_Type;
                                  Handler : in     Mouse_Event)
   is
   begin
      if Object.On_Mouse_Up_Event /= null then
         Object.Unbind_Event ("mouseup");
      end if;

      Object.On_Mouse_Up_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mouseup",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Mouse_Up_Handler;

   procedure Fire_On_Mouse_Up (Object   : in out Base_Type;
                               Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Up_Event /= null then
         Object.On_Mouse_Up_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Up;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event)
   is
   begin
      if Object.On_Mouse_Move_Event /= null then
         Object.Unbind_Event ("mousemove");
      end if;

      Object.On_Mouse_Move_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "mousemove",
                            Message => "",
                            Script  => Mouse_Event_Script);
      end if;
   end On_Mouse_Move_Handler;

   procedure Fire_On_Mouse_Move (Object   : in out Base_Type;
                                 Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Move_Event /= null then
         Object.On_Mouse_Move_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Move;

   procedure On_Drag_Start_Handler (Object    : in out Base_Type;
                                    Handler   : in     Action_Event;
                                    Drag_Text : in     String;
                                    Drag_Type : in     String := "text/plain")
   is
   begin
      if Object.On_Drag_Start_Event /= null then
         Object.Unbind_Event ("dragstart");
      end if;

      Object.On_Drag_Start_Event := Handler;

      if Handler /= null then
         Object.Bind_Event
           (Event   => "dragstart",
            Message => "",
            Eval  =>
             "e.originalEvent.dataTransfer.setData('" & Drag_Type & "', '" &
             Escape_Quotes (Drag_Text) & "');");
      end if;
   end On_Drag_Start_Handler;

   procedure Fire_On_Drag_Start (Object : in out Base_Type)
   is
   begin
      if Object.On_Drag_Start_Event /= null then
         Object.On_Drag_Start_Event (Object);
      end if;
   end Fire_On_Drag_Start;

   procedure On_Drag_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event)
   is
   begin
      if Object.On_Drag_Event /= null then
         Object.Unbind_Event ("drag");
      end if;

      Object.On_Drag_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "drag",
                            Message => "");
      end if;
   end On_Drag_Handler;

   procedure Fire_On_Drag (Object : in out Base_Type)
   is
   begin
      if Object.On_Drag_Event /= null then
         Object.On_Drag_Event (Object);
      end if;
   end Fire_On_Drag;

   procedure On_Drag_End_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event)
   is
   begin
      if Object.On_Drag_End_Event /= null then
         Object.Unbind_Event ("dragend");
      end if;

      Object.On_Drag_End_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "dragend",
                            Message => "");
      end if;
   end On_Drag_End_Handler;

   procedure Fire_On_Drag_End (Object : in out Base_Type)
   is
   begin
      if Object.On_Drag_End_Event /= null then
         Object.On_Drag_End_Event (Object);
      end if;
   end Fire_On_Drag_End;

   procedure On_Drag_Enter_Handler (Object  : in out Base_Type;
                                    Handler : in     Action_Event)
   is
   begin
      if Object.On_Drag_Enter_Event /= null then
         Object.Unbind_Event ("dragenter");
      end if;

      Object.On_Drag_Enter_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "dragenter",
                            Message => "");
      end if;
   end On_Drag_Enter_Handler;

   procedure Fire_On_Drag_Enter (Object : in out Base_Type)
   is
   begin
      if Object.On_Drag_Enter_Event /= null then
         Object.On_Drag_Enter_Event (Object);
      end if;
   end Fire_On_Drag_Enter;

   procedure On_Drag_Leave_Handler (Object  : in out Base_Type;
                                    Handler : in     Action_Event)
   is
   begin
      if Object.On_Drag_Leave_Event /= null then
         Object.Unbind_Event ("dragleave");
      end if;

      Object.On_Drag_Leave_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "dragleave",
                            Message => "");
      end if;
   end On_Drag_Leave_Handler;

   procedure Fire_On_Drag_Leave (Object : in out Base_Type)
   is
   begin
      if Object.On_Drag_Leave_Event /= null then
         Object.On_Drag_Leave_Event (Object);
      end if;
   end Fire_On_Drag_Leave;

   procedure On_Drop_Handler (Object    : in out Base_Type;
                              Handler   : in     Drop_Event;
                              Drag_Type : in     String := "text/plain")
   is
   begin
      if Object.On_Drop_Event /= null then
         Object.Unbind_Event ("dragover");
         Object.Unbind_Event ("drop");
      end if;

      Object.On_Drop_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "dragover",
                            Message => "",
                            Eval  => "e.preventDefault();");
         Object.Bind_Event
           (Event   => "drop",
            Message => "",
            Eval    => "e.preventDefault();",
            Script  =>
              "(e.originalEvent.clientX - " &
              "e.target.getBoundingClientRect().left) + '|' + " &
              "(e.originalEvent.clientY - " &
              "e.target.getBoundingClientRect().top)  + '|' + " &
              "e.originalEvent.dataTransfer.getData('" &
              Drag_Type & "') + '|'");
      end if;
   end On_Drop_Handler;

   procedure Fire_On_Drop (Object    : in out Base_Type;
                           X, Y      : in     Integer;
                           Drag_Text : in     String)
   is
   begin
      if Object.On_Drop_Event /= null then
         Object.On_Drop_Event (Object, X, Y, Drag_Text);
      end if;
   end Fire_On_Drop;

   ------------------
   -- On_Character --
   ------------------

   procedure On_Character_Handler (Object  : in out Base_Type;
                                   Handler : in     Character_Event)
   is
   begin
      if
        Object.On_Character_Event /= null and
        Object.On_Wide_Character_Event = null and
        Object.On_Key_Press_Event = null
      then
         Object.Unbind_Event ("keypress");
      end if;

      Object.On_Character_Event := Handler;

      if
        Handler /= null and
        Object.On_Wide_Character_Event = null and
        Object.On_Key_Press_Event = null
      then
         Object.Bind_Event (Event   => "keypress",
                            Message => "",
                            Script  => Keyboard_Event_Script);
      end if;
   end On_Character_Handler;

   procedure Fire_On_Character (Object : in out Base_Type;
                                Key    : in     Character)
   is
   begin
      if Object.On_Character_Event /= null then
         Object.On_Character_Event (Object, Key);
      end if;
   end Fire_On_Character;

   -----------------------
   -- On_Wide_Character --
   -----------------------

   procedure On_Wide_Character_Handler (Object  : in out Base_Type;
                                        Handler : in     Wide_Character_Event)
   is
   begin
      if
        Object.On_Character_Event = null and
        Object.On_Wide_Character_Event /= null and
        Object.On_Key_Press_Event = null
      then
         Object.Unbind_Event ("keypress");
      end if;

      Object.On_Wide_Character_Event := Handler;

      if
        Handler /= null and
        Object.On_Character_Event = null and
        Object.On_Key_Press_Event = null
      then
         Object.Bind_Event (Event   => "keypress",
                            Message => "",
                            Script  => Keyboard_Event_Script);
      end if;
   end On_Wide_Character_Handler;

   procedure Fire_On_Wide_Character (Object : in out Base_Type;
                                     Key    : in     Wide_Character)
   is
   begin
      if Object.On_Wide_Character_Event /= null then
         Object.On_Wide_Character_Event (Object, Key);
      end if;
   end Fire_On_Wide_Character;

   ------------------
   -- On_Key_Down --
   ------------------

   procedure On_Key_Down_Handler (Object  : in out Base_Type;
                                  Handler : in     Keyboard_Event)
   is
   begin
      if Object.On_Key_Down_Event /= null then
         Object.Unbind_Event ("keydown");
      end if;

      Object.On_Key_Down_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "keydown",
                            Message => "",
                            Script  => Keyboard_Event_Script);
      end if;
   end On_Key_Down_Handler;

   procedure Fire_On_Key_Down (Object : in out Base_Type;
                               Event  : in     Keyboard_Event_Record)
   is
   begin
      if Object.On_Key_Down_Event /= null then
         Object.On_Key_Down_Event (Object, Event);
      end if;
   end Fire_On_Key_Down;

   ---------------
   -- On_Key_Up --
   ---------------

   procedure On_Key_Up_Handler (Object  : in out Base_Type;
                                Handler : in     Keyboard_Event)
   is
   begin
      if Object.On_Key_Up_Event /= null then
         Object.Unbind_Event ("keyup");
      end if;

      Object.On_Key_Up_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "keyup",
                            Message => "",
                            Script  => Keyboard_Event_Script);
      end if;
   end On_Key_Up_Handler;

   procedure Fire_On_Key_Up (Object : in out Base_Type;
                             Event  : in     Keyboard_Event_Record)
   is
   begin
      if Object.On_Key_Up_Event /= null then
         Object.On_Key_Up_Event (Object, Event);
      end if;
   end Fire_On_Key_Up;

   ------------------
   -- On_Key_Press --
   ------------------

   procedure On_Key_Press_Handler (Object  : in out Base_Type;
                                  Handler : in     Keyboard_Event)
   is
   begin
      if
        Object.On_Character_Event = null and
        Object.On_Wide_Character_Event = null and
        Object.On_Key_Press_Event /= null
      then
         Object.Unbind_Event ("keypress");
      end if;

      Object.On_Key_Press_Event := Handler;

      if
        Handler /= null and
        Object.On_Character_Event = null and
        Object.On_Wide_Character_Event = null
      then
         Object.Bind_Event (Event   => "keypress",
                            Message => "",
                            Script  => Keyboard_Event_Script);
      end if;
   end On_Key_Press_Handler;

   procedure Fire_On_Key_Press (Object : in out Base_Type;
                               Event  : in     Keyboard_Event_Record)
   is
   begin
      if Object.On_Key_Press_Event /= null then
         Object.On_Key_Press_Event (Object, Event);
      end if;
   end Fire_On_Key_Press;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event)
   is
   begin
      if Object.On_Copy_Event /= null then
         Object.Unbind_Event ("copy");
      end if;

      Object.On_Copy_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "copy",
                            Message => "");
      end if;
   end On_Copy_Handler;

   procedure Fire_On_Copy (Object : in out Base_Type)
   is
   begin
      if Object.On_Copy_Event /= null then
         Object.On_Copy_Event (Object);
      end if;
   end Fire_On_Copy;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut_Handler (Object  : in out Base_Type;
                             Handler : in     Action_Event)
   is
   begin
      if Object.On_Cut_Event /= null then
         Object.Unbind_Event ("cut");
      end if;

      Object.On_Cut_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "cut",
                            Message => "");
      end if;
   end On_Cut_Handler;

   procedure Fire_On_Cut (Object : in out Base_Type)
   is
   begin
      if Object.On_Cut_Event /= null then
         Object.On_Cut_Event (Object);
      end if;
   end Fire_On_Cut;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      if Object.On_Paste_Event /= null then
         Object.Unbind_Event ("paste");
      end if;

      Object.On_Paste_Event := Handler;

      if Handler /= null then
         Object.Bind_Event (Event   => "paste",
                            Message => "");
      end if;
   end On_Paste_Handler;

   procedure Fire_On_Paste (Object : in out Base_Type)
   is
   begin
      if Object.On_Paste_Event /= null then
         Object.On_Paste_Event (Object);
      end if;
   end Fire_On_Paste;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Object : in out Base_Type)
   is
   begin
      Object.Fire_On_Create;
   end On_Create;

   procedure On_Create_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Create_Event := Handler;
   end On_Create_Handler;

   procedure Fire_On_Create (Object : in out Base_Type)
   is
   begin
      if Object.On_Create_Event /= null then
         Object.On_Create_Event (Object);
      end if;
   end Fire_On_Create;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Object : in out Base_Type)
   is
   begin
      Object.Fire_On_Destroy;
   end On_Destroy;

   procedure On_Destroy_Handler (Object  : in out Base_Type;
                                 Handler : in     Action_Event)
   is
   begin
      Object.On_Destroy_Event := Handler;
   end On_Destroy_Handler;

   procedure Fire_On_Destroy (Object : in out Base_Type)
   is
   begin
      if Object.On_Destroy_Event /= null then
         Object.On_Destroy_Event (Object);
      end if;
   end Fire_On_Destroy;

   --------------------
   -- On_Child_Added --
   --------------------

   procedure On_Child_Added (Object : in out Base_Type;
                             Child  : in out Base_Type'Class)
   is
   begin
      Object.Fire_On_Child_Added (Child);
   end On_Child_Added;

   procedure On_Child_Added_Handler (Object  : in out Base_Type;
                                     Handler : in     Child_Changed_Event)
   is
   begin
      Object.On_Child_Added_Event := Handler;
   end On_Child_Added_Handler;

   procedure Fire_On_Child_Added (Object : in out Base_Type;
                                  Child  : in out Base_Type'Class)
   is
   begin
      if Object.On_Child_Added_Event /= null then
         Object.On_Child_Added_Event (Object, Child);
      end if;
   end Fire_On_Child_Added;

   ----------------------
   -- On_Child_Removed --
   ----------------------

   procedure On_Child_Removed (Object : in out Base_Type;
                               Child  : in out Base_Type'Class)
   is
   begin
      Object.Fire_On_Child_Removed (Child);
   end On_Child_Removed;

   procedure On_Child_Removed_Handler (Object  : in out Base_Type;
                                       Handler : in     Child_Changed_Event)
   is
   begin
      Object.On_Child_Removed_Event := Handler;
   end On_Child_Removed_Handler;

   procedure Fire_On_Child_Removed (Object : in out Base_Type;
                                    Child  : in out Base_Type'Class)
   is
   begin
      if Object.On_Child_Removed_Event /= null then
         Object.On_Child_Removed_Event (Object, Child);
      end if;
   end Fire_On_Child_Removed;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      -- Object Events --
      if Event = "scroll" then
         Object.Fire_On_Scroll;

      -- Form Events --
      elsif Event = "focus" then
         Object.Fire_On_Focus;
      elsif Event = "blur" then
         Object.Fire_On_Blur;
      elsif Event = "change" then
         Object.Fire_On_Change;
      elsif Event = "focusin" then
         Object.Fire_On_Focus_In;
      elsif Event = "focusout" then
         Object.Fire_On_Focus_Out;
      elsif Event = "input" then
         Object.Fire_On_Input;
      elsif Event = "reset" then
         Object.Fire_On_Reset;
      elsif Event = "search" then
         Object.Fire_On_Search;
      elsif Event = "select" then
         Object.Fire_On_Select;
      elsif Event = "submit" then
         Object.Fire_On_Submit;

      -- Mouse Events --

      elsif Event = "click" then
         Object.Fire_On_Click;
         Object.Fire_On_Mouse_Click (Parse_Mouse_Event (Message, Click));
      elsif Event = "dblclick" then
         Object.Fire_On_Double_Click;
         Object.Fire_On_Mouse_Double_Click
           (Parse_Mouse_Event (Message, Double_Click));
      elsif Event = "contextmenu" then
         Object.Fire_On_Context_Menu;
         Object.Fire_On_Mouse_Right_Click
           (Parse_Mouse_Event (Message, Right_Click));
      elsif Event = "mouseenter" then
         Object.Fire_On_Mouse_Enter;
      elsif Event = "mouseleave" then
         Object.Fire_On_Mouse_Leave;
      elsif Event = "mouseover" then
         Object.Fire_On_Mouse_Over;
      elsif Event = "mouseout" then
         Object.Fire_On_Mouse_Out;
      elsif Event = "mousedown" then
         Object.Fire_On_Mouse_Down (Parse_Mouse_Event (Message, Mouse_Down));
      elsif Event = "mouseup" then
         Object.Fire_On_Mouse_Up (Parse_Mouse_Event (Message, Mouse_Up));
      elsif Event = "mousemove" then
         Object.Fire_On_Mouse_Move (Parse_Mouse_Event (Message, Mouse_Move));

      elsif Event = "dragstart" then
         Object.Fire_On_Drag_Start;
      elsif Event = "drag" then
         Object.Fire_On_Drag;
      elsif Event = "dragend" then
         Object.Fire_On_Drag_End;
      elsif Event = "dragenter" then
         Object.Fire_On_Drag_Enter;
      elsif Event = "dragleave" then
         Object.Fire_On_Drag_Leave;
      elsif Event = "dragover" then
         null;
      elsif Event = "drop" then
         declare
            D_X, D_Y : Integer;
            D_S      : constant String := Parse_Drop_Event (D_X, D_Y, Message);
         begin
            Object.Fire_On_Drop (D_X, D_Y, D_S);
         end;

      -- Keyboard Events --

      elsif Event = "keydown" then
         Object.Fire_On_Key_Down (Parse_Keyboard_Event (Message, Key_Down));
      elsif Event = "keyup" then
         Object.Fire_On_Key_Up (Parse_Keyboard_Event (Message, Key_Up));
      elsif Event = "keypress" then
         declare
            E : constant Keyboard_Event_Record :=
              Parse_Keyboard_Event (Message, Key_Press);
            C : constant Character :=
              Ada.Characters.Conversions.To_Character (E.Key_Char);
         begin
            Object.Fire_On_Key_Press (E);
            Object.Fire_On_Wide_Character (E.Key_Char);
            Object.Fire_On_Character (C);
         end;

      -- Clipboard Events --

      elsif Event = "copy" then
         Object.Fire_On_Copy;
      elsif Event = "cut" then
         Object.Fire_On_Cut;
      elsif Event = "paste" then
         Object.Fire_On_Paste;
      elsif Event = "resize" then
         if not Object.In_Resize then
            Object.In_Resize := True;
            Base_Type'Class (Object).On_Resize;
            Object.In_Resize := False;
         end if;
      else
         Gnoga.Log ("Unhandled Event : " & Event);
      end if;
   end On_Message;

   procedure On_Message_Handler (Object  : in out Base_Type;
                                 Handler : in     Message_Event)
   is
   begin
      Object.On_Message_Event := Handler;
   end On_Message_Handler;

   procedure Fire_On_Message (Object   : in out Base_Type;
                              Event    : in     String;
                              Message  : in     String;
                              Continue : out    Boolean)
   is
   begin
      Continue := True;

      if Object.On_Message_Event /= null then
         Object.On_Message_Event (Object, Event, Message, Continue);
      end if;
   end Fire_On_Message;

   -------------------------------------------------------------------------
   --  Base_Type - Event Internals
   -------------------------------------------------------------------------

   ----------------
   -- Bind_Event --
   ----------------

   procedure Bind_Event (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String;
                         Eval    : in     String    := "";
                         Script  : in     String    := "";
                         Cancel  : in     Boolean   := False)
   is
      US : constant String := Object.Unique_ID'Img;

      Full_Message : constant String := US (US'First + 1 .. US'Last) &
        "|" & Event & "|" & Message;

      function If_Script return String;
      function Cancel_Event return String;

      function If_Script return String is
      begin
         if Script = "" then
            return "";
         else
            return "+" & Script;
         end if;
      end If_Script;

      function Cancel_Event return String is
      begin
         if Cancel then
            return " return false;";
         else
            return "";
         end if;
      end Cancel_Event;
   begin
      Bind_Event_Script (Object => Object,
                         Event  => Event,
                         Script => Eval & "ws.send ('" &
                           Escape_Quotes (Full_Message) & "'" &
                           If_Script & ");" & Cancel_Event);
   end Bind_Event;

   -----------------------
   -- Bind_Event_Script --
   -----------------------

   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String)
   is
   begin
      Object.jQuery_Execute ("on ('" & Event & "', function (e, data) {" &
                               Script & "});");
   end Bind_Event_Script;

   ------------------
   -- Unbind_Event --
   ------------------

   procedure Unbind_Event (Object : in out Base_Type;
                           Event  : in     String)
   is
   begin
      Object.jQuery_Execute ("off ('" & Event & "');");
   end Unbind_Event;

   -----------------------------
   -- Attach_To_Message_Queue --
   -----------------------------

   procedure Attach_To_Message_Queue (Object : in out Base_Type) is
   begin
      Gnoga.Server.Connection.Add_To_Message_Queue (Object);
   end Attach_To_Message_Queue;

   --------------------------------
   -- Detach_From_Message_Queue --
   --------------------------------

   procedure Detach_From_Message_Queue (Object : in out Base_Type) is
   begin
      Gnoga.Server.Connection.Delete_From_Message_Queue (Object);
   end Detach_From_Message_Queue;

   ---------------------
   -- Script_Accessor --
   ---------------------

   function Script_Accessor (Object : Base_Type) return String is
   begin
      return Script_Accessor (Object.ID, Object.ID_Type);
   end Script_Accessor;

   function Script_Accessor (ID : String; ID_Type : ID_Enumeration)
                             return String
   is
   begin
      case ID_Type is
         when No_ID =>
            raise Object_Was_Not_Created;
         when DOM_ID =>
            return "#" & ID;
         when Script =>
            return ID;
         when Gnoga_ID =>
            return "gnoga['" & ID & "']";
      end case;
   end Script_Accessor;

   ------------
   -- jQuery --
   ------------

   function jQuery (Object : Base_Type) return String is
   begin
      case Object.ID_Type is
         when No_ID =>
            raise Object_Was_Not_Created;
         when DOM_ID =>
            return "$('" & Object.Script_Accessor & "')";
         when Script | Gnoga_ID =>
            return "$(" & Object.Script_Accessor & ")";
      end case;
   end jQuery;

   --------------------
   -- jQuery_Execute --
   --------------------

   procedure jQuery_Execute (Object : in out Base_Type; Method : String) is
      Message_Script : constant String := jQuery (Object) & "." & Method;
   begin
      Gnoga.Server.Connection.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end jQuery_Execute;

   function jQuery_Execute (Object : Base_Type; Method : String)
                            return String
   is
      Message_Script : constant String := jQuery (Object) & "." & Method;
   begin
      return Gnoga.Server.Connection.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end jQuery_Execute;

   function jQuery_Execute (Object : Base_Type; Method : String)
                            return Integer
   is
      use Ada.Strings.Fixed;

      R : constant String := Object.jQuery_Execute (Method);
   begin
      if Index (R, ".") > 0 then
         return Integer (Float'Value (R));
      else
         return Integer'Value (R);
      end if;
   exception
      when E : others =>
         Log ("Error jQuery_Execute converting to Integer (forced to 0).");
         Log (Ada.Exceptions.Exception_Information (E));
         return 0;
   end jQuery_Execute;

   function jQuery_Execute (Object : Base_Type; Method : String)
                            return Float
   is
      R : constant String := Object.jQuery_Execute (Method);
   begin
      return Float'Value (R);
   exception
      when E : others =>
         Log ("Error jQuery_Execute converting to Float (forced to 0.0).");
         Log (Ada.Exceptions.Exception_Information (E));
         return 0.0;
   end jQuery_Execute;
end Ada_GUI.Gnoga.Gui;

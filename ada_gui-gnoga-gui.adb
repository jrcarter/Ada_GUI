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
      Object.Parent_Object := Value'Unchecked_Access;

      Value.On_Child_Added (Object);
   end Parent;

   procedure Parent (Object : in out Base_Type;
                     Value  : in Pointer_To_Base_Class)
   is
   begin
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

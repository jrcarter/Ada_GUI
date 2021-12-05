-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . G U I . W I N D O W                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Calendar.Formatting;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

with Ada_GUI.Gnoga.Client_Storage;
with Ada_GUI.Gnoga.Server.Connection;
pragma Elaborate (Ada_GUI.Gnoga.Server.Connection);
with Ada_GUI.Gnoga.Gui.Element;
with Ada_GUI.Gnoga.Gui.View;

package body Ada_GUI.Gnoga.Gui.Window is

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Object : in out Window_Type) is
      P : Gnoga.Pointer_to_Connection_Data_Class :=
            Object.Connection_Data;

      procedure Free_Data is
        new Ada.Unchecked_Deallocation
          (Gnoga.Connection_Data_Type'Class,
           Gnoga.Pointer_to_Connection_Data_Class);
   begin
      if Object.View /= null and Object.View_Is_Dynamic then
         Object.View.Free;
         Object.View := null;
      else
         Object.View := null;
      end if;

      Gnoga.Gui.Base_Type (Object).Finalize;

      if Object.Free_Connection_Data then
         Free_Data (P);
         Object.Connection_Data (null);
         Log ("Connection_Data freed " & Object.Connection_ID'Image);
      end if;
   exception
      when E : others =>
         Log ("Error finalizing Window - " & Object.ID);
         Log (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Window        : in out Window_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String                     := "window";
      ID_Type       : in     Gnoga.ID_Enumeration := Gnoga.Script)
   is
   begin
      if ID_Type = Gnoga.DOM_ID then
         raise Invalid_ID_Type;
      end if;

      Gnoga.Gui.Attach
        (Object        => Gnoga.Gui.Base_Type (Window),
         Connection_ID => Connection_ID,
         ID            => ID,
         ID_Type       => ID_Type);

      Window.DOM_Document.Attach (Connection_ID, ID, ID_Type);

      Window.Location.Attach
        (Connection_ID => Connection_ID,
         ID            => Window.jQuery & ".prop ('location')",
         ID_Type       => Gnoga.Script);

      Window.Bind_Event (Event   => "resize",
                         Message => "");
   end Attach;

   procedure Attach
     (Window  : in out Window_Type;
      Parent  : in out Window_Type'Class;
      ID      : in     String;
      ID_Type : in     Gnoga.ID_Enumeration := Gnoga.Script)
   is
      CID : constant String := Gnoga.Server.Connection.Execute_Script
        (Parent.Connection_ID,
         Script_Accessor (ID, ID_Type) & ".gnoga['Connection_ID']");
   begin
      if ID_Type = Gnoga.DOM_ID then
         raise Invalid_ID_Type;
      end if;

      Attach (Window, Gnoga.Connection_ID'Value (CID));
   exception
      when E : others =>
         Log ("Unable to find gnoga['Connection_ID'] on " & ID & " eval returned : " & CID);
         Log (Ada.Exceptions.Exception_Information (E));
         raise Not_A_Gnoga_Window;
   end Attach;

   --------------
   -- Reattach --
   --------------

   procedure Reattach (Window : in out Window_Type;
                       Parent : in out Window_Type'Class)
   is
      CID : constant String := Gnoga.Server.Connection.Execute_Script
        (Parent.Connection_ID,
         Script_Accessor (Window.ID, Window.ID_Type) & ".gnoga['Connection_ID']");
   begin
      Connection_ID (Object => Window, Value => Gnoga.Connection_ID'Value (CID));
   end Reattach;

   ---------------------------
   -- Disable_Auto_Set_View --
   ---------------------------

   procedure Disable_Auto_Set_View (Window : in out Window_Type;
                                    Value  : in     Boolean := True)
   is
   begin
      Window.Auto_Set_View := not Value;
   end Disable_Auto_Set_View;
   --------------
   -- Set_View --
   --------------

   procedure Set_View (Window : in out Window_Type;
                       Object : in out Gnoga.Gui.Base_Type'Class;
                       Place  : in     Boolean := True)
   is
      use Gnoga.Gui.Element;
   begin
      if not (Object in Element_Type'Class) then
         raise Invalid_ID_Type with "Not in Element_Type'Class";
      end if;

      if Place then
         Element_Type (Object).Place_Inside_Top_Of
           (Window.Document.Body_Element.all);
      end if;

      Window.View := Object'Unchecked_Access;
      Window.View_Is_Dynamic := Object.Dynamic;

      Element_Type (Object).Box_Sizing (Border_Box);
      Element_Type (Object).Position (Gnoga.Gui.Element.Fixed);
      Element_Type (Object).Display ("block");
      Element_Type (Object).Left (0);
      Element_Type (Object).Top (0);
      Element_Type (Object).Box_Height (Window.Height);
      Element_Type (Object).Box_Width (Window.Width);
   end Set_View;

   --------------
   -- Get_View --
   --------------

   function Get_View (Window : Window_Type)
                      return Gnoga.Gui.Pointer_To_Base_Class
   is
   begin
      return Window.View;
   end Get_View;

   -----------------
   -- Remove_View --
   -----------------

   procedure Remove_View (Window : in out Window_Type) is
   begin
      Window.View := null;
   end Remove_View;

   --------------
   -- Document --
   --------------

   function Document (Window : Window_Type)
                      return Gnoga.Gui.Document.Document_Access
   is
   begin
      return Window.DOM_Document'Unrestricted_Access;
   end Document;

   --------------
   -- Location --
   --------------

   function Location (Window : Window_Type)
                      return Gnoga.Gui.Location.Location_Access
   is
   begin
      return Window.Location'Unrestricted_Access;
   end Location;

   ------------------------
   -- Browser_Status_Bar --
   ------------------------

   procedure Browser_Status_Bar (Window : in out Window_Type;
                                 Value  : in     String)
   is
   begin
      Window.Property ("status", Value);
   end Browser_Status_Bar;

   function Browser_Status_Bar (Window : Window_Type) return String
   is
   begin
      return Window.Property ("status");
   end Browser_Status_Bar;

   ----------
   -- Name --
   ----------

   procedure Name (Window : in out Window_Type; Value : in String) is
   begin
      Window.Property ("name", Value);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Window : Window_Type) return String is
   begin
      return Window.Property ("name");
   end Name;

   ------------------
   -- Inner_Height --
   ------------------

   procedure Inner_Height (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("innerHeight", Value);
   end Inner_Height;

   ------------------
   -- Inner_Height --
   ------------------

   function Inner_Height (Window : Window_Type) return Integer is
   begin
      return Window.Property ("innerHeight");
   end Inner_Height;

   -----------------
   -- Inner_Width --
   -----------------

   procedure Inner_Width (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("innerWidth", Value);
   end Inner_Width;

   -----------------
   -- Inner_Width --
   -----------------

   function Inner_Width (Window : Window_Type) return Integer is
   begin
      return Window.Property ("innerWidth");
   end Inner_Width;

   ------------------
   -- Outer_Height --
   ------------------

   procedure Outer_Height (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("outerHeight", Value);
   end Outer_Height;

   ------------------
   -- Outer_Height --
   ------------------

   function Outer_Height (Window : Window_Type) return Integer is
   begin
      return Window.Property ("outerHeight");
   end Outer_Height;

   -----------------
   -- Outer_Width --
   -----------------

   procedure Outer_Width (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("outerWidth", Value);
   end Outer_Width;

   -----------------
   -- Outer_Width --
   -----------------

   function Outer_Width (Window : Window_Type) return Integer is
   begin
      return Window.Property ("outerWidth");
   end Outer_Width;

   --------------
   -- X_Offset --
   --------------

   function X_Offset (Window : Window_Type) return Integer is
   begin
      return Window.Property ("pageXOffset");
   end X_Offset;

   --------------
   -- Y_Offset --
   --------------

   function Y_Offset (Window : Window_Type) return Integer is
   begin
      return Window.Property ("pageYOffset");
   end Y_Offset;

   ---------
   -- Top --
   ---------

   function Top (Window : Window_Type) return Integer is
   begin
      return Window.Property ("screenY");
   end Top;

   ----------
   -- Left --
   ----------

   function Left (Window : Window_Type) return Integer is
   begin
      return Window.Property ("screenX");
   end Left;

   --------------------
   -- Form_Parameter --
   --------------------

   function Form_Parameter (Window : Window_Type; Name  : String)
                              return String
   is
   begin
      return Gnoga.Server.Connection.Form_Parameter (Window.Connection_ID,
                                                     Name);
   end Form_Parameter;

   ----------------------
   -- Gnoga_Session_ID --
   ----------------------

   function Gnoga_Session_ID (Window : Window_Type; Name : String := "gid")
                              return String
   is
      use Gnoga.Client_Storage;

      function Generate_Session_ID return String;
      --  Create a new unique string to identify sessions

      function Generate_Session_ID return String is
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use Ada.Calendar.Formatting;

         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         return Trim (Year (Now)'Img, Side => Both)
           & Trim (Month (Now)'Img, Side => Both)
           & Trim (Day (Now)'Img, Side => Both)
           & Trim (Hour (Now)'Img, Side => Both)
           & Trim (Minute (Now)'Img, Side => Both)
           & Trim (Second (Now)'Img, Side => Both)
           & Translate (Trim (Sub_Second (Now)'Img, Side => Both),
                        Ada.Strings.Maps.To_Mapping (".", "0"));
      end Generate_Session_ID;

      S   : Session_Storage_Type := Session_Storage (Window);
      Gid : constant String               := S.Get (Name);
   begin
      if Gid = "null" then
         declare
            New_Session : constant String := Generate_Session_ID;
         begin
            S.Set (Name, New_Session);

            return New_Session;
         end;
      else
         return Gid;
      end if;
   end Gnoga_Session_ID;

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (Window  : in out Window_Type;
      Data    : access Gnoga.Connection_Data_Type'Class;
      Dynamic : in     Boolean := True)
   is
   begin
      if Data = null then
         Gnoga.Server.Connection.Connection_Data
           (ID => Window.Connection_ID, Data => null);

         Window.Free_Connection_Data := False;
      else
         Gnoga.Server.Connection.Connection_Data
           (ID => Window.Connection_ID, Data => Data.all'Unrestricted_Access);

         Window.Free_Connection_Data := Dynamic;
      end if;
   end Connection_Data;

   ------------
   -- Launch --
   ------------

   procedure Launch (Window   : in out Window_Type;
                     Parent   : in out Window_Type'Class;
                     URL      : in     String;
                     Width    : in     Integer := -1;
                     Height   : in     Integer := -1;
                     Left     : in     Integer := -1;
                     Top      : in     Integer := -1;
                     Location : in     Boolean := False;
                     Menu     : in     Boolean := False;
                     Status   : in     Boolean := False;
                     Tool_Bar : in     Boolean := False;
                     Title    : in     Boolean := False)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;

      function Params return String;

      function Params return String is
         use Ada.Strings.Unbounded;

         P : Unbounded_String;
         C : Boolean := False;

         procedure Add_Param (S : String; V : String);
         procedure Add_Param (S : String; V : Boolean);

         procedure Add_Param (S : String; V : String) is
         begin
            if C then
               P := P & ", ";
            end if;

            P := P & To_Unbounded_String (S) & "=" & To_Unbounded_String (V);
            C := True;
         end Add_Param;

         procedure Add_Param (S : String; V : Boolean) is
         begin
            if V then
               Add_Param (S, "yes");
            else
               Add_Param (S, "no");
            end if;
         end Add_Param;

      begin
         if Width > -1 then
            Add_Param ("width", Width'Img);
         end if;

         if Height > -1 then
            Add_Param ("height", Height'Img);
         end if;

         if Top > -1 then
            Add_Param ("top", Top'Img);
         end if;

         if Left > -1 then
            Add_Param ("left", Left'Img);
         end if;

         Add_Param ("menubar", Menu);
         Add_Param ("status", Status);
         Add_Param ("toolbar", Tool_Bar);
         Add_Param ("menubar", Menu);
         Add_Param ("titlebar", Title);
         Add_Param ("location", Location);

         return To_String (P);
      end Params;

   begin
      Window.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => GID,
         Script        => "gnoga['" & GID & "']=window.open ('" & URL &
           "', '" & GID & "', '" & Params & "')",
         ID_Type       => Gnoga.Gnoga_ID);

      delay 0.25;
      --  Needed for Firefox or there is a race condition with the value of
      --  gnoga['" & GID & "']. Popups are nasty, they should be avoided.

      if
        Gnoga.Server.Connection.Execute_Script
          (Parent.Connection_ID, "gnoga['" & GID & "']") = "undefined"
      then
         raise Popup_Blocked;
      end if;

      Window.DOM_Document.Attach
        (Parent.Connection_ID, GID, Gnoga.Gnoga_ID);

      Window.Location.Attach
        (Connection_ID => Parent.Connection_ID,
         ID            => Window.jQuery & ".prop ('location')",
         ID_Type       => Gnoga.Script);
   end Launch;

   -----------
   -- Alert --
   -----------

   procedure Alert (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("alert ('" & Escape_Quotes (Message) & "');");
   end Alert;

   ---------
   -- Log --
   ---------

   procedure Log (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("console.log ('" & Escape_Quotes (Message) & "');");
   end Log;

   -----------
   -- Error --
   -----------

   procedure Error (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("console.error ('" & Escape_Quotes (Message) & "');");
   end Error;

   -----------
   -- Close --
   -----------

   procedure Close (Window : in out Window_Type) is
   begin
      Window.Execute ("close();");
   end Close;

   -----------
   -- Print --
   -----------

   procedure Print (Window : in out Window_Type) is
   begin
      Window.Execute ("print();");
   end Print;

   ---------------
   -- Resize_By --
   ---------------

   procedure Resize_By
     (Window        : in out Window_Type;
      Width, Height : Integer)
   is
   begin
      Window.Execute ("resizeBy(" & Width'Img & "," & Height'Img & ");");
   end Resize_By;

   ---------------
   -- Resize_To --
   ---------------

   procedure Resize_To
     (Window        : in out Window_Type;
      Width, Height : Integer)
   is
   begin
      Window.Execute ("resizeTo(" & Width'Img & "," & Height'Img & ");");
   end Resize_To;

   -------------
   -- Move_By --
   -------------

   procedure Move_By (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("moveBy(" & X'Img & "," & Y'Img & ");");
   end Move_By;

   -------------
   -- Move_To --
   -------------

   procedure Move_To (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("moveTo(" & X'Img & "," & Y'Img & ");");
   end Move_To;

   ---------------
   -- Scroll_By --
   ---------------

   procedure Scroll_By (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("scrollBy(" & X'Img & "," & Y'Img & ");");
   end Scroll_By;

   ---------------
   -- Scroll_To --
   ---------------

   procedure Scroll_To (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("scrollTo(" & X'Img & "," & Y'Img & ");");
   end Scroll_To;

   ----------------------
   -- Close_Connection --
   ----------------------

   procedure Close_Connection (Window : in out Window_Type) is
   begin
      Gnoga.Server.Connection.Close (Window.Connection_ID);
   end Close_Connection;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize (Window : in out Window_Type) is
      use Gnoga.Gui.Element;
   begin
      if Window.View /= null then
         Element_Access (Window.View).Box_Height (Window.Height);
         Element_Access (Window.View).Box_Width (Window.Width);
      end if;
   end On_Resize;

   --------------------
   -- On_Child_Added --
   --------------------

   overriding
   procedure On_Child_Added (Object : in out Window_Type;
                             Child  : in out Gnoga.Gui.Base_Type'Class)
   is
   begin
      if Child in Gnoga.Gui.View.View_Base_Type'Class and Object.Auto_Set_View
      then
         if Gnoga.Gui.View.View_Base_Type (Child).Auto_Place then
            Object.Set_View (Child);
         end if;
      end if;
   end On_Child_Added;
end Ada_GUI.Gnoga.Gui.Window;

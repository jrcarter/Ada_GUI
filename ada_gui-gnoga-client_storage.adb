-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                   G N O G A . C L I E N T . S T O R A G E                --
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

with Ada_GUI.Gnoga.Server.Connection;

package body Ada_GUI.Gnoga.Client_Storage is
   --  One local and one session storage is available across an entire
   --  scheme://domain:port. Give that there is no point in supporting access
   --  to non-Gnoga iFrames or Popups since if they are not form the same
   --  scheme://domain:port the browser security would not allow access
   --  anyways.

   procedure Execute (Storage : in out Storage_Type'Class; Method : in String);
   function Execute (Storage : Storage_Type'Class; Method : String)
                     return String;
   --  Execute access to Storage on its Connection_ID

   -------------
   -- Execute --
   -------------

   procedure Execute (Storage : in out Storage_Type'Class; Method : in String)
   is
      Message_Script : constant String :=
                         Storage.Script_Accessor & "." & Method;
   begin
      if Storage.Connection_ID = Gnoga.No_Connection then
         raise Program_Error with "Use of Storage without Connection";
      end if;

      Gnoga.Server.Connection.Execute_Script
        (ID     => Storage.Connection_ID,
         Script => Message_Script);
   end Execute;

   function Execute (Storage : Storage_Type'Class; Method : String)
                     return String
   is
      Message_Script : constant String :=
                         Storage.Script_Accessor & "." & Method;
   begin
      if Storage.Connection_ID = Gnoga.No_Connection then
         raise Program_Error with "Use of Storage without Connection";
      end if;

      return Gnoga.Server.Connection.Execute_Script
        (ID     => Storage.Connection_ID,
         Script => Message_Script);
   end Execute;

   ------------
   -- Length --
   ------------

   function Length (Storage : Storage_Type) return Natural is
   begin
      return Natural'Value (Execute (Storage, "length"));
   end Length;

   ---------
   -- Key --
   ---------

   function Key (Storage : Storage_Type; Value : Positive) return String is
      JS_Value : constant Natural := Value - 1;
   begin
      return Execute (Storage, "key(" & JS_Value'Img & ")");
   end Key;

   ---------
   -- Set --
   ---------

   procedure Set (Storage : in out Storage_Type; Name, Value : String) is
   begin
      Execute (Storage, "setItem ('" & Escape_Quotes (Name) & "','" &
                 Escape_Quotes (Value) & "')");
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Storage : Storage_Type; Name : String) return String is
   begin
      return Execute (Storage, "getItem('" & Escape_Quotes (Name) & "')");
   end Get;

   ---------------------
   -- Script_Accessor --
   ---------------------

   function Script_Accessor (Storage : Storage_Type) return String is
   begin
      raise Program_Error
        with "Use Session_Storage_Type or Local_Storage_Type";
      return "";
   end Script_Accessor;

   -------------------
   -- Local_Storage --
   -------------------

   function Local_Storage
     (Object : Gnoga.Gui.Base_Type'Class)
      return Local_Storage_Type
   is
   begin
      return Local_Storage_Type'(Connection_ID => Object.Connection_ID);
   end Local_Storage;

   ---------------------
   -- Script_Accessor --
   ---------------------

   overriding
   function Script_Accessor (Storage : Local_Storage_Type) return String is
      pragma Unreferenced (Storage);
   begin
      return "localStorage";
   end Script_Accessor;

   ---------------------
   -- Session_Storage --
   ---------------------

   function Session_Storage
     (Object : Gnoga.Gui.Base_Type'Class)
      return Session_Storage_Type
   is
   begin
      return Session_Storage_Type'(Connection_ID => Object.Connection_ID);
   end Session_Storage;

   ---------------------
   -- Script_Accessor --
   ---------------------

   overriding
   function Script_Accessor (Storage : Session_Storage_Type) return String is
      pragma Unreferenced (Storage);
   begin
      return "sessionStorage";
   end Script_Accessor;

end Ada_GUI.Gnoga.Client_Storage;

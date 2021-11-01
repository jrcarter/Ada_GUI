-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                   G N O G A . C L I E N T . S T O R A G E                --
--                                                                          --
--                                 S p e c                                  --
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

with Ada_GUI.Gnoga.Gui;

package Ada_GUI.Gnoga.Client_Storage is

   -------------------------------------------------------------------------
   --  Storage_Type
   -------------------------------------------------------------------------
   --  Base class for client side storage of data.
   --  In order to ease access to client side data, the Storage_Type can be
   --  accessed using any object instead of just through the parent window.
   --  One local and one session storage is available across an entire
   --  location defined as scheme://domain:port.

   type Storage_Type is tagged private;
   type Storage_Access is access all Storage_Type;
   type Pointer_To_Storage_Class is access all Storage_Type'Class;

   function Length (Storage : Storage_Type) return Natural;
   --  Number of entries in Storage

   function Key (Storage : Storage_Type; Value : Positive) return String;
   --  Return the Key at Value

   procedure Set (Storage : in out Storage_Type; Name, Value : String);
   --  Set Name=Value in Storage

   function Get (Storage : Storage_Type; Name : String) return String;
   --  Get Value for Name in Storage. If Name does not exist returns "null"

   function Script_Accessor (Storage : Storage_Type) return String;

   type Local_Storage_Type is new Storage_Type with private;
   type Local_Storage_Access is access all Local_Storage_Type;
   type Pointer_To_Local_Storage_Class is access all Local_Storage_Type'Class;

   function Local_Storage (Object : Gnoga.Gui.Base_Type'Class)
                           return Local_Storage_Type;

   overriding
   function Script_Accessor (Storage : Local_Storage_Type) return String;

   type Session_Storage_Type is new Storage_Type with private;
   type Session_Storage_Access is access all Session_Storage_Type;
   type Pointer_To_Session_Storage_Class is
     access all Session_Storage_Type'Class;

   function Session_Storage (Object : Gnoga.Gui.Base_Type'Class)
                             return Session_Storage_Type;

   overriding
   function Script_Accessor (Storage : Session_Storage_Type) return String;

private
   type Storage_Type is tagged
      record
         Connection_ID : Gnoga.Connection_ID :=
                           Gnoga.No_Connection;
      end record;

   type Local_Storage_Type is new Storage_Type with null record;
   type Session_Storage_Type is new Storage_Type with null record;
end Ada_GUI.Gnoga.Client_Storage;

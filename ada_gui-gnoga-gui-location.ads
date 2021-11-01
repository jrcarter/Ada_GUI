-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . G U I . L O C A T I O N                   --
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

package Ada_GUI.Gnoga.Gui.Location is

   -------------------------------------------------------------------------
   --  Location_Type
   -------------------------------------------------------------------------
   --  Location_Type is the class encapsulating the DOM Location node
   --  To use, access via Window_Type.Location

   type Location_Type is new Gnoga.Gui.Base_Type with private;
   type Location_Access is access all Location_Type;
   type Pointer_To_Location_Class is access all Location_Type'Class;

   -------------------------------------------------------------------------
   --  Location_Type - Properties
   -------------------------------------------------------------------------

   procedure URL (Location : in out Location_Type; Value : in String);
   function URL (Location : Location_Type) return String;
   --  Setting URL will navigate the browser from the current location and
   --  close the current Gnoga Connection.

   procedure Hash (Location : in out Location_Type; Value : in String);
   function Hash (Location : Location_Type) return String;

   procedure Host (Location : in out Location_Type; Value : in String);
   function Host (Location : Location_Type) return String;

   procedure Host_Name (Location : in out Location_Type; Value : in String);
   function Host_Name (Location : Location_Type) return String;

   function Origin (Location : Location_Type) return String;

   procedure Path_Name (Location : in out Location_Type; Value : in String);
   function Path_Name (Location : Location_Type) return String;

   procedure Port (Location : in out Location_Type; Value : in String);
   function Port (Location : Location_Type) return String;

   procedure Protocol (Location : in out Location_Type; Value : in String);
   function Protocol (Location : Location_Type) return String;

   procedure Search (Location : in out Location_Type; Value : in String);
   function Search (Location : Location_Type) return String;

   function Parse (URL : in String; Encoding : String := "")
                   return Gnoga.Data_Map_Type;
   --  Parse form GET parameters in URL (typically from Search function)
   --  to Data_Map_Type
   --  Supported encodings are ISO-8859-1 (default)
   --  and UTF-8 (typically from Input_Encoding function)

   -------------------------------------------------------------------------
   --  Location_Type - Methods
   -------------------------------------------------------------------------

   procedure Reload (Location : in out Location_Type);
   --  Reload the current page

   procedure Replace (Location : in out Location_Type; URL : in String);
   --  Replace the current page with URL (the current page will be removed
   --  from the browser history)

   procedure Assign (Location : in out Location_Type; URL : in String);
   --  Assign URL to the current page, the current page will be available
   --  using the back button.

private
   type Location_Type is new Gnoga.Gui.Base_Type with null record;
end Ada_GUI.Gnoga.Gui.Location;

-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . S E R V E R . T E M P L A T E _ P A R S E R . S I M P L E  --
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

--  A simple find and replace template parser.
--  Templates are marked up as @@var_name.element@@
--
--  Info messages are available as @@gnoga_infos.row_number@@
--  Error messages are available as @@gnoga_errors.row_number@@
--  Note: The Simple parser is just a find and replace and so there is now
--        way to loop or otherwise determine in advance how many rows exist.

package Ada_GUI.Gnoga.Server.Template_Parser.Simple is

   function Load_View (Name : String) return String;
   --  Return named view with no passed data

   function Load_View (Name     : String;
                       Data_Map : Gnoga.Data_Map_Type;
                       Var_Name : String := "data")
                       return String;
   --  Return named view with data map

   function Load_View (Name : String; Data : View_Data)
                       return String;
   --  Return named view with Data

   function Load_View (Name      : String;
                       Data_List : View_Data_Array)
                       return String;
   --  Return named view with Data_List array of View_Data items

end Ada_GUI.Gnoga.Server.Template_Parser.Simple;

-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . S E R V E R . D A T A B A S E               --
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

package body Ada_GUI.Gnoga.Server.Database is

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type (Field : Field_Description) return String is
      Data  : constant String  :=
        Ada.Strings.Unbounded.To_String (Field.Data_Type);
      Right : constant Natural := Ada.Strings.Fixed.Index (Data, "(");
   begin
      if Right = 0 then
         return Data;
      else
         return Data (Data'First .. Right - 1);
      end if;
   end Field_Type;

   function Field_Size (Field : Field_Description) return Natural is
      Option : constant String := Field_Options (Field);
   begin
      if Option = "" then
         return 0;
      else
         declare
            Comma : constant Natural := Ada.Strings.Fixed.Index (Option, ",");
         begin
            if Comma = 0 then
               return Natural'Value (Option);
            else
               return Natural'Value (Option (Option'First .. Comma - 1));
            end if;
         end;
      end if;
   end Field_Size;

   --------------------
   -- Field_Decimals --
   --------------------

   function Field_Decimals (Field : Field_Description) return Natural is
      Option : constant String  := Field_Options (Field);
      Comma  : constant Natural := Ada.Strings.Fixed.Index (Option, ",");
   begin
      if Comma = 0 then
         return 0;
      else
         return Natural'Value (Option (Comma + 1 .. Option'Last));
      end if;
   end Field_Decimals;

   -------------------
   -- Field_Options --
   -------------------

   function Field_Options (Field : Field_Description) return String is
      Data  : constant String  :=
        Ada.Strings.Unbounded.To_String (Field.Data_Type);
      Right : constant Natural := Ada.Strings.Fixed.Index (Data, "(");
      Left  : constant Natural := Ada.Strings.Fixed.Index (Data, ")");
   begin
      if Right = 0 or Left = 0 then
         return "";
      else
         return Data (Right + 1 .. Left - 1);
      end if;
   end Field_Options;

end Ada_GUI.Gnoga.Server.Database;

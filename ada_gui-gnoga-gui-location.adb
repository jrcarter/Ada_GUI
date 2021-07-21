-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . L O C A T I O N                         --
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

package body Ada_GUI.Gnoga.Gui.Location is

   ---------
   -- URL --
   ---------

   procedure URL (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("href", Value);
   end URL;

   function URL (Location : Location_Type) return String is
   begin
      return Location.Property ("href");
   end URL;

   ----------
   -- Hash --
   ----------

   procedure Hash (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("hash", Value);
   end Hash;

   function Hash (Location : Location_Type) return String is
   begin
      return Location.Property ("hash");
   end Hash;

   ----------
   -- Host --
   ----------

   procedure Host (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("host", Value);
   end Host;

   function Host (Location : Location_Type) return String is
   begin
      return Location.Property ("host");
   end Host;

   ---------------
   -- Host_Name --
   ---------------

   procedure Host_Name (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("hostname", Value);
   end Host_Name;

   function Host_Name (Location : Location_Type) return String is
   begin
      return Location.Property ("hostname");
   end Host_Name;

   ------------
   -- Origin --
   ------------

   function Origin (Location : Location_Type) return String is
   begin
      return Location.Property ("origin");
   end Origin;

   ---------------
   -- Path_Name --
   ---------------

   procedure Path_Name (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("pathname", Value);
   end Path_Name;

   function Path_Name (Location : Location_Type) return String is
   begin
      return Location.Property ("pathname");
   end Path_Name;

   ----------
   -- Port --
   ----------

   procedure Port (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("port", Value);
   end Port;

   function Port (Location : Location_Type) return String is
   begin
      return Location.Property ("port");
   end Port;

   --------------
   -- Protocol --
   --------------

   procedure Protocol (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("protocol", Value);
   end Protocol;

   function Protocol (Location : Location_Type) return String is
   begin
      return Location.Property ("protocol");
   end Protocol;

   ------------
   -- Search --
   ------------

   procedure Search (Location : in out Location_Type; Value : in String) is
   begin
      Location.Property ("search", Value);
   end Search;

   function Search (Location : Location_Type) return String is
   begin
      return Location.Property ("search");
   end Search;

   -----------
   -- Parse --
   -----------

   function Parse (URL : in String; Encoding : String := "")
                   return Gnoga.Data_Map_Type is
      P : Gnoga.Data_Map_Type;
      Ind1, Ind2, Ind3 : Natural;
      use Ada.Strings.Fixed;
   begin
      Ind1 := Index (URL, "?");
      if Ind1 > 0 then
         loop
            Ind2 := Index (URL, "=", Ind1);
            if Ind2 > 0 then
               Ind3 := Index (URL, "&", Ind2);
               if Ind3 > 0 then
                  if Encoding /= "" then
                     P.Include
                     (URL_Decode
                        (URL (Ind1 + 1 .. Ind2 - 1),
                         Encoding), URL_Decode
                        (URL (Ind2 + 1 .. Ind3 - 1),
                         Encoding));
                  else
                     P.Include (URL (Ind1 + 1 .. Ind2 - 1),
                                URL (Ind2 + 1 .. Ind3 - 1));
                  end if;
                  Ind1 := Ind3;
               else
                  if Encoding /= "" then
                     P.Include
                     (URL_Decode
                        (URL (Ind1 + 1 .. Ind2 - 1),
                         Encoding), URL_Decode
                        (URL (Ind2 + 1 .. URL'Last),
                         Encoding));
                  else
                     P.Include (URL (Ind1 + 1 .. Ind2 - 1),
                                URL (Ind2 + 1 .. URL'Last));
                  end if;
                  exit;
               end if;
            else
               exit;
            end if;
         end loop;
      end if;
      return P;
   end Parse;

   ------------
   -- Reload --
   ------------

   procedure Reload (Location : in out Location_Type) is
   begin
      Location.Execute ("reload()");
   end Reload;

   -------------
   -- Replace --
   -------------

   procedure Replace (Location : in out Location_Type; URL : in String) is
   begin
      Location.Execute ("replace('" & Escape_Quotes (URL) & "')");
   end Replace;

   ------------
   -- Assign --
   ------------

   procedure Assign (Location : in out Location_Type; URL : in String) is
   begin
      Location.Execute ("assign('" & Escape_Quotes (URL) & "')");
   end Assign;

end Ada_GUI.Gnoga.Gui.Location;

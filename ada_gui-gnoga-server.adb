-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                         G N O G A . S E R V E R                          --
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

with Ada.Directories;

with GNAT.OS_Lib;

package body Ada_GUI.Gnoga.Server is
   function Find_Subdirectory (Sub : String) return String;
   --  Return the path to the given subdirectory or return "";

   -----------------------
   -- Find_Subdirectory --
   -----------------------

   function Find_Subdirectory (Sub : String) return String is
      Dir  : constant String := Application_Directory & Sub &
               GNAT.OS_Lib.Directory_Separator;

      Html : constant String := Application_Directory & "html" &
               GNAT.OS_Lib.Directory_Separator;
   begin
      if Ada.Directories.Exists (Dir) then
         return Dir;
      elsif Ada.Directories.Exists (Html) then
         return Html;
      else
         return Application_Directory;
      end if;
   end Find_Subdirectory;

   -------------------------
   -- Directory_Separator --
   -------------------------

   function Directory_Separator return String is
   begin
      return (1 => GNAT.OS_Lib.Directory_Separator);
   end Directory_Separator;

   ---------------------------
   -- Application_Directory --
   ---------------------------

   function Application_Directory return String is
      Exe : constant String := Executable_Directory;
   begin
      if Exe (Exe'Last - 3 .. Exe'Last - 1) = "bin" then
         return Exe (Exe'First .. Exe'Last - 4);
      else
         return Exe;
      end if;
   end Application_Directory;

   --------------------------
   -- Executable_Directory --
   --------------------------

   function Executable_Directory return String is
   begin
      return Ada.Directories.Current_Directory & GNAT.OS_Lib.Directory_Separator;
   end Executable_Directory;

   --------------------
   -- HTML_Directory --
   --------------------

   function HTML_Directory return String is
   begin
      return Find_Subdirectory ("html");
   end HTML_Directory;

   ------------------
   -- JS_Directory --
   ------------------

   function JS_Directory return String is
   begin
      return Find_Subdirectory ("js");
   end JS_Directory;

   -------------------
   -- IMG_Directory --
   -------------------

   function IMG_Directory return String is
   begin
      return Find_Subdirectory ("img");
   end IMG_Directory;

   -------------------
   -- CSS_Directory --
   -------------------

   function CSS_Directory return String is
   begin
      return Find_Subdirectory ("css");
   end CSS_Directory;

   ----------------------
   -- Upload_Directory --
   ----------------------

   function Upload_Directory return String is
   begin
      return Find_Subdirectory ("upload");
   end Upload_Directory;

   -------------------------
   -- Templates_Directory --
   -------------------------

   function Templates_Directory return String is
      Dir  : constant String := Application_Directory & "templates" &
               GNAT.OS_Lib.Directory_Separator;

      Alt : constant String := Application_Directory & "share" &
              GNAT.OS_Lib.Directory_Separator & "gnoga" &
              GNAT.OS_Lib.Directory_Separator & "templates" &
              GNAT.OS_Lib.Directory_Separator;
   begin
      if Ada.Directories.Exists (Dir) then
         return Dir;
      elsif Ada.Directories.Exists (Alt) then
         return Alt;
      else
         return Application_Directory;
      end if;
   end Templates_Directory;

end Ada_GUI.Gnoga.Server;

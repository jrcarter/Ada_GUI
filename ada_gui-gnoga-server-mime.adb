-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . S E R V E R . M I M E                   --
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

package body Ada_GUI.Gnoga.Server.Mime is

   ---------------
   -- Mime_Type --
   ---------------

   function Mime_Type (File_Name : String) return String is
      Ext : constant String := Ada.Directories.Extension (File_Name);
   begin
      if Ext = "js" then
         return "text/javascript";
      elsif Ext = "css" then
         return "text/css";
      elsif Ext = "jpg" or Ext = "jpeg" or Ext = "jpe" then
         return "image/jpg";
      elsif Ext = "png" then
         return "image/png";
      elsif Ext = "gif" then
         return "image/gif";
      elsif Ext = "pdf" then
         return "application/pdf";
      elsif Ext = "zip" then
         return "application/zip";
      elsif Ext = "gz" or Ext = "z" then
         return "application/x-gzip";
      elsif Ext = "ico" then
         return "image/x-icon";
      elsif Ext = "html" or Ext = "htm" then
         return "text/html";
      else
         return "text/plain";
      end if;
   end Mime_Type;

end Ada_GUI.Gnoga.Server.Mime;

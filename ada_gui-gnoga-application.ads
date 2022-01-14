-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . A P P L I C A T I O N                    --
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
--
-- Changed by J. Carter 2021 to only run "singleton" applications

with Ada_GUI.Gnoga.Gui.Window;

package Ada_GUI.Gnoga.Application is
   procedure Initialize
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      ID          : in     Positive := 8080;
      Title       : in     String   := "Ada-GUI Application";
      Icon        : in     String   := "favicon.ico");
   -- Initializes the application with ID
   -- If the default browser is not running, starts it
   -- Opens a new "window" (may be a browser tab) for the application
   -- Title is the title of the "window" and Icon its icon
   -- Applications with different IDs can run at the same time
   -- If multiple applications have the same ID, only one can run at a time
   --
   -- (To Be Honest: the ID is actually the port used to talk to the browser,
   --  and the restriction is because only one program can talk on a port at
   --  a time)

   function Favicon return String;
   --  Returns the name of the Icon passed to Initialize

   procedure End_Application;
   -- Terminate application.
end Ada_GUI.Gnoga.Application;

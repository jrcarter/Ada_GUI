-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                         G N O G A . S E R V E R                          --
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

--  Serverside bindings and tools

package Ada_GUI.Gnoga.Server is

   --  Gnoga applications generally use the following layout. However
   --  if the executable can be located in App Dir. Any missing standard
   --  subdirectory will instead use the html root which if missing is
   --  App Dir.
   --
   --  App Dir
   --    |
   --    |___ bin - your Gnoga app binary
   --    |
   --    |___ html - boot.html (or other boot loader used)
   --    |
   --    |___ js - must contain jquery.min.js
   --    |
   --    |___ css - optional, a directory for serving css files
   --    |
   --    |___ img - optional, a directory of serving graphics.
   --    |
   --    |___ templates - optional, if using Gnoga.Server.Template_Parser
   --    |
   --    |___ upload - option, optional directory for incoming files

   function Directory_Separator return String;
   --  Return the Directory Separator using for the OS Gnoga is compiled on.

   -- For Ada_GUI, these all usually return the current working directory

   function Application_Directory return String;
   --  This is the root directory for the application.

   function Executable_Directory return String;
   --  Locates this application's executable directory
   --  This is usually in Application_Directory/bin

   function HTML_Directory return String;
   --  Locates the applications HTML Root for this application

   function JS_Directory return String;
   --  Locates the /js directory for this application

   function CSS_Directory return String;
   --  Locates the /css director for this application

   function IMG_Directory return String;
   --  Locates the /img directory for this application

   function Upload_Directory return String;
   --  Locates the /upload directory for this application

   function Templates_Directory return String;
   --  Locates the templates directory for this application
   --  If not in Application_Directory/templates, tries
   --  Application_Directory/share/gnoga/templates, if not
   --  uses Application_Directory

end Ada_GUI.Gnoga.Server;

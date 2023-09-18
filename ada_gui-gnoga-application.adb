-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . A P P L I C A T I O N                    --
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
--
-- Changed by J. Carter 2021 to only run "singleton" applications
--                      2022 Improved OS detection

with GNAT.OS_Lib;

with Ada.Directories;
with Ada.Task_Identification;

with Ada_GUI.Gnoga.Server.Connection;

package body Ada_GUI.Gnoga.Application is
   procedure Set_Title (Name : in String);

   procedure Set_End_Text (Text : in String);

   procedure Open_URL (URL : in String);

   procedure Set_Favicon (Name : in String);

   App_Name : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("Ada GUI - An Ada-oriented GUI");

   HTML_For_On_Close : Ada.Strings.Unbounded.Unbounded_String;

   Favicon_URL : Ada.Strings.Unbounded.Unbounded_String;

   ----------------------
   -- Application_Name --
   ----------------------

   procedure Set_Title (Name : in String) is
   begin
      App_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Title;

   -------------------
   -- HTML_On_Close --
   -------------------

   procedure Set_End_Text (Text : String) is
   begin
      HTML_For_On_Close := Ada.Strings.Unbounded.To_Unbounded_String (Text);
   end Set_End_Text;

   function HTML_On_Close return String is
   begin
      return Ada.Strings.Unbounded.To_String (HTML_For_On_Close);
   end HTML_On_Close;

   --------------
   -- Open_URL --
   --------------

   Open_Mac     : constant String := "/usr/bin/open";
   Open_Unix    : constant String := "/usr/bin/xdg-open";
   Open_Windows : constant String := "cmd /c start";

   procedure Open_URL (URL : in String) is
      Args : GNAT.OS_Lib.Argument_List_Access;
      PID  : GNAT.OS_Lib.Process_Id;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List
         ( (if    Ada.Directories.Current_Directory (1) /= '/' then Open_Windows
            elsif Ada.Directories.Exists (Open_Unix)           then Open_Unix
            elsif Ada.Directories.Exists (Open_Mac)            then Open_Mac
            else raise Program_Error with "Operating system cannot be determined") &
          ' ' & URL);
      PID := GNAT.OS_Lib.Non_Blocking_Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last));
      Gnat.OS_Lib.Free (Arg => Args);
   end Open_URL;

   -------------------
   -- Favicon --
   -------------------

   procedure Set_Favicon (Name : in String) is
   begin
      Favicon_URL := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Favicon;

   function Favicon return String is
   begin
      return Ada.Strings.Unbounded.To_String (Favicon_URL);
   end Favicon;

   Connection_ID : Gnoga.Connection_ID := No_Connection;
   --  Set after Initialization

   Application_Holder : Gnoga.Server.Connection.Connection_Holder_Type;
   --  Used to block Initialize until On_Connect is called

   Connection_Holder : Gnoga.Server.Connection.Connection_Holder_Type;
   --  Used to hold the single incoming connection

   procedure On_Connect
     (ID         : in     Gnoga.Connection_ID;
      Connection : access Gnoga.Server.Connection.Connection_Holder_Type);
   --  Connection On_Connect handler

   ---------------------
   -- Web_Server_Task --
   ---------------------

   task type Web_Server_Task is
      entry Start;
   end Web_Server_Task;

   task body Web_Server_Task is
   begin
      accept Start;
      Gnoga.Server.Connection.Run;
   end Web_Server_Task;

   Web_Server : Web_Server_Task;

   ----------------
   -- On_Connect --
   ----------------

   procedure On_Connect
     (ID         : in     Gnoga.Connection_ID;
      Connection : access Gnoga.Server.Connection.Connection_Holder_Type)
   is
   begin
      if Connection_ID = No_Connection then
         Connection_ID := ID;

         Application_Holder.Release;

         Connection.Hold;

         Connection_Holder.Release;
         Gnoga.Gui.Event_Queue.Enqueue
            (New_Item => (Event => Ada.Strings.Unbounded.To_Unbounded_String (Closed_Text), others => <>) );

         Gnoga.Server.Connection.Stop;
      else
         Gnoga.Server.Connection.Execute_Script
           (ID, "document.writeln ('Only one connection permitted.');");
      end if;
   end On_Connect;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      ID          : in     Positive := 8080;
      Title       : in     String   := "Ada-GUI Application";
      Icon        : in     String   := "favicon.ico")
   is
   begin
      Set_Title (Name => Title);
      Set_End_Text (Text => Title & " ended");
      Set_Favicon (Name => Icon);
      Open_URL (URL => "http://127.0.0.1:" & Left_Trim (ID'Image) );
      Gnoga.Activate_Exception_Handler (ID => Ada.Task_Identification.Current_Task);
      Gnoga.Server.Connection.Initialize (Host => "localhost", Port => ID, Boot => "boot.html", Verbose => True);

      Gnoga.Write_To_Console (Message => "If closing the browser or browser tab does not end the program, press Ctrl-C");

      Gnoga.Server.Connection.On_Connect_Handler (Event => On_Connect'Access);

      Web_Server.Start;

      Application_Holder.Hold;

      Main_Window.Attach (Connection_ID => Connection_ID);
      Gnoga.Server.Connection.HTML_On_Close (ID => Connection_ID, HTML => HTML_On_Close);

      Main_Window.Document.Title (Value => Title);

      Log (Message => "Sending to single route from " & Gnoga.Server.Connection.Connection_Client_Address (Connection_ID) );
   end Initialize;

   ---------------------
   -- End_Application --
   ---------------------

   procedure End_Application is
   begin
      Gnoga.Server.Connection.Close (Connection_ID);
   end End_Application;
end Ada_GUI.Gnoga.Application;

-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . S E R V E R . C O N N E C I O N              --
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

with Ada.Strings.Unbounded;

with Ada_GUI.Gnoga.Gui;

package Ada_GUI.Gnoga.Server.Connection is

   procedure Initialize (Host    : in String  := "";
                         Port    : in Integer := 8080;
                         Boot    : in String  := "boot.html";
                         Verbose : in Boolean := True);
   --  Initialize connection to webserver and dispatchers
   --  If Host = "" then server will listen on all network interfaces.
   --  If Host = "localhost" use will be constrained to local machine only.
   --  If Verbose then display start up details.

   procedure Run;
   --  Start webserver.

   procedure Stop;
   --  Close all connections and Stop webserver

   function Shutting_Down return Boolean;
   --  If application is shutting down returns true

   procedure Execute_Script (ID     : in Gnoga.Connection_ID;
                             Script : in String);
   --  Execute Script on Connection ID

   function Execute_Script (ID     : in Gnoga.Connection_ID;
                            Script : in String)
                            return String;
   --  Execute Script on Connection ID and return result of script

   function Buffer_Connection (ID : Gnoga.Connection_ID) return Boolean;
   procedure Buffer_Connection (ID    : in Gnoga.Connection_ID;
                                Value : in Boolean);
   --  Buffering Property of connection with ID

   procedure Flush_Buffer (ID : in Gnoga.Connection_ID);
   --  Flush buffer of connection ID

   procedure Buffer_Append (ID    : in Gnoga.Connection_ID;
                            Value : in String);
   --  Append Value to output buffer. This can be used to output HTML directly
   --  on a Long_Polling connection.

   Script_Error : exception;

   protected type Connection_Holder_Type is
      entry Hold;
      procedure Release;
   private
      Connected : Boolean := True;
   end Connection_Holder_Type;
   --  This type is a binary semaphore starting in the seized stated.
   --  It is used to allow Connect_Event handlers to remain in memory
   --  until the web socket connection is closed.

   procedure Connection_Data
     (ID   : in     Gnoga.Connection_ID;
      Data : access Gnoga.Connection_Data_Type'Class);
   function Connection_Data
     (ID : in Gnoga.Connection_ID)
      return Gnoga.Pointer_to_Connection_Data_Class;
   --  Sets a connection specific Data object. Usually this is set with
   --  Gnoga.Gui.Windows.Connection_Data

   type Connect_Event is access
     procedure (ID         : in Gnoga.Connection_ID;
                Connection : access Connection_Holder_Type);

   procedure On_Connect_Handler (Event : in Connect_Event);
   --  Set event handler for new socket connections.

   type Gnoga_Connection_Type is (HTTP, Long_Polling, WebSocket, None);

   function Connection_Type (ID : Gnoga.Connection_ID)
                             return Gnoga_Connection_Type;
   --  Returns the connection type for ID
   --  The connection type may change during the life time of an ID as part
   --  of fall back mechanisms to support different network conditions and
   --  browsers. Returns none if ID is not valid.

   function Connection_Path (ID : Gnoga.Connection_ID)
                             return String;
   --  Returns the original connection path used to reach boot file, not the
   --  specific path used for ID (e.g. not the WebSocket URL). Returns "" if
   --  ID is invalid.

   function Connection_Client_Address (ID : Gnoga.Connection_ID)
                             return String;
   --  Returns the client address and port with the form nnn.nnn.nnn.nnn:ppppp
   --  Returns "" if ID is invalid.

   function Active_Connections return Natural;
   --  Returns the number of active connections

   type Post_Request_Event is access
     procedure
       (URI                 : in String;
        Accepted_Parameters : out Ada.Strings.Unbounded.Unbounded_String);

   procedure On_Post_Request_Handler (Event : Post_Request_Event);
   --  Event is called when a post request is received. Only those CGI
   --  parameters in the common separated Accepted_Parameters list will be
   --  parsed and sent to the On_Post Event.

   type Post_Event is access
     procedure (URI        : in String;
                Parameters : in out Gnoga.Data_Map_Type);

   procedure On_Post_Handler (Event : Post_Event);
   --  Called when a post has been received and parameters based on the
   --  On_Post_Request event have been parsed in to Parameters

   type Post_File_Event is access
     procedure (URI       : in String;
                File_Name : in String;
                Temp_Name : in String);

   procedure On_Post_File_Handler (Event : Post_File_Event);
   --  Called when a file is received from a post. Note the name of the CGI
   --  parameter for the file input must have been returned in the event
   --  On_Post_Request_Event and there must be a On_Post_File_Event set
   --  or the file will not be downloaded.

   function Form_Parameter (ID   : Gnoga.Connection_ID;
                            Name : String)
                            return String;
   --  Returns the value of parameters passed in on URL.
   --  Returns "undefined" if Name is not in URL query.
   --  For example: http://localhost:8080/?page_id=2
   --  Form_Parameter (ID, "page_id") = "2"

   function Valid (ID : Gnoga.Connection_ID) return Boolean;
   --  If ID is valid return true. Note that a broken web socket that has not
   --  been closed properly will have to time out first before reporting an
   --  error and Gnoga invalidating the ID.

   procedure Close (ID : in Gnoga.Connection_ID);
   --  Close connection ID

   procedure HTML_On_Close (ID   : in Gnoga.Connection_ID;
                            HTML : in String);
   --  On connection closed or lost HTML to display in browser.
   --  By default pages are left in the state they were in and an alter box
   --  announcing connection interruption is displayed.

   procedure New_Unique_ID (New_ID : out Gnoga.Unique_ID);
   --  Generates a new unique ID in to New_ID

   function New_GID return String;
   --  Generates unique ID for use in browser storage of elements

   procedure Add_To_Message_Queue
     (Object : in out Gnoga.Gui.Base_Type'Class);
   --  Add Object to Message Queue

   procedure Delete_From_Message_Queue
     (Object : in out Gnoga.Gui.Base_Type'Class);
   --  Delete an Object from Message Queue

   Connection_Error : exception;

end Ada_GUI.Gnoga.Server.Connection;

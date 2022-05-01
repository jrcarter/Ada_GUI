-- Ada_GUI implementation based on Gnoga. Adapted 2021
-- Cleaned up alternative icon name processing 2022
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . S E R V E R . C O N N E C I O N              --
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
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Ada.Exceptions;

with Ada.Containers.Ordered_Maps;

with Ada_GUI.Gnoga.Server.Mime;
with Ada_GUI.Gnoga.Application;

with Strings_Edit.Quoted;
with GNAT.Sockets.Server; use GNAT.Sockets.Server;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Server;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

with Ada_GUI.Gnoga.Server.Connection.Common; use Ada_GUI.Gnoga.Server.Connection.Common;
with Ada_GUI.Gnoga.Server.Template_Parser.Simple;

with Strings_Edit.UTF8.Handling;
with Strings_Edit.Streams;
with Ada.Streams;

package body Ada_GUI.Gnoga.Server.Connection is
   On_Connect_Event      : Connect_Event      := null;
   On_Post_Event         : Post_Event         := null;
   On_Post_Request_Event : Post_Request_Event := null;
   On_Post_File_Event    : Post_File_Event    := null;

   Exit_Application_Requested : Boolean := False;

   function Global_Gnoga_Client_Factory
     (Listener       : access Connections_Server'Class;
      Request_Length : Positive;
      Input_Size     : Buffer_Length;
      Output_Size    : Buffer_Length)
      return Connection_Ptr;
   --  Passed to Gnoga.Server.Connection.Common.Gnoga_Client_Factory
   --  This allows a common HTTP client for secure and insecure connections
   --  and when desired the secure libraries connection need not be linked in.

   -------------------------------------------------------------------------
   --  Private Types
   -------------------------------------------------------------------------

   protected type String_Buffer is
      procedure Buffering (Value : Boolean);
      function Buffering return Boolean;

      procedure Add (S : in String);
      --  Add to end of buffer

      procedure Preface (S : in String);
      --  Preface to buffer

      function Get return String;
      --  Retrieve buffer

      procedure Get_And_Clear (S : out Ada.Strings.Unbounded.Unbounded_String);
      --  Retrieve and clear buffer

      function Length return Natural;
      --  Size of buffer

      procedure Clear;
      --  Clear buffer
   private
      Is_Buffering : Boolean := False;
      Buffer       : Ada.Strings.Unbounded.Unbounded_String;
   end String_Buffer;

   task type Watchdog_Type is
      entry Start;
      entry Stop;
   end Watchdog_Type;

   type Watchdog_Access is access Watchdog_Type;

   Watchdog : Watchdog_Access := null;

   --  Keep alive and check connection status

   -------------------------------------------------------------------------
   --  HTTP Server Setup for Gnoga_HTTP_Server
   -------------------------------------------------------------------------

   --  Gnoga_HTTP_Content  --
   --  Per http connection data

   type Gnoga_HTTP_Client;
   type Socket_Type is access all Gnoga_HTTP_Client;

   type Gnoga_HTTP_Content is new Content_Source with
      record
         Socket          : Socket_Type           := null;
         Connection_Type : Gnoga_Connection_Type := HTTP;
         Connection_Path : Ada.Strings.Unbounded.Unbounded_String;
         FS              : Ada.Streams.Stream_IO.File_Type;
         Input_Overflow  : String_Buffer;
         Buffer          : String_Buffer;
         Finalized       : Boolean := False;
         Text            : aliased Strings_Edit.Streams.String_Stream (500);
      end record;

   overriding
   function Get (Source : access Gnoga_HTTP_Content) return String;
   --  Handle long polling method

   pragma Warnings (Off);
   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Item   : in     Gnoga_HTTP_Content);
   for Gnoga_HTTP_Content'Write use Write;
   pragma Warnings (On);

   --  Gnoga_HTTP_Factory  --
   --  Creates Gnoga_HTTP_Client objects on incoming connections
   --  from Gnoga_HTTP_Connection

   type Gnoga_HTTP_Factory (Request_Length  : Positive;
                            Input_Size      : Buffer_Length;
                            Output_Size     : Buffer_Length;
                            Max_Connections : Positive)
   is new Connections_Factory with null record;

   overriding
   function Create (Factory  : access Gnoga_HTTP_Factory;
                    Listener : access Connections_Server'Class;
                    From     : GNAT.Sockets.Sock_Addr_Type)
                    return Connection_Ptr;

   --  Gnoga_HTTP_Connection  --

   type Gnoga_HTTP_Connection is
     new GNAT.Sockets.Server.Connections_Server with null record;

   overriding
   function Get_Server_Address (Listener : Gnoga_HTTP_Connection)
                                return GNAT.Sockets.Sock_Addr_Type;
   --  Set the listening host if was set in Initialize

   overriding
   procedure Create_Socket
     (Listener : in out Gnoga_HTTP_Connection;
      Socket   : in out GNAT.Sockets.Socket_Type;
      Address  : GNAT.Sockets.Sock_Addr_Type);
   --  Create socket with exception handler

   --  Gnoga_HTTP_Client  --

   type Gnoga_HTTP_Client is new HTTP_Client with
      record
         Content : aliased Gnoga_HTTP_Content;
      end record;

   --  type Socket_Type is access all Gnoga_HTTP_Client;

   overriding
   procedure Finalize (Client : in out Gnoga_HTTP_Client);
   --  Handle browser crashes or webkit abrupt closes

   overriding
   function Get_Name (Client : Gnoga_HTTP_Client) return String;

   overriding
   procedure Do_Get  (Client : in out Gnoga_HTTP_Client);

   overriding
   procedure Do_Post (Client : in out Gnoga_HTTP_Client);

   overriding
   procedure Body_Received  (Client  : in out Gnoga_HTTP_Client;
                             Content : in out CGI_Keys.Table'Class);

   overriding
   procedure Body_Received  (Client  : in out Gnoga_HTTP_Client;
                             Content : in out Ada.Streams.Root_Stream_Type'Class);

   overriding
   procedure Do_Body (Client : in out Gnoga_HTTP_Client);

   overriding
   procedure Do_Head (Client : in out Gnoga_HTTP_Client);

   overriding
   function WebSocket_Open (Client : access Gnoga_HTTP_Client)
                            return WebSocket_Accept;

   overriding
   procedure WebSocket_Initialize (Client : in out Gnoga_HTTP_Client);

   overriding
   procedure WebSocket_Received_Part (Client  : in out Gnoga_HTTP_Client;
                                      Message : in     String);

   overriding
   procedure WebSocket_Received (Client  : in out Gnoga_HTTP_Client;
                                 Message : in     String);

   overriding
   procedure WebSocket_Closed (Client  : in out Gnoga_HTTP_Client;
                               Status  : in     WebSocket_Status;
                               Message : in     String);

   overriding
   procedure WebSocket_Error
     (Client : in out Gnoga_HTTP_Client;
      Error  : in     Ada.Exceptions.Exception_Occurrence);

   -------------------------------------------------------------------------
   --  Connection Helpers
   -------------------------------------------------------------------------

   pragma Warnings (Off);
   procedure Start_Long_Polling_Connect
     (Client : in out Gnoga_HTTP_Client;
      ID     : out    Gnoga.Connection_ID);
   --  Start a long polling connection alternative to websocket
   pragma Warnings (On);

   function Buffer_Add (ID     : Gnoga.Connection_ID;
                        Script : String)
                        return Boolean;
   --  If buffering add Script to the buffer for ID and return true, if not
   --  buffering return false;

   procedure Dispatch_Message (Message : in String);
   --  Dispatch an incoming message from browser to event system

   -----------------------
   -- Gnoga_HTTP_Server --
   -----------------------

   Server_Wait : Connection_Holder_Type;

   task type Gnoga_HTTP_Server_Type is
      entry Start;
      entry Stop;
   end Gnoga_HTTP_Server_Type;

   type Gnoga_HTTP_Server_Access is access Gnoga_HTTP_Server_Type;

   Gnoga_HTTP_Server : Gnoga_HTTP_Server_Access := null;

   task body Gnoga_HTTP_Server_Type is
   begin
      accept Start;

      declare
         Factory : aliased Gnoga_HTTP_Factory
           (Request_Length  => Max_HTTP_Request_Length,
            Input_Size      => Max_HTTP_Input_Chunk,
            Output_Size     => Max_HTTP_Output_Chunk,
            Max_Connections => Max_HTTP_Connections);
      begin
         if Verbose_Output then
            Gnoga.Log ("HTTP Server Started");
            --  Trace_On (Factory  => Factory,
            --            Received => Trace_Any,
            --            Sent     => Trace_Any);
         end if;

         if not Secure_Server then
            declare
               Server : Gnoga_HTTP_Connection (Factory'Access,  Server_Port);
               pragma Unreferenced (Server);
            begin
               accept Stop;
            end;
         else
            if not Secure_Only then
               declare
                  Server1 : Gnoga_HTTP_Connection
                    (Factory'Access,  Server_Port);
                  pragma Unreferenced (Server1);
                  Server2 : Gnoga_HTTP_Connection
                    (Gnoga.Server.Connection.Common.Gnoga_Secure_Factory.all,
                     Secure_Port);
                  pragma Unreferenced (Server2);
               begin
                  accept Stop;
               end;
            else
               declare
                  Server : Gnoga_HTTP_Connection
                    (Gnoga.Server.Connection.Common.Gnoga_Secure_Factory.all,
                     Secure_Port);
               pragma Unreferenced (Server);
               begin
                  accept Stop;
               end;
            end if;
         end if;

         Server_Wait.Release;

         if Verbose_Output then
            Gnoga.Log ("HTTP Server Stopping");
         end if;
      end;
   end Gnoga_HTTP_Server_Type;

   ------------
   -- Create --
   ------------

   function Global_Gnoga_Client_Factory
     (Listener       : access Connections_Server'Class;
      Request_Length : Positive;
      Input_Size     : Buffer_Length;
      Output_Size    : Buffer_Length)
      return Connection_Ptr
   is
      Socket : constant Socket_Type := new Gnoga_HTTP_Client
        (Listener       => Listener.all'Unchecked_Access,
         Request_Length => Request_Length,
         Input_Size     => Input_Size,
         Output_Size    => Output_Size);
   begin
      Socket.Content.Socket := Socket;

      return Connection_Ptr (Socket);
   end Global_Gnoga_Client_Factory;

   overriding
   function Create (Factory  : access Gnoga_HTTP_Factory;
                    Listener : access Connections_Server'Class;
                    From     : GNAT.Sockets.Sock_Addr_Type)
                    return Connection_Ptr
   is
      pragma Unreferenced (From);
   begin
      return Gnoga.Server.Connection.Common.Gnoga_Client_Factory
        (Listener       => Listener.all'Unchecked_Access,
         Request_Length => Factory.Request_Length,
         Input_Size     => Factory.Input_Size,
         Output_Size    => Factory.Output_Size);
   end Create;

   -------------------------
   --  Get_Server_Address --
   -------------------------

   overriding
   function Get_Server_Address (Listener : Gnoga_HTTP_Connection)
                                return GNAT.Sockets.Sock_Addr_Type
   is
      use GNAT.Sockets;
      use type Ada.Strings.Unbounded.Unbounded_String;

      Address : Sock_Addr_Type;
      Host    : constant String :=
        (if Server_Host = "localhost" then "127.0.0.1" else Ada.Strings.Unbounded.To_String (Server_Host));
   begin
      if Host = "" then
         Address.Addr := Any_Inet_Addr;
      else
         Address.Addr := Inet_Addr (Host);
      end if;

      Address.Port := Listener.Port;

      return Address;
   end Get_Server_Address;

   --------------------
   --  Create_Socket --
   --------------------

   overriding
   procedure Create_Socket
     (Listener : in out Gnoga_HTTP_Connection;
      Socket   : in out GNAT.Sockets.Socket_Type;
      Address  : GNAT.Sockets.Sock_Addr_Type)
   is
      use type GNAT.Sockets.Socket_Type;
   begin
      Create_Socket (Connections_Server (Listener), Socket, Address);
   exception
      when Error : others =>
         Gnoga.Log (Error);
         if Socket /= GNAT.Sockets.No_Socket then
            begin
               GNAT.Sockets.Shutdown_Socket (Socket);
            exception
               when others =>
                  null;
            end;
            begin
               GNAT.Sockets.Close_Socket (Socket);
            exception
               when others =>
                  null;
            end;
            Socket := GNAT.Sockets.No_Socket;
         end if;
         Stop;
   end Create_Socket;

   --------------
   -- Get_Name --
   --------------

   overriding
   function Get_Name (Client : Gnoga_HTTP_Client) return String is
      pragma Unreferenced (Client);
   begin
      return Gnoga.HTTP_Server_Name;
   end Get_Name;

   -----------------
   -- Do_Get_Head --
   -----------------

   procedure Do_Get_Head (Client : in out Gnoga_HTTP_Client;
                          Get    : in     Boolean);

   procedure Do_Get_Head (Client : in out Gnoga_HTTP_Client;
                          Get    : in     Boolean)
   is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;

      use Strings_Edit.Quoted;

      Status : Status_Line renames Get_Status_Line (Client);

      function Adjust_Name return String;

      function Adjust_Name return String is
         function Start_Path return String;
         function After_Start_Path return String;

         File_Name : constant String := Status.File;

         function Start_Path return String is
            Q : constant Integer := Index (File_Name, "/");
         begin
            if Q = 0 then
               return "";
            else
               return File_Name (File_Name'First .. Q - 1);
            end if;
         end Start_Path;

         function After_Start_Path return String is
            Q : constant Integer := Index (File_Name, "/");
         begin
            if Q = 0 then
               return File_Name;
            else
               return File_Name (Q + 1 .. File_Name'Last);
            end if;
         end After_Start_Path;

         Start              : constant String := Start_Path;
         Path_Adjusted_Name : constant String := After_Start_Path;
      begin
         if File_Name = "gnoga_ajax" then
            return File_Name;
         elsif Start = "" and File_Name = "" then
            return Gnoga.Server.HTML_Directory & To_String (Boot_HTML);
         elsif Start = "js" then
            return Gnoga.Server.JS_Directory & Path_Adjusted_Name;
         elsif Start = "css" then
            return Gnoga.Server.CSS_Directory & Path_Adjusted_Name;
         elsif Start = "img" then
            return Gnoga.Server.IMG_Directory & Path_Adjusted_Name;
         else
            if Ada.Directories.Exists
              (Gnoga.Server.HTML_Directory & File_Name)
            then
               return Gnoga.Server.HTML_Directory & File_Name;
            else
               return Gnoga.Server.HTML_Directory & To_String (Boot_HTML);
            end if;
         end if;
      end Adjust_Name;

   begin
      case Status.Kind is
         when None =>
            if Verbose_Output then
               Gnoga.Log ("Requested: Kind: " & Status.Kind'Img & ", Query: " & Status.Query);
               Gnoga.Log ("Reply: Not found");
            end if;

            Reply_Text (Client, 404, "Not found", "Not found");
         when File =>
            if Verbose_Output then
               Gnoga.Log ("Requested: Kind: " & Status.Kind'Img & ", File: " & Status.File
                          & ", Query: " & Status.Query);
            end if;
            Client.Content.Connection_Path :=
              To_Unbounded_String (Status.File);

            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send (Client,
                  "Cache-Control: no-cache, no-store, must-revalidate" &
                    Gnoga.Server.Connection.Common.CRLF);
            Send (Client, "Pragma: no-cache" &
                    Gnoga.Server.Connection.Common.CRLF);
            Send (Client, "Expires: 0" &
                    Gnoga.Server.Connection.Common.CRLF);
            Send_Connection (Client, Persistent => True);
            Send_Server (Client);

            declare
               F : constant String := Adjust_Name;
               M : constant String := Gnoga.Server.Mime.Mime_Type (F);
            begin
               if F = "gnoga_ajax" then
                  Send_Body (Client, "", Get);

                  declare
                     MH      : constant String := "?m=";
                     Q       : constant Integer := Index
                       (Status.Query, MH, Going => Forward);
                     Message : constant String := Status.Query
                       (Q + MH'Length .. Status.Query'Last);
                  begin
                     Dispatch_Message (Message);
                  end;
               else
                  if M = "text/html" then
                     Client.Content.Finalized := False;

                     declare
                        Favicon_Name : constant String := "favicon.ico";

                        ID : Gnoga.Connection_ID;
                        F  : Unbounded_String := To_Unbounded_String (Gnoga.Server.Template_Parser.Simple.Load_View (Adjust_Name) );
                     begin
                        if Gnoga.Application.Favicon /= "" and
                           Index (F, "<meta name=""generator"" content=""Gnoga"" />") > 0 and
                           Index (F, Favicon_Name) > 0
                        then
                           String_Replace (Source      => F,
                                           Pattern     => Favicon_Name,
                                           Replacement => Gnoga.Application.Favicon);
                        end if;
                        if Index (F, "/js/ajax.js") > 0 then
                           Client.Content.Connection_Type := Long_Polling;
                           Client.Content.Buffer.Add (To_String (F));

                           Send_Body (Client, Client.Content'Access, Get);

                           Start_Long_Polling_Connect (Client, ID);
                        elsif Index (F, "/js/auto.js") > 0 then
                           Client.Content.Connection_Type := Long_Polling;
                           Start_Long_Polling_Connect (Client, ID);

                           String_Replace (Source      => F,
                                           Pattern     => "@@Connection_ID@@",
                                           Replacement => ID'Img);
                           Client.Content.Buffer.Add (To_String (F));

                           Send_Body (Client, Client.Content'Access, Get);
                        else
                           Client.Content.Connection_Type := HTTP;
                           Client.Content.Buffer.Add (To_String (F));

                           Send_Body (Client, Client.Content'Access, Get);
                        end if;
                     end;
                  else
                     Send_Content_Type (Client, M);
                     declare
                        use Ada.Streams.Stream_IO;
                     begin
                        if Is_Open (Client.Content.FS) then
                           Close (Client.Content.FS);
                        end if;

                        Open (Client.Content.FS, In_File, F,
                              Form => "shared=no");
                        Send_Body (Client,
                                   Stream (Client.Content.FS),
                                   Get);
                     end;
                  end if;
               end if;
               if Verbose_Output then
                  Gnoga.Log ("Reply: " & F & " (" & M & ')');
               end if;
            exception
               when Ada.Text_IO.Name_Error =>
                  if Verbose_Output then
                     Gnoga.Log ("Reply: Not found");
                  end if;
                  Reply_Text (Client,
                              404,
                              "Not found",
                              "No file " & Quote (Status.File) & " found");
            end;

         when URI =>
            if Verbose_Output then
               Gnoga.Log ("Requested: Kind: " & Status.Kind'Img & ", Path: " & Status.Path
                          & ", Query: " & Status.Query);
               Gnoga.Log ("Reply: Not found");
            end if;

            Reply_Text (Client,
                        404,
                        "Not found",
                        "No URI " & Quote (Status.Path) & " found");
      end case;
   exception
      when E : others =>
         Log ("Do_Get_Head Error");
         Log (Ada.Exceptions.Exception_Information (E));
   end Do_Get_Head;

   ------------
   -- Do_Get --
   ------------

   overriding
   procedure Do_Get  (Client : in out Gnoga_HTTP_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding
   procedure Do_Post (Client : in out Gnoga_HTTP_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Post;

   -------------------
   -- Body_Received --
   -------------------

   overriding
   procedure Body_Received  (Client  : in out Gnoga_HTTP_Client;
                             Content : in out CGI_Keys.Table'Class)
   is
      pragma Unreferenced (Content);
      Status : Status_Line renames Get_Status_Line (Client);

      Parameters : Gnoga.Data_Map_Type;
   begin
      if On_Post_Event /= null and Status.Kind = File then
         for i in 1 .. Client.Get_CGI_Size loop
            Parameters.Insert (Strings_Edit.UTF8.Handling.To_String (Client.Get_CGI_Key (i),
                                 Substitution_Character),
                               Strings_Edit.UTF8.Handling.To_String (Client.Get_CGI_Value (i),
                                 Substitution_Character));
         end loop;

         On_Post_Event (Status.File & Status.Query, Parameters);
      end if;
   end Body_Received;

   -------------------
   -- Body_Received --
   -------------------

   overriding
   procedure Body_Received  (Client  : in out Gnoga_HTTP_Client;
                             Content : in out Ada.Streams.Root_Stream_Type'Class)
   is
      pragma Unreferenced (Content);
      Status       : Status_Line renames Get_Status_Line (Client);
      Disposition  : constant String := Client.Get_Multipart_Header (Content_Disposition_Header);
      Field_ID     : constant String := "name=""";
      n            : constant Natural := Ada.Strings.Fixed.Index (Disposition, Field_ID);
      Eq           : constant Natural := Ada.Strings.Fixed.Index (Disposition, """", n + Field_ID'Length);
      Field_Name   : constant String := Disposition (n + Field_ID'Length .. Eq - 1);
      Content_Type : constant String :=
        Client.Get_Multipart_Header (Content_Type_Header);

      Parameters : Gnoga.Data_Map_Type;
   begin
      if On_Post_Event /= null and Status.Kind = File and Content_Type = "" then
         Parameters.Insert (Strings_Edit.UTF8.Handling.To_String (Field_Name, Substitution_Character),
                            Strings_Edit.UTF8.Handling.To_String (Client.Content.Text.Get, Substitution_Character));
         On_Post_Event (Status.File & Status.Query, Parameters);
      end if;
   end Body_Received;

   -------------
   -- Do_Post --
   -------------

   overriding
   procedure Do_Body (Client : in out Gnoga_HTTP_Client) is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use Ada.Streams.Stream_IO;

      Status : Status_Line renames Get_Status_Line (Client);

      Param_List : Unbounded_String;

      Content_Type : constant String :=
        Client.Get_Header (Content_Type_Header);
      Disposition  : constant String := Client.Get_Multipart_Header
        (Content_Disposition_Header);
   begin
      --  Gnoga.Log ("Content_Type: " & Content_Type & ", Disposition: " & Disposition);
      if On_Post_Request_Event /= null then
         On_Post_Request_Event (Status.File & Status.Query, Param_List);
      end if;

      if Content_Type = "application/x-www-form-urlencoded" then
         Client.Receive_Body (Strings_Edit.UTF8.Handling.To_UTF8 (To_String (Param_List)));
      end if;

      if Index (Content_Type, "multipart/form-data") = Content_Type'First then
         if Index (Disposition, "form-data") = Disposition'First  then
            declare
               Field_ID : constant String := "name=""";
               File_ID  : constant String := "filename=""";

               n : constant Natural := Index (Disposition, Field_ID);
               f : constant Natural := Index (Disposition, File_ID);
            begin
               if n /= 0 then
                  declare
                     Eq : constant Natural := Index
                       (Disposition, """", n + Field_ID'Length);
                     Field_Name : constant String := Disposition
                       (n + Field_ID'Length .. Eq - 1);
                  begin
                     if Index (Strings_Edit.UTF8.Handling.To_UTF8 (To_String (Param_List)), Field_Name) > 0 then
                        if f /= 0 then
                           declare
                              Eq : constant Natural := Index
                                (Disposition, """", f + File_ID'Length);
                              File_Name : constant String := Disposition
                                (f + File_ID'Length .. Eq - 1);
                           begin
                              if On_Post_File_Event = null then
                                 Gnoga.Log ("Attempt to upload file without" &
                                              " an On_Post_File_Event set");
                              else
                                 if Is_Open (Client.Content.FS) then
                                    Close (Client.Content.FS);
                                 end if;

                                 Create (Client.Content.FS,
                                         Out_File,
                                         Gnoga.Server.Upload_Directory &
                                           File_Name & ".tmp",
                                         "Text_Translation=No");

                                 Receive_Body
                                   (Client, Stream (Client.Content.FS));

                                 On_Post_File_Event (Status.File & Status.Query,
                                                     Strings_Edit.UTF8.Handling.To_String
                                                       (File_Name, Substitution_Character),
                                                     Strings_Edit.UTF8.Handling.To_String
                                                       (File_Name, Substitution_Character) & ".tmp");
                              end if;
                           end;
                        else
                           Client.Content.Text.Rewind;
                           Client.Receive_Body (Client.Content.Text'Access);
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;
      end if;
   exception
      when E : others =>
         Log ("Do_Body Error");
         Log (Ada.Exceptions.Exception_Information (E));
   end Do_Body;

   -------------
   -- Do_Head --
   -------------

   overriding
   procedure Do_Head (Client : in out Gnoga_HTTP_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   -------------------------------------------------------------------------
   --  Gnoga Server Connection Methods
   -------------------------------------------------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Host    : in String  := "";
                         Port    : in Integer := 8080;
                         Boot    : in String  := "boot.html";
                         Verbose : in Boolean := True)
   is
   begin
      Verbose_Output := Verbose;

      Boot_HTML   := Ada.Strings.Unbounded.To_Unbounded_String (Boot);
      Server_Port := GNAT.Sockets.Port_Type (Port);
      Server_Host := Ada.Strings.Unbounded.To_Unbounded_String (Host);

      if Verbose then
         Write_To_Console ("Ada GUI          :" & Gnoga.Version);
         Write_To_Console ("Application root :" & Application_Directory);
         Write_To_Console ("Executable at    :" & Executable_Directory);
         Write_To_Console ("HTML root        :" & HTML_Directory);
         Write_To_Console ("Upload directory :" & Upload_Directory);
         Write_To_Console ("Templates root   :" & Templates_Directory);
         Write_To_Console ("/js  at          :" & JS_Directory);
         Write_To_Console ("/css at          :" & CSS_Directory);
         Write_To_Console ("/img at          :" & IMG_Directory);

         if not Secure_Only then
            Write_To_Console ("Boot file        :" & Boot);
            Write_To_Console ("HTTP listen on   :" & Host & ":" &
                                Left_Trim (Server_Port'Img));
         end if;

         if Secure_Server then
            Write_To_Console ("HTTPS listen on  :" & Host & ":" &
                                Left_Trim (Secure_Port'Img));
         end if;
      end if;

      Watchdog := new Watchdog_Type;
      Watchdog.Start;
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Gnoga_HTTP_Server := new Gnoga_HTTP_Server_Type;
      Gnoga_HTTP_Server.Start;

      Server_Wait.Hold;

      Exit_Application_Requested := True;
   end Run;

   -------------------
   -- Shutting_Down --
   -------------------

   function Shutting_Down return Boolean is
   begin
      return Exit_Application_Requested;
   end Shutting_Down;

   ----------------------------
   -- Connection_Holder_Type --
   ----------------------------

   protected body Connection_Holder_Type is
      entry Hold when not Connected is
      begin
         null;
         --  Semaphore does not reset itself to a blocking state.
         --  This ensures that if Released before Hold that Hold
         --  will not block and connection will be released.
         --  It also allows for On_Connect Handler to not have to use
         --  Connection.Hold unless there is a desire code such as to
         --  clean up after a connection is ended.
      end Hold;

      procedure Release is
      begin
         Connected := False;
      end Release;
   end Connection_Holder_Type;

   type Connection_Holder_Access is access all Connection_Holder_Type;

   package Connection_Holder_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Unique_ID, Connection_Holder_Access);

   package Connection_Data_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Unique_ID, Gnoga.Pointer_to_Connection_Data_Class);

   ---------------------
   -- Event_Task_Type --
   ---------------------

   task type Event_Task_Type (TID : Gnoga.Connection_ID);

   type Event_Task_Access is access all Event_Task_Type;

   procedure Free_Event_Task is
        new Ada.Unchecked_Deallocation (Event_Task_Type,
                                        Event_Task_Access);

   package Event_Task_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Unique_ID, Event_Task_Access);

   ------------------------
   -- Connection Manager --
   ------------------------

   package Socket_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Connection_ID, Socket_Type);
   --  Socket Maps are used for the Connection Manager to map connection IDs
   --  to web sockets.

   protected Connection_Manager is
      procedure Add_Connection (Socket : in  Socket_Type;
                                New_ID : out Gnoga.Connection_ID);
      --  Adds Socket to managed Connections and generates a New_ID.

      procedure Start_Connection (New_ID : in Gnoga.Connection_ID);
      --  Start event task on connection

      procedure Swap_Connection (New_ID : in Gnoga.Connection_ID;
                                 Old_ID : in Gnoga.Connection_ID);
      --  Reconnect old connection

      procedure Add_Connection_Holder (ID     : in Gnoga.Connection_ID;
                                       Holder : in Connection_Holder_Access);
      --  Adds a connection holder to the connection
      --  Can only be one at any given time.

      procedure Add_Connection_Data
        (ID   : in Gnoga.Connection_ID;
         Data : in Gnoga.Pointer_to_Connection_Data_Class);
      --  Adds data to be associated with connection

      function Connection_Data
        (ID : in Gnoga.Connection_ID)
         return Gnoga.Pointer_to_Connection_Data_Class;
      --  Returns the Connection_Data associated with ID

      procedure Delete_Connection_Holder (ID : in Gnoga.Connection_ID);
      --  Delete connection holder

      procedure Delete_Connection (ID : in Gnoga.Connection_ID);
      --  Delete Connection with ID.
      --  Releases connection holder if present.

      procedure Finalize_Connection (ID : in Gnoga.Connection_ID);
      --  Mark Connection with ID for deletion.

      function Valid (ID : in Gnoga.Connection_ID) return Boolean;
      --  Return True if ID is in connection map.

      procedure First (ID : out Gnoga.Connection_ID);
      --  Return first ID if ID is in connection map else 0.

      procedure Next (ID : out Gnoga.Connection_ID);
      --  Return next ID if ID is in connection map else 0.

      function Connection_Socket (ID : in Gnoga.Connection_ID)
                                  return Socket_Type;
      --  Return the Socket_Type associated with ID
      --  Raises Connection_Error if ID is not Valid

      function Find_Connection_ID (Socket : Socket_Type)
                                  return Gnoga.Connection_ID;
      --  Find the Connection_ID related to Socket.

      procedure Delete_All_Connections;
      --  Called by Stop to close down server

      function Active_Connections return Ada.Containers.Count_Type;
      --  Returns the number of active connections
   private
      Socket_Count          : Gnoga.Connection_ID := 0;
      Connection_Holder_Map : Connection_Holder_Maps.Map;
      Connection_Data_Map   : Connection_Data_Maps.Map;
      Event_Task_Map        : Event_Task_Maps.Map;
      Socket_Map            : Socket_Maps.Map;
      Shadow_Socket_Map     : Socket_Maps.Map;
      Current_Socket        : Socket_Maps.Cursor := Socket_Maps.No_Element;
   end Connection_Manager;

   protected body Connection_Manager is
      procedure Add_Connection (Socket : in  Socket_Type;
                                New_ID : out Gnoga.Connection_ID)
      is
      begin
         Socket_Count := Socket_Count + 1;
         New_ID := Socket_Count;
         Socket_Map.Insert (New_ID, Socket);
      end Add_Connection;

      procedure Start_Connection (New_ID : in Gnoga.Connection_ID)
      is
      begin
         Event_Task_Map.Insert (New_ID, new Event_Task_Type (New_ID));
      end Start_Connection;

      procedure Swap_Connection (New_ID : in Gnoga.Connection_ID;
                                 Old_ID : in Gnoga.Connection_ID)
      is
      begin
         if Socket_Map.Contains (Old_ID) then
            declare
               Old_Socket : constant Socket_Type :=
                 Socket_Map.Element (Old_ID);
               New_Socket : constant Socket_Type :=
                 Socket_Map.Element (New_ID);
            begin
               New_Socket.Content.Connection_Path :=
                 Old_Socket.Content.Connection_Path;
               Socket_Map.Replace (Old_ID, New_Socket);
               Socket_Map.Replace (New_ID, Old_Socket);
               Old_Socket.Content.Finalized := True;
            end;
         else
            raise Connection_Error with
              "Old connection " & Old_ID'Img & " already gone";
         end if;
      end Swap_Connection;

      procedure Add_Connection_Holder (ID     : in Gnoga.Connection_ID;
                                       Holder : in Connection_Holder_Access)
      is
      begin
         Connection_Holder_Map.Insert (ID, Holder);
      end Add_Connection_Holder;

      procedure Delete_Connection_Holder (ID : in Gnoga.Connection_ID)
      is
      begin
         if Connection_Holder_Map.Contains (ID) then
            Connection_Holder_Map.Delete (ID);
         end if;
      end Delete_Connection_Holder;

      procedure Add_Connection_Data
        (ID   : in Gnoga.Connection_ID;
         Data : in Gnoga.Pointer_to_Connection_Data_Class)
      is
      begin
         Connection_Data_Map.Include (ID, Data);
      end Add_Connection_Data;

      function Connection_Data
        (ID : in Gnoga.Connection_ID)
         return Gnoga.Pointer_to_Connection_Data_Class
      is
      begin
         if Connection_Data_Map.Contains (ID) then
            return Connection_Data_Map.Element (ID);
         else
            return null;
         end if;
      end Connection_Data;

      procedure Delete_Connection (ID : in Gnoga.Connection_ID) is
      begin
         if (ID > 0) then
            Gnoga.Log ("Deleting connection -" & ID'Img);

            if Connection_Holder_Map.Contains (ID) then
               Connection_Holder_Map.Element (ID).Release;
               Connection_Holder_Map.Delete (ID);
            end if;

            if Connection_Data_Map.Contains (ID) then
               Connection_Data_Map.Delete (ID);
            end if;

            if Socket_Map.Contains (ID) then
               Socket_Map.Delete (ID);
            end if;

            if Event_Task_Map.Contains (ID) then
               declare
                  E  : Event_Task_Access := Event_Task_Map.Element (ID);
               begin
                  Free_Event_Task (E);
                  Event_Task_Map.Delete (ID);
               end;
            end if;
         end if;
      exception
         when E : others =>
         Log ("Delete_Connection " & ID'Img & " error.");
         Log (Ada.Exceptions.Exception_Information (E));
      end Delete_Connection;

      procedure Finalize_Connection (ID : in Gnoga.Connection_ID) is
      begin
         if (ID > 0) and Socket_Map.Contains (ID) then
            if Verbose_Output then
               Gnoga.Log ("Finalizing connection -" & ID'Img);
            end if;

            Socket_Map.Element (ID).Content.Finalized := True;
         end if;
      end Finalize_Connection;

      function Valid (ID : in Gnoga.Connection_ID) return Boolean is
      begin
         return Socket_Map.Contains (ID);
      end Valid;

      procedure First (ID : out Gnoga.Connection_ID) is
         use type Socket_Maps.Cursor;
      begin
         Shadow_Socket_Map := Socket_Map;
         Current_Socket := Shadow_Socket_Map.First;
         if Current_Socket /= Socket_Maps.No_Element then
            ID := Socket_Maps.Key (Current_Socket);
         else
            ID := 0;
         end if;
      end First;

      procedure Next (ID : out Gnoga.Connection_ID) is
         use type Socket_Maps.Cursor;
      begin
         Current_Socket := Socket_Maps.Next (Current_Socket);
         if Current_Socket /= Socket_Maps.No_Element then
            ID := Socket_Maps.Key (Current_Socket);
         else
            ID := 0;
         end if;
      end Next;

      function Connection_Socket (ID : in Gnoga.Connection_ID)
                                  return Socket_Type
      is
         use type Socket_Maps.Cursor;
         Connection_Cursor : constant Socket_Maps.Cursor := Socket_Map.Find (ID);
      begin
         if Connection_Cursor = Socket_Maps.No_Element then
            Log ("Error Connection_Socket - " & ID'Img & " not found in connection map. ");
            raise Connection_Error with
              "Connection ID" & ID'Img & " not found in connection map. " &
              "Connection most likely was previously closed.";
         else
            return Socket_Maps.Element (Connection_Cursor);
         end if;
      end Connection_Socket;

      function Find_Connection_ID (Socket : Socket_Type)
                               return Gnoga.Connection_ID
      is
         use type Socket_Maps.Cursor;

         Cursor : Socket_Maps.Cursor := Socket_Map.First;
      begin
         while Cursor /= Socket_Maps.No_Element loop
            if Socket_Maps.Element (Cursor) = Socket then
               return Socket_Maps.Key (Cursor);
            else
               Socket_Maps.Next (Cursor);
            end if;
         end loop;

         return Gnoga.No_Connection;
      end Find_Connection_ID;

      procedure Delete_All_Connections is
         procedure Do_Delete (C : in Socket_Maps.Cursor);

         procedure Do_Delete (C : in Socket_Maps.Cursor) is
         begin
            Delete_Connection (Socket_Maps.Key (C));
         end Do_Delete;
      begin
         --  Socket_Map.Iterate (Do_Delete'Access); --  provoque PROGRAM_ERROR
         --  Message: Gnoga.Server.Connection.Socket_Maps.Tree_Operations.
         --    Delete_Node_Sans_Free: attempt to tamper with cursors
         --    (container is busy)
         while not Socket_Map.Is_Empty loop
            Do_Delete (Socket_Map.First);
         end loop;
      end Delete_All_Connections;

      function Active_Connections return Ada.Containers.Count_Type is
      begin
         return Socket_Map.Length;
      end Active_Connections;
   end Connection_Manager;

   task body Event_Task_Type is
      Connection_Holder : aliased Connection_Holder_Type;
      ID : Gnoga.Connection_ID;
   begin
      ID := TID;
      --  Insure that TID is retained even if task is "deleted"

      Connection_Manager.Add_Connection_Holder
        (ID, Connection_Holder'Unchecked_Access);

      begin
         delay 0.3;
         --  Give time to finish handshaking

         Execute_Script (ID, "gnoga['Connection_ID']=" & ID'Img);

         Execute_Script (ID, "TRUE=true");
         Execute_Script (ID, "FALSE=false");
         --  By setting the variable TRUE and FALSE it is possible to set
         --  a property or attribute with Boolean'Img which will result
         --  in TRUE or FALSE not the case sensitive true or false
         --  expected.

         On_Connect_Event (ID, Connection_Holder'Unchecked_Access);
      exception
         when E : Connection_Error =>
            --  Browser was closed by user
            Log ("Error browser was closed by user -" & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            Connection_Holder.Release;
         when E : others =>
            Connection_Holder.Release;

            Log ("Error on Connection ID =" & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
      end;

      Connection_Manager.Delete_Connection_Holder (ID);
      Connection_Manager.Finalize_Connection (ID);
      --  Insure cleanup even if socket not closed by external connection
   exception
      when E : others =>
         Log ("Connection Manager Error Connection ID =" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
   end Event_Task_Type;

   --------------
   -- Watchdog --
   --------------

   task body Watchdog_Type is
      procedure Ping (ID : in Gnoga.Connection_ID);

      procedure Ping (ID : in Gnoga.Connection_ID) is
         Socket : Socket_Type := Connection_Manager.Connection_Socket (ID);
      begin
         if Socket.Content.Finalized then
            if Verbose_Output then
               Gnoga.Log ("Ping on Finalized -" & ID'Img);
            end if;
            Connection_Manager.Delete_Connection (ID);
            Socket.Shutdown;
         elsif Socket.Content.Connection_Type = Long_Polling then
            if Verbose_Output then
               Gnoga.Log ("Ping on long polling -" & ID'Img);
            end if;

            Execute_Script (ID, "0");

         elsif Socket.Content.Connection_Type = WebSocket then
            if Verbose_Output then
               Gnoga.Log ("Ping on websocket -" & ID'Img);
            end if;

            Socket.WebSocket_Send ("0");
         end if;
      exception
         when E : Storage_Error =>
            Gnoga.Log ("Invalid socket, Deleting ID -" & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            Connection_Manager.Delete_Connection (ID);
         when E : others =>
            Log ("Ping" & ID'Img & " error.");
            Log (Ada.Exceptions.Exception_Information (E));
            if Socket.Content.Connection_Type = Long_Polling then
               Gnoga.Log ("Long polling error closing ID " & ID'Img);
               Socket.Content.Finalized := True;
               Socket.Shutdown;
            else
               begin
                  delay 3.0;
                  Socket := Connection_Manager.Connection_Socket (ID);
                  Socket.WebSocket_Send ("0");
               exception
                  when E : others =>
                     Log ("Watchdog closed connection ID " & ID'Img);
                     Log (Ada.Exceptions.Exception_Information (E));

                     begin
                        Connection_Manager.Delete_Connection (ID);
                     exception
                        when E : others =>
                           Log ("Watchdog ping error - " & ID'Img);
                           Log (Ada.Exceptions.Exception_Information (E));
                     end;
               end;
            end if;
      end Ping;
   begin
      accept Start;

      loop
         declare
            ID : Gnoga.Connection_ID;
         begin
            Connection_Manager.First (ID);
            while ID /= 0 loop
               Ping (ID);
               Connection_Manager.Next (ID);
            end loop;
         exception
            when E : others =>
               Log ("Watchdog error on websocket - " & ID'Img);
               Log (Ada.Exceptions.Exception_Information (E));
         end;

         select
            accept Stop;
            exit;
         or
            delay 60.0;
         end select;
      end loop;
   end Watchdog_Type;

   ---------------------------
   -- Message Queue Manager --
   ---------------------------

   No_Object : exception;

   function "=" (Left, Right : Gnoga.Gui.Pointer_To_Base_Class)
                 return Boolean;
   --  Properly identify equivalent objects

   function "=" (Left, Right : Gnoga.Gui.Pointer_To_Base_Class)
                 return Boolean
   is
   begin
      return Left.Unique_ID = Right.Unique_ID;
   end "=";

   package Object_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Unique_ID, Gnoga.Gui.Pointer_To_Base_Class);

   protected Object_Manager is
      function Get_Object (ID : Gnoga.Unique_ID)
                           return Gnoga.Gui.Pointer_To_Base_Class;
      procedure Insert
        (ID     : in Gnoga.Unique_ID;
         Object : in Gnoga.Gui.Pointer_To_Base_Class);

      procedure Delete (ID : Gnoga.Unique_ID);
   private
      Object_Map : Object_Maps.Map;
   end Object_Manager;

   protected body Object_Manager is
      function Get_Object (ID : Gnoga.Unique_ID)
                           return Gnoga.Gui.Pointer_To_Base_Class
      is
      begin
         if Object_Map.Contains (ID) then
            return Object_Map.Element (ID);
         else
            raise No_Object with "ID:" & ID'Img;
         end if;
      end Get_Object;

      procedure Insert
        (ID     : in Gnoga.Unique_ID;
         Object : in Gnoga.Gui.Pointer_To_Base_Class)
      is
      begin
         Object_Map.Insert (Key      => ID,
                            New_Item => Object);
      end Insert;

      procedure Delete (ID : Gnoga.Unique_ID) is
      begin
         if Object_Map.Contains (ID) then
            Object_Map.Delete (ID);
         end if;
      end Delete;
   end Object_Manager;

   --------------------
   -- WebSocket_Open --
   --------------------

   overriding
   function WebSocket_Open (Client : access Gnoga_HTTP_Client)
                            return WebSocket_Accept
   is

      Status : Status_Line renames Get_Status_Line (Client.all);

      F : constant String := Status.File;
   begin
      if F /= "gnoga" then
         Gnoga.Log ("Invalid URL for Websocket: " & F);
         declare
            Reason : constant String := "Invalid URL";
         begin
            return (Accepted => False,
                    Length   => Reason'Length,
                    Code     => 400,
                    Reason   => Reason);
         end;
      end if;

      Client.Content.Connection_Type := WebSocket;

      if On_Connect_Event /= null then
         return (Accepted  => True,
                 Length    => 0,
                 Size      => Max_Websocket_Message,
                 Duplex    => True,
                 Chunked   => True,
                 Protocols => "");
      else
         Gnoga.Log ("No Connection event set.");
         declare
            Reason : constant String := "No connection event set";
         begin
            return (Accepted => False,
                    Length   => Reason'Length,
                    Code     => 400,
                    Reason   => Reason);
         end;
      end if;
   end WebSocket_Open;

   --------------------------
   -- WebSocket_Initialize --
   --------------------------

   overriding
   procedure WebSocket_Initialize (Client : in out Gnoga_HTTP_Client)
   is
      Status : Status_Line renames Get_Status_Line (Client);

      F      : constant String := Status.Query;
      S      : constant Socket_Type := Client'Unchecked_Access;

      ID     : Gnoga.Connection_ID := Gnoga.No_Connection;

      function Get_Old_ID return String;

      function Get_Old_ID return String is
         use Ada.Strings.Fixed;

         C : constant String := "Old_ID=";

         I : constant Integer := Index (F, C);
      begin
         if I > 0 then
            return F (I + C'Length .. F'Last);
         else
            return "";
         end if;
      end Get_Old_ID;

      Old_ID : constant String := Get_Old_ID;
   begin
      Connection_Manager.Add_Connection (Socket => S,
                                         New_ID => ID);

      if Old_ID /= "" and Old_ID /= "undefined" then
         if Verbose_Output then
            Gnoga.Log ("Swapping websocket connection " &
                         ID'Img & " <=> " & Old_ID);
         end if;

         begin
            Connection_Manager.Swap_Connection
              (ID, Gnoga.Connection_ID'Value (Old_ID));
         exception
            when E : Connection_Error =>
               Gnoga.Log ("Connection error - " & ID'Img);
               Gnoga.Log (Ada.Exceptions.Exception_Message (E));
               Client.Content.Finalized := True;
               Connection_Manager.Delete_Connection (ID);
               Gnoga.Log ("Connection aborted - " & ID'Img);
         end;
      else
         Connection_Manager.Start_Connection (ID);

         if Verbose_Output then
            Gnoga.Log ("New connection - ID" & ID'Img);
         end if;
      end if;
   exception
      when E : others =>
         Gnoga.Log ("Open error ID" & ID'Img);
         Gnoga.Log (Ada.Exceptions.Exception_Information (E));
   end WebSocket_Initialize;

   ----------------------
   -- WebSocket_Closed --
   ----------------------

   overriding
   procedure WebSocket_Closed (Client  : in out Gnoga_HTTP_Client;
                               Status  : in     WebSocket_Status;
                               Message : in     String)
   is
      pragma Unreferenced (Status);
      S  : constant Socket_Type := Client'Unchecked_Access;

      ID : constant Gnoga.Connection_ID :=
             Connection_Manager.Find_Connection_ID (S);
   begin
      if ID /= Gnoga.No_Connection then
         S.Content.Finalized := True;

         if Verbose_Output then
            if Message /= "" then
               Gnoga.Log ("Websocket connection closed - ID" & ID'Img &
                            " with message : " & Message);
            else
               Gnoga.Log ("Websocket connection closed - ID" & ID'Img);
            end if;
         end if;

         Connection_Manager.Delete_Connection (ID);
      end if;
   end WebSocket_Closed;

   ---------------------
   -- WebSocket_Error --
   ---------------------

   overriding
   procedure WebSocket_Error
     (Client : in out Gnoga_HTTP_Client;
      Error  : in     Ada.Exceptions.Exception_Occurrence)
   is
      ID : constant Gnoga.Connection_ID := Connection_Manager.Find_Connection_ID (Client'Unchecked_Access);
   begin
      Client.Content.Finalized := True;

      if not Exit_Application_Requested then
         Gnoga.Log
            (Message => "Connection error ID" & ID'Image & " with message: " & Ada.Exceptions.Exception_Information (Error) );
      end if;
      --  If not reconnected by next watchdog ping connection will be deleted.
   end WebSocket_Error;

   --------------------------------
   -- Start_Long_Polling_Connect --
   --------------------------------

   procedure Start_Long_Polling_Connect
     (Client : in out Gnoga_HTTP_Client;
      ID     : out    Gnoga.Connection_ID)
   is
      S  : constant Socket_Type := Client'Unchecked_Access;
   begin
      Connection_Manager.Add_Connection (Socket => S,
                                         New_ID => ID);
      Connection_Manager.Start_Connection (ID);

      if Verbose_Output then
         Gnoga.Log ("New long polling connection - ID" & ID'Img);
      end if;
   end Start_Long_Polling_Connect;

   --------------------
   -- Script_Manager --
   --------------------

   protected type Script_Holder_Type is
      entry Hold;
      procedure Release (Result : in String);
      function Result return String;
   private
      Connected     : Boolean := True;
      Script_Result : Ada.Strings.Unbounded.Unbounded_String;
   end Script_Holder_Type;

   protected body Script_Holder_Type is
      entry Hold when not Connected is
      begin
         null;
         --  Semaphore does not reset itself to a blocking state.
         --  This ensures that if Released before Hold that Hold
         --  will not block and connection will be released.
      end Hold;

      procedure Release (Result : in String) is
      begin
         Connected := False;
         Script_Result := Ada.Strings.Unbounded.To_Unbounded_String (Result);
      end Release;

      function Result return String is
      begin
         return Ada.Strings.Unbounded.To_String (Script_Result);
      end Result;
   end Script_Holder_Type;

   type Script_Holder_Access is access all Script_Holder_Type;

   package Script_Holder_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Unique_ID, Script_Holder_Access);

   protected type Script_Manager_Type is
      procedure Add_Script_Holder (ID     : out Gnoga.Unique_ID;
                                   Holder : in  Script_Holder_Access);
      --  Adds a script holder to wait for script execution to end
      --  and return results;

      procedure Delete_Script_Holder (ID : in Gnoga.Unique_ID);
      --  Delete script holder

      procedure Release_Hold (ID     : in Gnoga.Unique_ID;
                              Result : in String);
      --  Delete connection hold with ID.
   private
      Script_Holder_Map : Script_Holder_Maps.Map;
      Script_ID         : Gnoga.Unique_ID := 0;
   end Script_Manager_Type;

   protected body Script_Manager_Type is
      procedure Add_Script_Holder (ID     : out Gnoga.Connection_ID;
                                   Holder : in Script_Holder_Access)
      is
      begin
         Script_ID := Script_ID + 1;
         Script_Holder_Map.Insert (Script_ID, Holder);

         ID := Script_ID;
      end Add_Script_Holder;

      procedure Delete_Script_Holder (ID : in Gnoga.Connection_ID) is
      begin
         Script_Holder_Map.Delete (ID);
      end Delete_Script_Holder;

      procedure Release_Hold (ID     : in Gnoga.Unique_ID;
                              Result : in String)
      is
      begin
         if Script_Holder_Map.Contains (ID) then
            Script_Holder_Map.Element (ID).Release (Result);
         end if;
      end Release_Hold;
   end Script_Manager_Type;

   Script_Manager : Script_Manager_Type;

   -----------------------------
   -- WebSocket_Received_Part --
   -----------------------------

   overriding
   procedure WebSocket_Received_Part (Client  : in out Gnoga_HTTP_Client;
                                      Message : in     String)
   is
   begin
      Client.Content.Input_Overflow.Add (Message);
   end WebSocket_Received_Part;

   ------------------------
   -- WebSocket_Received --
   ------------------------

   overriding
   procedure WebSocket_Received (Client  : in out Gnoga_HTTP_Client;
                                 Message : in     String)
   is
      Full_Message : constant String :=
        Client.Content.Input_Overflow.Get & Message;
   begin
      Client.Content.Input_Overflow.Clear;

      if Full_Message = "0" then
         return;
      end if;

      Dispatch_Message (Strings_Edit.UTF8.Handling.To_String (Full_Message,
                        Substitution_Character));

   exception
      when E : others =>
         Log ("Websocket Message Error");
         Log (Ada.Exceptions.Exception_Information (E));
   end WebSocket_Received;

   ----------------------
   -- Dispatch_Message --
   ----------------------

   procedure Dispatch_Message (Message : in String) is
      use Ada.Strings.Fixed;
   begin
      if Message (Message'First) = 'S' then
         declare
            P1 : constant Integer := Index (Source  => Message,
                                            Pattern => "|");

            UID    : constant String := Message (Message'First + 2 .. (P1 - 1));
            Result : constant String := Message ((P1 + 1) .. Message'Last);
         begin
            Script_Manager.Release_Hold (Gnoga.Unique_ID'Value (UID),
                                         Result);
         end;
      else
         declare
            P1 : constant Integer := Index (Source  => Message,
                                            Pattern => "|");

            P2 : constant Integer := Index (Source  => Message,
                                            Pattern => "|",
                                            From    => P1 + 1);

            UID        : constant String := Message (Message'First .. (P1 - 1));
            Event      : constant String := Message ((P1 + 1) .. (P2 - 1));
            Event_Data : constant String := Message ((P2 + 1) .. Message'Last);

            Object : constant Gnoga.Gui.Pointer_To_Base_Class :=
                       Object_Manager.Get_Object (Integer'Value (UID));
         begin
            Gui.Event_Queue.Enqueue (New_Item => (Event  => Ada.Strings.Unbounded.To_Unbounded_String (Event),
                                                  Object => Object,
                                                  Data   => Ada.Strings.Unbounded.To_Unbounded_String (Event_Data) ) );
         end;
      end if;
   exception
      when E : No_Object =>
         Log ("Request to dispatch message to non-existant object");
         Log (Ada.Exceptions.Exception_Information (E));
         return;
      when E : others =>
         Log ("Dispatch Message Error");
         Log (Ada.Exceptions.Exception_Information (E));
   end Dispatch_Message;

   -------------------
   -- String_Buffer --
   -------------------

   protected body String_Buffer is
      procedure Buffering (Value : Boolean) is
      begin
         Is_Buffering := Value;
      end Buffering;

      function Buffering return Boolean is
      begin
         return Is_Buffering;
      end Buffering;

      procedure Add (S : in String) is
         use type Ada.Strings.Unbounded.Unbounded_String;
      begin
         Buffer := Buffer & S;
      end Add;

      procedure Preface (S : in String) is
         use type Ada.Strings.Unbounded.Unbounded_String;
      begin
         Buffer := S & Buffer;
      end Preface;

      function Length return Natural is
      begin
         return Ada.Strings.Unbounded.Length (Buffer);
      end Length;

      function Get return String is
      begin
         return Ada.Strings.Unbounded.To_String (Buffer);
      end Get;

      procedure Get_And_Clear (S : out Ada.Strings.Unbounded.Unbounded_String)
      is
      begin
         S := Buffer;
         Buffer := Ada.Strings.Unbounded.To_Unbounded_String ("");
      end Get_And_Clear;

      procedure Clear is
      begin
         Buffer := Ada.Strings.Unbounded.To_Unbounded_String ("");
      end Clear;
   end String_Buffer;

   ----------------
   -- Buffer_Add --
   ----------------

   function Buffer_Add (ID     : Gnoga.Connection_ID;
                        Script : String)
                        return Boolean
   is
      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);
   begin
      if Socket.Content.Buffer.Buffering then
         if Socket.Content.Buffer.Length + Script'Length >=
           Max_Buffer_Length
         then
            Flush_Buffer (ID);
         end if;

         if Socket.Content.Connection_Type = WebSocket then
            Socket.Content.Buffer.Add (Script &
                                         Gnoga.Server.Connection.Common.CRLF);
         elsif Socket.Content.Connection_Type = Long_Polling then
            Socket.Content.Buffer.Add
              ("<script>" & Script & "</script>");
         else
            Gnoga.Log ("Buffer_Add called on unsupported connection type.");
         end if;

         return True;
      else
         return False;
      end if;
   end Buffer_Add;

   -----------------------
   -- Buffer_Connection --
   -----------------------

   function Buffer_Connection (ID : Gnoga.Connection_ID) return Boolean
   is
      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);
   begin
      return Socket.Content.Buffer.Buffering;
   end Buffer_Connection;

   procedure Buffer_Connection (ID    : in Gnoga.Connection_ID;
                                Value : in Boolean)
   is
      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);
   begin
      if Value = False then
         Flush_Buffer (ID);
      end if;

      Socket.Content.Buffer.Buffering (Value);
   end Buffer_Connection;

   ------------------
   -- Flush_Buffer --
   ------------------

   procedure Flush_Buffer (ID : in Gnoga.Connection_ID)
   is
      Socket : Socket_Type;
   begin
      if Connection_Manager.Valid (ID) then
         Socket :=
           Connection_Manager.Connection_Socket (ID);
         if Socket.Content.Buffer.Buffering and
           Socket.Content.Connection_Type = WebSocket
         then
            Socket.Content.Buffer.Buffering (False);
            Execute_Script (ID, Socket.Content.Buffer.Get);
            Socket.Content.Buffer.Clear;
            Socket.Content.Buffer.Buffering (True);
         elsif Socket.Content.Connection_Type = Long_Polling then
            Socket.Unblock_Send;
         end if;
      end if;
   exception
      when E : Connection_Error =>
         --  Connection already closed.
         Log ("Connection" & ID'Img & " already closed.");
         Log (Ada.Exceptions.Exception_Information (E));
      when E : others =>
         Log ("Flush_Buffer Error -" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
   end Flush_Buffer;

   -------------------
   -- Buffer_Append --
   -------------------

   procedure Buffer_Append (ID    : in Gnoga.Connection_ID;
                            Value : in String)
   is
      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);
   begin
      Socket.Content.Buffer.Add (Value);
   end Buffer_Append;

   --------------------
   -- Execute_Script --
   --------------------

   procedure Execute_Script (ID     : in Gnoga.Connection_ID;
                             Script : in String)
   is
      UTF8_Script : constant String :=
        Strings_Edit.UTF8.Handling.To_UTF8 (Script);

      procedure Try_Execute;

      procedure Try_Execute is
         Socket  : constant Socket_Type :=
           Connection_Manager.Connection_Socket (ID);
      begin
         if Socket.Content.Connection_Type = Long_Polling then
            Socket.Content.Buffer.Add ("<script>" & UTF8_Script & "</script>");

            if not Socket.Content.Buffer.Buffering then
               Socket.Unblock_Send;
            end if;
         elsif Socket.Content.Connection_Type = WebSocket then
            Socket.WebSocket_Send (UTF8_Script);
         end if;
      exception
         when E : Ada.Text_IO.End_Error =>
            Log ("Error Try_Execute - " & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            raise Connection_Error with
              "Socket Closed before execute of : " & Script;
         when E : others =>
            Log ("Error Try_Execute - " & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            raise Connection_Error with
              "Socket Error during execute of : " & Script;
      end Try_Execute;

   begin
      if Connection_Manager.Valid (ID) and UTF8_Script /= "" then
         if not Buffer_Add (ID, UTF8_Script) then
            Try_Execute;
         end if;
      end if;
   exception
      when E : others =>
         Log ("Error Execute_Script -" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
         delay 2.0;
         Try_Execute;
   end Execute_Script;

   function Execute_Script (ID     : in Gnoga.Connection_ID;
                            Script : in String)
                            return String
   is
      UTF8_Script : constant String :=
        Strings_Edit.UTF8.Handling.To_UTF8 (Script);

      function Try_Execute return String;

      function Try_Execute return String is
         Script_Holder : aliased Script_Holder_Type;
      begin
         declare
            Script_ID : Gnoga.Unique_ID;
            Socket    : constant Socket_Type :=
                          Connection_Manager.Connection_Socket (ID);
         begin
            Script_Manager.Add_Script_Holder
              (ID     => Script_ID,
               Holder => Script_Holder'Unchecked_Access);

            declare
               Message : constant String := "ws.send (" &
                           """S" & Script_ID'Img & "|""+" &
                           "eval (""" & UTF8_Script & """)" &
                           ");";
            begin
               if Socket.Content.Connection_Type = Long_Polling then
                  Socket.Content.Buffer.Add ("<script>" &
                                               Message &
                                               "</script>");
                  Socket.Unblock_Send;
               elsif Socket.Content.Connection_Type = WebSocket then
                  Socket.WebSocket_Send (Message);
               end if;

               select
                  delay Script_Time_Out; --  Timeout for browser answer

                  Script_Manager.Delete_Script_Holder (Script_ID);

                  raise Script_Error with
                    "Timeout error, no browser response for: " & Message;
               then abort
                  Script_Holder.Hold;
               end select;
            end;

            declare
               Result : constant String := Script_Holder.Result;
            begin
               Script_Manager.Delete_Script_Holder (Script_ID);

               return Result;
            end;
         end;
      exception
         when E : Ada.Text_IO.End_Error =>
            Log ("Error Try_Execute -" & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            raise Connection_Error with
              "Socket Closed before execute of : " & Script;
         when E : others =>
            Log ("Error Try_Execute -" & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            raise Connection_Error with
              "Socket Error during execute of : " & Script;
      end Try_Execute;
   begin
      begin
         if Connection_Manager.Valid (ID) then
            Flush_Buffer (ID);
            return Try_Execute;
         else
            raise Connection_Error with "Invalid ID " & ID'Img;
         end if;
      exception
         when E : others =>
            Log ("Error Execute_Script -" & ID'Img);
            Log (Ada.Exceptions.Exception_Information (E));
            begin
               delay 2.0;
               return Try_Execute;
            exception
               when E : others =>
                  Log ("Error Execute_Script after retrying -" & ID'Img);
                  Log (Ada.Exceptions.Exception_Information (E));
                  Close (ID);
                  raise Connection_Error with "Invalid ID " & ID'Img;
            end;
      end;
   end Execute_Script;

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (ID   : in     Gnoga.Connection_ID;
      Data : access Gnoga.Connection_Data_Type'Class)
   is
   begin
      Connection_Manager.Add_Connection_Data
        (ID,
         Gnoga.Pointer_to_Connection_Data_Class (Data));
   end Connection_Data;

   function Connection_Data
     (ID : in Gnoga.Connection_ID)
      return Gnoga.Pointer_to_Connection_Data_Class
   is
   begin
      return Connection_Manager.Connection_Data (ID);
   end Connection_Data;

   ------------------------
   -- On_Connect_Handler --
   ------------------------

   procedure On_Connect_Handler (Event : in Connect_Event) is
   begin
      On_Connect_Event := Event;
   end On_Connect_Handler;

   ---------------------
   -- Connection_Type --
   ---------------------

   function Connection_Type (ID : Gnoga.Connection_ID)
                             return Gnoga_Connection_Type
   is
      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);
   begin
      return Socket.Content.Connection_Type;
   exception
      when E : Connection_Error =>
         Log ("Error Connection_Type -" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
         return None;
   end Connection_Type;

   ---------------------
   -- Connection_Path --
   ---------------------

   function Connection_Path (ID : Gnoga.Connection_ID)
                             return String
   is
      use Ada.Strings.Unbounded;

      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);

      S : constant String := To_String (Socket.Content.Connection_Path);
   begin
      if Socket.Content.Connection_Type = Long_Polling then
         return S;
      else
         if S = "" then
            Socket.Content.Connection_Path :=
              To_Unbounded_String (Left_Trim_Slashes
                                   (Execute_Script
                                      (ID, "window.location.pathname")));

            return To_String (Socket.Content.Connection_Path);
         else
            return S;
         end if;
      end if;
   exception
      when E : Connection_Error =>
         Log ("Error Connection_Path -" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
         return "";
   end Connection_Path;

   -------------------------------
   -- Connection_Client_Address --
   -------------------------------

   function Connection_Client_Address (ID : Gnoga.Connection_ID)
                                       return String
   is
      Socket : constant Socket_Type :=
        Connection_Manager.Connection_Socket (ID);
      Client_Address : constant GNAT.Sockets.Sock_Addr_Type := Get_Client_Address (Socket.all);
   begin
      return GNAT.Sockets.Image (Client_Address);
   exception
      when E : Connection_Error =>
         Log ("Error Connection_Client_Address -" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
         return "";
   end Connection_Client_Address;

   ------------------------
   -- Active_Connections --
   ------------------------

   function Active_Connections return Natural is
   begin
      return Natural (Connection_Manager.Active_Connections);
   end Active_Connections;

   -----------------------------
   -- On_Post_Request_Handler --
   -----------------------------

   procedure On_Post_Request_Handler (Event : Post_Request_Event) is
   begin
      On_Post_Request_Event := Event;
   end On_Post_Request_Handler;

   ---------------------
   -- On_Post_Handler --
   ---------------------

   procedure On_Post_Handler (Event : in Post_Event) is
   begin
      On_Post_Event := Event;
   end On_Post_Handler;

   --------------------------
   -- On_Post_File_Handler --
   --------------------------

   procedure On_Post_File_Handler (Event : in Post_File_Event) is
   begin
      On_Post_File_Event := Event;
   end On_Post_File_Handler;

   --------------------
   -- Form_Parameter --
   --------------------

   function Form_Parameter (ID   : Gnoga.Connection_ID;
                            Name : String)
                            return String
   is
   begin
      return Execute_Script (ID, "params['" & Name & "'];");
   end Form_Parameter;

   -----------
   -- Valid --
   -----------

   function Valid (ID : Gnoga.Connection_ID) return Boolean is
   begin
      if ID = Gnoga.No_Connection then
         return False;
      else
         return Connection_Manager.Valid (ID);
      end if;
   end Valid;

   -----------
   -- Close --
   -----------

   procedure Close (ID : Gnoga.Connection_ID) is
   begin
      if Valid (ID) then
         declare
            Socket  : constant Socket_Type :=
              Connection_Manager.Connection_Socket (ID);
         begin
            if Socket.Content.Connection_Type = Long_Polling then
               Socket.Content.Finalized := True;
            else
               Execute_Script (ID, "ws.close()");
            end if;
         exception
            when E : others =>
               Log ("Error Close - " & ID'Img);
               Log (Ada.Exceptions.Exception_Information (E));
         end;
      end if;
   end Close;

   -------------------
   -- HTML_On_Close --
   -------------------

   procedure HTML_On_Close (ID   : in Gnoga.Connection_ID;
                            HTML : in String)
   is
   begin
      Execute_Script (ID     => ID,
                      Script => "gnoga['html_on_close']='" &
                        Escape_Quotes (HTML) & "';");
   end HTML_On_Close;

   ---------------------
   -- ID_Machine_Type --
   ---------------------

   protected type ID_Machine_Type is
      procedure Next_ID (ID : out Gnoga.Unique_ID);
   private
      Current_ID : Gnoga.Unique_ID := 0;
   end ID_Machine_Type;

   protected body ID_Machine_Type is
      procedure Next_ID (ID : out Gnoga.Unique_ID) is
      begin
         Current_ID := Current_ID + 1;
         ID := Current_ID;
      end Next_ID;
   end ID_Machine_Type;

   ID_Machine : ID_Machine_Type;

   -------------------
   -- New_Unique_ID --
   -------------------

   procedure New_Unique_ID (New_ID : out Gnoga.Unique_ID) is
   begin
      ID_Machine.Next_ID (New_ID);
   end New_Unique_ID;

   -------------
   -- New_GID --
   -------------

   function New_GID return String is
      New_ID : Gnoga.Unique_ID;
   begin
      New_Unique_ID (New_ID);

      return "g" & Left_Trim (New_ID'Img);
   end New_GID;

   --------------------------
   -- Add_To_Message_Queue --
   --------------------------

   procedure Add_To_Message_Queue
     (Object : in out Gnoga.Gui.Base_Type'Class)
   is
   begin
      Object_Manager.Insert (Object.Unique_ID, Object'Unchecked_Access);
   end Add_To_Message_Queue;

   -------------------------------
   -- Delete_From_Message_Queue --
   -------------------------------

   procedure Delete_From_Message_Queue
     (Object : in out Gnoga.Gui.Base_Type'Class) is
   begin
      Object_Manager.Delete (Object.Unique_ID);
   end Delete_From_Message_Queue;

   ----------
   -- Stop --
   ----------

   procedure Stop is
      ID : Gnoga.Connection_ID;
      procedure Free is new Ada.Unchecked_Deallocation (Watchdog_Type, Watchdog_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Gnoga_HTTP_Server_Type, Gnoga_HTTP_Server_Access);
   begin
      if not Exit_Application_Requested and
        Watchdog /= null and
        Gnoga_HTTP_Server /= null
      then
         Exit_Application_Requested := True;
         Watchdog.Stop;
         Free (Watchdog);

         Connection_Manager.First (ID);
         while ID /= 0 loop
            begin
               Close (ID);
               Connection_Manager.Next (ID);
            exception
               when others =>
                  Connection_Manager.First (ID);
            end;
         end loop;

         Connection_Manager.Delete_All_Connections;

         Gnoga_HTTP_Server.Stop;
         Free (Gnoga_HTTP_Server);
      end if;
   end Stop;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Item   : in     Gnoga_HTTP_Content)
   is
   begin
      null;
   end Write;

   ---------
   -- Get --
   ---------

   overriding
   function Get (Source : access Gnoga_HTTP_Content) return String is
   begin
      if Source.Buffer.Length = 0 then
         if Source.Connection_Type = HTTP then
            return "";
         elsif Source.Finalized then
            declare
               ID : constant Gnoga.Connection_ID :=
                 Connection_Manager.Find_Connection_ID
                   (Source.Socket);
            begin
               Gnoga.Log ("Shutting down long polling connection -" & ID'Img);
               return "";
            end;
         else
            raise Content_Not_Ready;
         end if;
      else
         declare
            use Ada.Strings.Unbounded;

            Chunk_Size : constant := Max_HTTP_Output_Chunk - 80;

            S : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Source.Buffer.Get_And_Clear (S);

            if Length (S) > Chunk_Size then
               Source.Buffer.Preface (Slice (Source => S,
                                             Low    => 1 + Chunk_Size,
                                             High   => Length (S)));

               return Slice (Source => S,
                             Low    => 1,
                             High   => Chunk_Size);
            else
               return To_String (S);
            end if;
         end;
      end if;
   end Get;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Client : in out Gnoga_HTTP_Client) is
      ID : constant Gnoga.Connection_ID :=
             Connection_Manager.Find_Connection_ID (Client'Unchecked_Access);
   begin
      if Ada.Streams.Stream_IO.Is_Open (Client.Content.FS) then
         Ada.Streams.Stream_IO.Close (Client.Content.FS);
      end if;

      if ID /= Gnoga.No_Connection then
         Gnoga.Log ("Deleting connection during finalize -" & ID'Img);
         Connection_Manager.Delete_Connection (ID);
      end if;

      HTTP_Client (Client).Finalize;
   exception
      when E : others =>
         Log ("Error Finalize Gnoga_HTTP_Client -" & ID'Img);
         Log (Ada.Exceptions.Exception_Information (E));
   end Finalize;
begin
   Gnoga.Server.Connection.Common.Gnoga_Client_Factory :=
      Global_Gnoga_Client_Factory'Access;
end Ada_GUI.Gnoga.Server.Connection;

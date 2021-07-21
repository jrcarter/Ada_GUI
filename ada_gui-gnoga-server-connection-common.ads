-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . S E R V E R . C O N N E C I O N . C O M M O N        --
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

with GNAT.Sockets;
with GNAT.Sockets.Server; use GNAT.Sockets.Server;

package Ada_GUI.Gnoga.Server.Connection.Common is

   -------------------------------------------------------------------------
   --  Gnoga Connection Settings
   -------------------------------------------------------------------------

   Max_HTTP_Request_Length : constant := 1024;
   Max_HTTP_Connections    : constant := 200;
   Max_HTTP_Input_Chunk    : constant := 1024;
   Max_HTTP_Output_Chunk   : constant := 1024;
   Max_Websocket_Message   : constant := 1024;
   Script_Time_Out         : constant := 3.0;

   Max_Buffer_Length : constant := 2 ** 16 - 1;
   --  Maximum length of to Buffer output to Gnoga clients before an
   --  automatic buffer flush is done.

   -------------------------------------------------------------------------
   --  Private Variables
   -------------------------------------------------------------------------

   CRLF : constant String := (Character'Val (13), Character'Val (10));

   Boot_HTML   : Ada.Strings.Unbounded.Unbounded_String;

   Server_Host : Ada.Strings.Unbounded.Unbounded_String;
   Server_Port : GNAT.Sockets.Port_Type;

   Secure_Port   : GNAT.Sockets.Port_Type;
   Secure_Only   : Boolean := False;
   Secure_Server : Boolean := False;
   Secure_Crt    : Ada.Strings.Unbounded.Unbounded_String;
   Secure_Key    : Ada.Strings.Unbounded.Unbounded_String;

   Verbose_Output : Boolean := False;

   type Client_Factory_Type is access function
     (Listener       : access Connections_Server'Class;
      Request_Length : Positive;
      Input_Size     : Buffer_Length;
      Output_Size    : Buffer_Length)
      return Connection_Ptr;

   Gnoga_Client_Factory : Client_Factory_Type := null;

   type Pointer_To_Connections_Factory_Class is
     access all Connections_Factory'Class;

   type Server_Factory_Type is
     access function return Pointer_To_Connections_Factory_Class;

   Gnoga_Secure_Factory : Server_Factory_Type := null;

end Ada_GUI.Gnoga.Server.Connection.Common;

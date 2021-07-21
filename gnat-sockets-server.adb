--                                                                    --
--  package GNAT.Sockets.Server     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  14:53 29 Feb 2020  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Calendar;             use Ada.Calendar;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GNAT.Sockets.Server is

   Receive_Masks : constant array (IO_Tracing_Mode) of Factory_Flags :=
                   (  Trace_None    =>  0,
                      Trace_Encoded => Trace_Encoded_Received,
                      Trace_Decoded => Trace_Decoded_Received,
                      Trace_Any     => Trace_Encoded_Received
                                    or Trace_Decoded_Received
                   );
   Sent_Masks    : constant array (IO_Tracing_Mode) of Factory_Flags :=
                   (  Trace_None    =>  0,
                      Trace_Encoded => Trace_Encoded_Sent,
                      Trace_Decoded => Trace_Decoded_Sent,
                      Trace_Any     => Trace_Encoded_Sent
                                    or Trace_Decoded_Sent
                   );

   procedure Free is
      new Ada.Unchecked_Deallocation (Encoder'Class, Encoder_Ptr);

   procedure Activated (Client : in out Connection) is
   begin
      null;
   end Activated;

   procedure Append
             (  List  : in out Connection_Ptr;
                Item  : Connection_Ptr;
                Count : in out Integer
             )  is
   begin
      if Item.Successor = null then
         if List = null then
            List := Item;
            Item.Successor   := Item;
            Item.Predecessor := Item;
         else
            Item.Successor   := List;
            Item.Predecessor := List.Predecessor;
            List.Predecessor := Item;
            Item.Predecessor.Successor := Item;
         end if;
         Count := Count + 1;
      end if;
   end Append;

   function Available_To_Process (Client : Connection)
      return Stream_Element_Count is
   begin
      return Used (Client.Read);
   end Available_To_Process;

   function Available_To_Send (Client : Connection)
      return Stream_Element_Count is
   begin
      return Free (Client.Written);
   end Available_To_Send;

   procedure Clear (Client : in out Connection'Class) is
   begin
      Client.Failed                := False; -- Clean I/O state
      Client.External_Action       := False;
      Client.Data_Sent             := False;
      Client.Dont_Block            := False;
      Client.Read.Expected         := 0;
      Client.Read.First_Read       := 0;
      Client.Read.Free_To_Read     := 0;
      Client.Written.First_Written := 0;
      Client.Written.Free_To_Write := 0;
      Client.Written.Send_Blocked  := False;
      Free (Client.Transport);
   end Clear;

   procedure Close (Socket : in out Socket_Type) is
   begin
      if Socket /= No_Socket then
         begin
            Shutdown_Socket (Socket);
         exception
            when others =>
               null;
         end;
         begin
            Close_Socket (Socket);
         exception
            when others =>
               null;
         end;
         Socket := No_Socket;
      end if;
   end Close;

   procedure Connect
             (  Listener       : in out Connections_Server;
                Client         : Connection_Ptr;
                Host           : String;
                Port           : Port_Type;
                Max_Connect_No : Positive := Positive'Last;
                Overlapped     : Stream_Element_Count :=
                                 Stream_Element_Count'Last
             )  is
      Address : Sock_Addr_Type renames Client.Client_Address;
      Option  : Request_Type := (Non_Blocking_IO, True);
   begin
      if Client.Socket /= No_Socket then
         Raise_Exception
         (  Use_Error'Identity,
            "Connection " & Image (Address) & " is already in use"
         );
      end if;
      Address.Addr := To_Addr (Host);
      Address.Port := Port;
      Create_Socket (Client.Socket);
      Set_Socket_Option
      (  Client.Socket,
         Socket_Level,
         (Reuse_Address, True)
      );
      Control_Socket (Client.Socket, Option);
      Connect_Parameters_Set
      (  Client.all,
         Host,
         Address,
         Max_Connect_No
      );
      Client.Session         := Session_Disconnected;
      Client.Client          := True;
      Client.Connect_No      := 0;
      Client.Max_Connect_No  := Max_Connect_No;
      Client.Socket_Listener := Listener'Unchecked_Access;
      Client.Overlapped_Read := Stream_Element_Count'Min
                                (  Overlapped,
                                   Client.Output_Size
                                );
      Increment_Count (Client.all);
      Listener.Request.Connect (Client);
   end Connect;

   procedure Connect_Error
             (  Client : in out Connection;
                Error  : Error_Type
             )  is
   begin
      null;
   end Connect_Error;

   procedure Connect_Parameters_Set
             (  Client         : in out Connection;
                Host           : String;
                Address        : Sock_Addr_Type;
                Max_Connect_No : Positive
             )  is
   begin
      null;
   end Connect_Parameters_Set;

   procedure Connected (Client : in out Connection) is
   begin
      null;
   end Connected;

   procedure Connected
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             )  is
   begin
      Client.Session := Session_Active;
   end Connected;

   function Create
            (  Factory  : access Connections_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
   begin
      return null;
   end Create;

   procedure Create_Socket
             (  Listener : in out Connections_Server;
                Socket   : in out Socket_Type;
                Address  : Sock_Addr_Type
             )  is
   begin
      Create_Socket (Socket);
      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Socket, Address);
      Listen_Socket (Socket);
   end Create_Socket;

   function Create_Transport
            (  Factory  : access Connections_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr is
   begin
      return null;
   end Create_Transport;

   procedure Create_Transport (Client : in out Connection) is
   begin
      if Client.Transport /= null then
         Raise_Exception
         (  Status_Error'Identity,
            "Connection already has a transport layer"
         );
      end if;
      Client.Transport :=
         Create_Transport
         (  Client.Socket_Listener.Factory,
            Client.Socket_Listener,
            Client'Unchecked_Access
         );
      if Client.Transport = null then
         Raise_Exception
         (  Status_Error'Identity,
            "Connection transport layer is not supported"
         );
      end if;
      Client.Session := Session_Handshaking;
      if Client.Client then
         Append
         (  Client.Socket_Listener.Postponed,
            Client'Unchecked_Access,
            Client.Socket_Listener.Postponed_Count
         );
      end if;
   end Create_Transport;

   procedure Data_Sent
             (  Listener : in out Connections_Server;
                Client   : Connection_Ptr
             )  is
   begin
      Client.Data_Sent := False;
      Sent (Client.all);
   end Data_Sent;

   procedure Disconnected (Client : in out Connection) is
   begin
      null;
   end Disconnected;

   procedure Disconnected
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             )  is
   begin
      Client.Session := Session_Disconnected;
      Remove (Listener.Postponed, Client, Listener.Postponed_Count);
   end Disconnected;

   procedure Downed
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             )  is
   begin
      Client.Session := Session_Down;
   end Downed;

   procedure Downed (Client : in out Connection) is
   begin
      null;
   end Downed;

   procedure Do_Connect
             (  Listener : in out Connections_Server'Class;
                Client   : in out Connection_Ptr
             )  is
      Status : Selector_Status;
   begin
      Client.Connect_No := Client.Connect_No + 1;
      Client.Session    := Session_Connecting;
      if Client.Connect_No > Client.Max_Connect_No then
         Client.Try_To_Reconnect := False; -- Ensure connection killed
         Save_Occurrence (Client.Last_Error, Null_Occurrence);
         Stop (Listener, Client);
      else
         Clear (Client.all);
         Connect_Socket
         (  Socket   => Client.Socket,
            Server   => Client.Client_Address,
            Timeout  => 0.0,
            Selector => Listener.Selector'Unchecked_Access,
            Status   => Status
         );
         if Status = Completed then
            Set (Listener.Read_Sockets, Client.Socket);
            On_Connected (Listener, Client.all);
         end if;
      end if;
   exception
      when Connection_Error =>
         Client.Try_To_Reconnect := False; -- Ensure connection killed
         Save_Occurrence (Client.Last_Error, Null_Occurrence);
         Stop (Listener, Client);
      when Error : Socket_Error =>
         if Resolve_Exception (Error) = Operation_Now_In_Progress then
            Trace_Sending
            (  Listener.Factory.all,
               Client.all,
               False,
               ", connecting to ..."
            );
         else
            Trace_Error
            (  Listener.Factory.all,
               "Connect socket",
               Error
            );
            Client.Try_To_Reconnect := False; -- Ensure object killed
            Save_Occurrence (Client.Last_Error, Error);
            Stop (Listener, Client);
         end if;
      when Error : others =>
         Trace_Error
         (  Listener.Factory.all,
            "Connect socket",
            Error
         );
         Client.Try_To_Reconnect := False; -- Ensure connection killed
         Save_Occurrence (Client.Last_Error, Error);
         Stop (Listener, Client);
   end Do_Connect;

   procedure Elevated (Client : in out Connection) is
   begin
      null;
   end Elevated;

   procedure Fill_From_Stream
             (  Buffer  : in out Output_Buffer;
                Stream  : in out Root_Stream_Type'Class;
                Count   : Stream_Element_Count;
                Reserve : Stream_Element_Count;
                Last    : out Stream_Element_Offset;
                Next    : out Stream_Element_Offset;
                Done    : out Boolean
             )  is
   begin
      if Reserve >= Buffer.Written'Length then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer is too small for prefix and suffix ("
            &  Image (Reserve)
            &  ")"
         )  );
      end if;
      if Buffer.First_Written <= Buffer.Free_To_Write then
         --
         -- [     XXXXXXXXXXXXXXX        ]
         --       |              |
         --       First_Written  Free_To_Write
         --
         declare
            Tail : constant Stream_Element_Offset :=
                   Reserve + Buffer.Written'First - Buffer.First_Written;
         begin
            if Tail > 0 then
               if Buffer.Free_To_Write + Tail = Buffer.Written'Last then
                  Done := False;
               else
                  Next := Buffer.Written'Last - Tail;
                  if Count < Next - Buffer.Free_To_Write then
                     Next := Buffer.Free_To_Write + Count - 1;
                  end if;
                  Read
                  (  Stream,
                     Buffer.Written (Buffer.Free_To_Write..Next),
                     Last
                  );
                  Done := Last < Next;
                  Next := Last + 1;
               end if;
            else
               Next := Buffer.Written'Last;
               if Count < Next - Buffer.Free_To_Write then
                  Next := Buffer.Free_To_Write + Count - 1;
               end if;
               Read
               (  Stream,
                  Buffer.Written (Buffer.Free_To_Write..Next),
                  Last
               );
               Done := Last < Next;
               if Last < Buffer.Written'Last then
                  Next := Last + 1;
               else
                  Next := Buffer.Written'First;
               end if;
            end if;
         end;
      else
         --
         -- [XXXXX               XXXXXXX]
         --       |              |
         --       Free_To_Write  First_Written
         --
         if Buffer.Free_To_Write + Reserve >= Buffer.First_Written then
            Done := False;
         else
            Next := Buffer.First_Written - Reserve;
            if Count < Next - Buffer.Free_To_Write then
               Next := Buffer.Free_To_Write + Count - 1;
            end if;
            Read
            (  Stream,
               Buffer.Written (Buffer.Free_To_Write..Next),
               Last
            );
            Done := Last < Next;
            Next := Last + 1;
         end if;
      end if;
   end Fill_From_Stream;

   procedure Finalize (Listener : in out Connections_Server) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Worker, Worker_Ptr);
   begin
      if Listener.Doer /= null then
         Listener.Finalizing := True;
         Abort_Selector (Listener.Selector);
         while not Listener.Doer'Terminated loop
            delay 0.001;
         end loop;
         Free (Listener.Doer);
      end if;
      Close_Selector (Listener.Selector);
   end Finalize;

   procedure Finalize (Client : in out Connection) is
   begin
      Close (Client.Socket);
      Free (Client.Transport);
      Object.Finalize (Object.Entity (Client));
   end Finalize;

   function Free (Buffer : Output_Buffer) return Stream_Element_Count is
   begin
      return Buffer.Written'Length - Used (Buffer) - 1;
   end Free;

   function From_String (Data : String) return Stream_Element_Array is
      Result  : Stream_Element_Array (1..Data'Length);
      Pointer : Stream_Element_Offset := Result'First;
   begin
      for Index in Data'Range loop
         Result (Pointer) := Character'Pos (Data (Index));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_String;

   function Get_Client_Address (Client : Connection)
      return Sock_Addr_Type is
   begin
      return Client.Client_Address;
   end Get_Client_Address;

   function Get_Clients_Count (Listener : Connections_Server)
      return Natural is
   begin
      return Listener.Clients;
   end Get_Clients_Count;

   function Get_Client_Name
            (  Factory : Connections_Factory;
               Client  : Connection'Class
            )  return String is
   begin
      return Image (Client.Client_Address);
   end Get_Client_Name;

   function Get_Connections_Server (Client : Connection)
      return Connections_Server_Ptr is
   begin
      return Client.Socket_Listener;
   end Get_Connections_Server;

   function Get_IO_Timeout (Factory : Connections_Factory)
      return Duration is
   begin
      return 0.02;
   end Get_IO_Timeout;

   procedure Get_Occurrence
             (  Client : Connection;
                Source : out Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Source, Client.Last_Error);
   end Get_Occurrence;

   function Get_Overlapped_Size (Client : Connection)
      return Stream_Element_Count is
   begin
      return Client.Overlapped_Read;
   end Get_Overlapped_Size;

   function Get_Polling_Timeout (Factory : Connections_Factory)
      return Duration is
   begin
      return 0.5;
   end Get_Polling_Timeout;

   function Get_Server_Address
            (  Listener : Connections_Server
            )  return Sock_Addr_Type is
      Address : Sock_Addr_Type;
   begin
      Address.Addr := Any_Inet_Addr;
      Address.Port := Listener.Port;
      return Address;
   end Get_Server_Address;

   function Get_Session_State (Client : Connection)
      return Session_State is
      Result : constant Session_State := Client.Session;
   begin
      if Result = Session_Down then
         if Client.Socket = No_Socket then
            return Session_Down;
         else
            return Session_Disconnected; -- Almost here
         end if;
      else
         return Result;
      end if;
   end Get_Session_State;

   function Get_Socket (Client : Connection) return Socket_Type is
   begin
      return Client.Socket;
   end Get_Socket;

   function Has_Data (Buffer : Input_Buffer) return Boolean is
   begin
      return
      (  Buffer.Free_To_Read /= Buffer.First_Read
      and then
         (  Buffer.Expected = 0
         or else
            Used (Buffer) >= Buffer.Size - 1
      )  );
   end Has_Data;

   function Has_Data (Client : Connection) return Boolean is
   begin
      return Has_Data (Client.Read);
   end Has_Data;

   function Image (Code : Error_Type) return String is
      Result : String := Error_Type'Image (Code);
   begin
      for Index in Result'First + 1..Result'Last loop
         if Result (Index) = '_' then
            Result (Index) := ' ';
         else
            Result (Index) := To_Lower (Result (Index));
         end if;
      end loop;
      return Result;
   end Image;

   function Image
            (  Data        : Stream_Element_Array;
               Hexadecimal : Boolean := False
            )  return String is
   begin
      if Hexadecimal then
         declare
            Result  : String (1..Data'Length * 3);
            Pointer : Integer := 1;
         begin
            for Index in Data'Range loop
               Put
               (  Destination => Result,
                  Pointer     => Pointer,
                  Value       => Integer (Data (Index)),
                  Base        => 16,
                  Field       => 2,
                  Fill        => '0',
                  Justify     => Right
               );
               Put
               (  Destination => Result,
                  Pointer     => Pointer,
                  Value       => " "
               );
            end loop;
            return Result;
         end;
      else
         declare
            Length : Natural := 0;
         begin
            for Index in Data'Range loop
               case Data (Index) is
                  when 32..36 | 38..126 =>
                     Length := Length + 1;
                  when others =>
                     Length := Length + 3;
               end case;
            end loop;
            declare
               Result  : String (1..Length);
               Pointer : Integer := 1;
            begin
               for Index in Data'Range loop
                  case Data (Index) is
                     when 32..36 | 38..126 =>
                        Put
                        (  Destination => Result,
                           Pointer     => Pointer,
                           Value       => Character'Val (Data (Index))
                        );
                     when others =>
                        Put
                        (  Destination => Result,
                           Pointer     => Pointer,
                           Value       => '%'
                        );
                        Put
                        (  Destination => Result,
                           Pointer     => Pointer,
                           Value       => Integer (Data (Index)),
                           Base        => 16,
                           Field       => 2,
                           Fill        => '0',
                           Justify     => Right
                        );
                  end case;
               end loop;
               return Result;
            end;
         end;
      end if;
   end Image;

   procedure Initialize (Listener : in out Connections_Server) is
   begin
      Listener.IO_Timeout := Get_IO_Timeout (Listener.Factory.all);
      Listener.Polling_Timeout :=
         Get_Polling_Timeout (Listener.Factory.all);
      Create_Selector (Listener.Selector);
      Listener.Doer := new Worker (Listener'Unchecked_Access);
   end Initialize;

   function Is_Connected (Client : Connection) return Boolean is
   begin
      return Client.Session in Session_Active..Session_Busy;
   end Is_Connected;

   function Is_Down (Client : Connection) return Boolean is
   begin
      return
      (  Client.Session = Session_Down
      and then
         Client.Socket = No_Socket
      );
   end Is_Down;

   function Is_Elevated (Client : Connection) return Boolean is
   begin
      return Client.Transport /= null;
   end Is_Elevated;

   function Is_Incoming (Client : Connection) return Boolean is
   begin
      return not Client.Client;
   end Is_Incoming;

   function Is_Opportunistic (Client : Connection) return Boolean is
   begin
      return False;
   end Is_Opportunistic;

   function Is_TLS_Capable
            (  Factory : Connections_Factory
            )  return Boolean is
   begin
      return False;
   end Is_TLS_Capable;

   function Is_Trace_Received_On
            (  Factory : Connections_Factory;
               Encoded : IO_Tracing_Mode
            )  return Boolean is
   begin
      return 0 /= (Factory.Trace_Flags and Receive_Masks (Encoded));
   end Is_Trace_Received_On;

   function Is_Trace_Sent_On
            (  Factory : Connections_Factory;
               Encoded : IO_Tracing_Mode
            )  return Boolean is
   begin
      return 0 /= (Factory.Trace_Flags and Sent_Masks (Encoded));
   end Is_Trace_Sent_On;

   function Is_Unblock_Send_Queued
            (  Listener : Connections_Server
            )  return Boolean is
   begin
      return Listener.Unblock_Send;
   end Is_Unblock_Send_Queued;

   procedure Keep_On_Sending (Client : in out Connection) is
   begin
      Client.Dont_Block := True;
   end Keep_On_Sending;

   procedure On_Connected
             (  Listener : in out Connections_Server'Class;
                Client   : in out Connection'Class
             )  is
   begin
      Trace_Sending
      (  Listener.Factory.all,
         Client,
         False,
         ", connected"
      );
      Free (Client.Transport);
      if not Is_Opportunistic (Client) then
         Client.Transport :=
            Create_Transport
            (  Listener.Factory,
               Listener'Unchecked_Access,
               Client'Unchecked_Access
            );
      end if;
      Set (Listener.Read_Sockets, Client.Socket);
      if Client.Transport = null then -- No handshaking
         declare
            Saved : constant Session_State := Client.Session;
         begin
            Client.Session := Session_Connected;
            Connected (Client);
            Connected (Listener, Client);
            Client.Session := Session_Active;
            Activated (Client);
         exception
            when others =>
               if Client.Session in Session_Connected
                                 .. Session_Active
               then
                  Client.Session := Saved;
               end if;
               raise;
         end;
      else
         Client.Session := Session_Handshaking;
         Append
         (  Listener.Postponed,
            Client'Unchecked_Access,
            Listener.Postponed_Count
         );
      end if;
   end On_Connected;

   procedure On_Worker_Start (Listener : in out Connections_Server) is
   begin
      null;
   end On_Worker_Start;

   procedure Process
             (  Buffer    : in out Input_Buffer;
                Receiver  : in out Connection'Class;
                Data_Left : out Boolean
             )  is
      Last    : Stream_Element_Offset;
      Pointer : Stream_Element_Offset;
   begin
      while Has_Data (Buffer) loop
         if Buffer.Free_To_Read < Buffer.First_Read then
            --
            -- [XXXXXXXXXXXXXX              XXXXX]
            --   Free_To_Read |  First_Read |
            --
            if Buffer.First_Read > Buffer.Read'Last then
               --
               -- [XXXXXXXXXXXXXX                   ]
               --   Free_To_Read |        First_Read |
               --
               Buffer.First_Read := Buffer.Read'First; -- Wrap
               Last := Buffer.Free_To_Read - 1;
            else
               Last := Buffer.Read'Last;
            end if;
         else
            --
            -- [           XXXXXXXXX             ]
            --  First_Read |        | Free_To_Read
            --
            Last := Buffer.Free_To_Read - 1;
         end if;
         Pointer := Last + 1;
         Received
         (  Receiver,
            Buffer.Read (Buffer.First_Read..Last),
            Pointer
         );
         if Pointer < Buffer.First_Read or else Pointer > Last + 1 then
            Raise_Exception
            (  Layout_Error'Identity,
               (  "Subscript error, pointer "
               &  Image (Pointer)
               &  " out of range "
               &  Image (Buffer.First_Read)
               &  ".."
               &  Image (Last)
               &  "+"
            )  );
         elsif Pointer > Buffer.Read'Last then
            if Buffer.Free_To_Read <= Buffer.Read'Last then
               Buffer.First_Read := Buffer.Read'First; -- Wrap
            else
               Buffer.First_Read := Pointer; -- Not yet
            end if;
         else
            Buffer.First_Read := Pointer;
         end if;
         if Pointer <= Last then -- Some input left unprocessed
            Data_Left := True;
            return;
         end if;
      end loop;
      Data_Left := False;
   end Process;

   procedure Process
             (  Listener  : in out Connections_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             )  is
   begin
      if Client.Transport = null then
         Process (Client.Read, Client.all, Data_Left);
      else
         Process
         (  Client.Transport.all,
            Listener,
            Client.all,
            Data_Left
         );
      end if;
   end Process;

   procedure Process_Packet (Client : in out Connection) is
   begin
      null;
   end Process_Packet;

   procedure Pull
             (  Buffer  : in out Input_Buffer;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Last   : Stream_Element_Offset;
      Offset : Stream_Element_Offset;
   begin
      while Pointer <= Data'Last and then Has_Data (Buffer) loop
         if Buffer.Free_To_Read < Buffer.First_Read then
            --
            -- [XXXXXXXXXXXXXX              XXXXX]
            --   Free_To_Read |  First_Read |
            --
            if Buffer.First_Read > Buffer.Read'Last then
               --
               -- [XXXXXXXXXXXXXX                   ]
               --   Free_To_Read |        First_Read |
               --
               Buffer.First_Read := Buffer.Read'First; -- Wrap
               Last := Buffer.Free_To_Read - 1;
            else
               Last := Buffer.Read'Last;
            end if;
         else
            --
            -- [           XXXXXXXXX             ]
            --  First_Read |        | Free_To_Read
            --
            Last := Buffer.Free_To_Read - 1;
         end if;
         Offset := Last - Buffer.First_Read;
         if Offset > Data'Last - Pointer then
            Offset := Data'Last - Pointer;
            Last   := Buffer.First_Read + Offset;
         end if;
         Data (Pointer..Pointer + Offset) :=
            Buffer.Read (Buffer.First_Read..Last);
         Pointer := Pointer + Offset + 1;
         if Last >= Buffer.Read'Last then
            if Buffer.Free_To_Read <= Buffer.Read'Last then
               Buffer.First_Read := Buffer.Read'First; -- Wrap
            else
               Buffer.First_Read := Last + 1; -- Not yet
            end if;
         else
            Buffer.First_Read := Last + 1;
         end if;
      end loop;
   end Pull;

   procedure Push
             (  Client : in out Connection;
                Data   : Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      if Client.Transport = null then
         Send_Socket (Client.Socket_Listener.all, Client, Data, Last);
      else
         Encode (Client.Transport.all, Client, Data, Last);
      end if;
      if Last + 1 /= Data'First then
         Client.Data_Sent := True;
         if (  0
            /= (  Client.Socket_Listener.Factory.Trace_Flags
               and
                  Trace_Decoded_Sent
            )  )
         then
            Trace_Sent
            (  Factory => Client.Socket_Listener.Factory.all,
               Client  => Client,
               Data    => Data,
               From    => Data'First,
               To      => Last,
               Encoded => False
            );
         end if;
      end if;
   end Push;

   procedure Queue
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Free   : Stream_Element_Offset;
      Count  : Stream_Element_Offset := Data'Last - Pointer + 1;
   begin
      if Buffer.First_Written = Buffer.Free_To_Write then
         --
         -- Moving  First_Written  as far back  as possible  to diminish
         -- buffer  fragmenting.  We cannot  move  it further  than  the
         -- number of elements we put there,  because of race condition,
         -- when Free_To_Write is not yet set.  But  when  Free_To_Write
         -- points into the elements written everything is OK
         --
         -- [   ............        ]
         --     |<--Count-->|
         --     |           Free_To_Write = First_Written
         --     new First_Written
         --
         Count :=
            Stream_Element_Offset'Min
            (  Buffer.Written'Length - 1,
               Count
            );
         Free := Stream_Element_Offset'Max
                 (  Buffer.Written'First,
                    Buffer.Free_To_Write - Count
                 );
         Buffer.Written (Free..Free + Count - 1) :=
            Data (Pointer..Pointer + Count - 1);
         Pointer := Pointer + Count;
         Buffer.First_Written := Free;
         Buffer.Free_To_Write := Free + Count;
         return;
      elsif Buffer.First_Written < Buffer.Free_To_Write then
         --
         -- [     XXXXXXXXXXXXXXX        ]
         --       |              |
         --       First_Written  Free_To_Write
         --
         Free :=
            (  Buffer.Written'Length
            -  Buffer.Free_To_Write
            +  Buffer.First_Written
            -  1  -- Last element is never written
            );
         if Free <= 0 then
            return;
         end if;
         declare
            Tail : constant Stream_Element_Offset :=
                   Stream_Element_Offset'Min
                   (  Buffer.Written'Last - Buffer.Free_To_Write + 1,
                      Free
                   );
         begin
            if Count <= Tail then -- Can queue all Count elements
               Buffer.Written
               (  Buffer.Free_To_Write
               .. Buffer.Free_To_Write + Count - 1
               )  := Data (Pointer..Data'Last);
               Pointer := Data'Last + 1;
               Free := Buffer.Free_To_Write + Count;
               if Free > Buffer.Written'Last then
                  Buffer.Free_To_Write := Buffer.Written'First;
               else
                  Buffer.Free_To_Write := Free;
               end if;
               return;
            end if; -- Can queue only Tail elements
            Buffer.Written
            (  Buffer.Free_To_Write
            .. Buffer.Free_To_Write + Tail - 1
            )  := Data (Pointer..Pointer + Tail - 1);
            Pointer := Pointer + Tail;
            Count   := Count   - Tail;
            Free    := Free    - Tail;
            if Buffer.Free_To_Write + Tail > Buffer.Written'Last then
               Buffer.Free_To_Write := Buffer.Written'First;
            else
               Buffer.Free_To_Write := Buffer.Free_To_Write + Tail;
            end if;
         end;
      else
         --
         -- [XXXXX               XXXXXXXX]
         --       |              |
         --       Free_To_Write  First_Written
         --
         Free :=
            (  Buffer.First_Written
            +  Buffer.Free_To_Write
            -  1  -- Last element is never written
            );
      end if;
      if Free <= 0 then
         return;
      end if;
      Count := Stream_Element_Offset'Min (Count, Free);
      Buffer.Written
      (  Buffer.Free_To_Write
      .. Buffer.Free_To_Write + Count - 1
      ) := Data (Pointer..Pointer + Count - 1);
      Pointer := Pointer + Count;
      Buffer.Free_To_Write := Buffer.Free_To_Write + Count;
   end Queue;

   function Queued_To_Send (Client : Connection)
      return Stream_Element_Count is
   begin
      return Used (Client.Written);
   end Queued_To_Send;

   procedure Read
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class
             )  is
      Buffer : Input_Buffer renames Client.Read;
      Last   : Stream_Element_Offset;
   begin
      if Client.Overlapped_Read < Queued_To_Send (Client) then
         return; -- Not ready to read yet
      elsif Buffer.Free_To_Read < Buffer.First_Read then
         --
         -- [XXXXXXXXXXXXXX              XXXXX]
         --   Free_To_Read |  First_Read |
         --
         Last := Buffer.First_Read - 2;
         if Last <= Buffer.First_Read then -- Read buffer is full
            return;
         end if;
      else
         --
         -- [           XXXXXXXXX             ]
         --  First_Read |        | Free_To_Read
         --
         if (  Buffer.Free_To_Read - Buffer.First_Read
            >= Buffer.Read'Length
            )
         then -- Read buffer is full
            return;
         elsif Buffer.Free_To_Read > Buffer.Read'Last then -- Wrap
            Buffer.Free_To_Read := Buffer.Read'First;
            Last := Buffer.First_Read - 2;
         else
            Last := Buffer.Read'Last;
         end if;
      end if;
      Receive_Socket
      (  Client.Socket_Listener.all,
         Client,
         Buffer.Read (Buffer.Free_To_Read..Last),
         Last
      );
      Received
      (  Factory,
         Client,
         Buffer.Read,
         Buffer.Free_To_Read,
         Last
      );
      if Last = Buffer.Free_To_Read - 1 then -- Nothing read
         raise Connection_Error;
      end if;
      Buffer.Expected :=
         Stream_Element_Offset'Max
         (  Buffer.Expected - (Last - Buffer.Free_To_Read + 1),
            0
         );
      Buffer.Free_To_Read := Last + 1;
   exception
      when Error : Socket_Error | Layout_Error =>
         Receive_Error (Client, Error);
         raise Connection_Error;
   end Read;

   procedure Receive_Socket
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class;
                Data     : in out Stream_Element_Array;
                Last     : out Stream_Element_Offset
             )  is
   begin
      Receive_Socket (Client.Socket, Data, Last);
   end Receive_Socket;

   procedure Received
             (  Factory : in out Connections_Factory;
                Client  : in out Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset
             )  is
   begin
      if Client.Transport = null then
         if 0 /= (Factory.Trace_Flags and Trace_Decoded_Received) then
            Trace_Received
            (  Factory => Connections_Factory'Class (Factory),
               Client  => Client,
               Data    => Data,
               From    => From,
               To      => To,
               Encoded => False
            );
         end if;
      else
         if 0 /= (Factory.Trace_Flags and Trace_Encoded_Received) then
            Trace_Received
            (  Factory => Connections_Factory'Class (Factory),
               Client  => Client,
               Data    => Data,
               From    => From,
               To      => To,
               Encoded => True
            );
         end if;
      end if;
   end Received;

   procedure Reconnect (Client : in out Connection) is
   begin
      if Client.Socket = No_Socket then
         Raise_Exception
         (  Use_Error'Identity,
            "No connection"
         );
      elsif not Client.Client then
         Raise_Exception
         (  Mode_Error'Identity,
            "Server connection"
         );
      elsif Client.Session /= Session_Down then
         if Client.Socket_Listener /= null then
            Request_Disconnect
            (  Client.Socket_Listener.all,
               Client,
               True
            );
            return;
         end if;
         Client.Session := Session_Down;
      end if;
      Raise_Exception
      (  Status_Error'Identity,
         "Downed connection"
      );
   end Reconnect;

   procedure Received
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      raise Connection_Error;
   end Received;

   procedure Released (Client : in out Connection) is
   begin
      null;
   end Released;

   procedure Remove
             (  List  : in out Connection_Ptr;
                Item  : in out Connection'Class;
                Count : in out Integer
             )  is
   begin
      if Item.Successor /= null then
         Count := Count - 1;
         if List = Item.Successor.Predecessor then -- First in the list
            if List = List.Successor then -- The single item of the list
               List := null;
               Item.Successor := null;
               return;
            else
               List := Item.Successor;
            end if;
         end if;
         Item.Predecessor.Successor := Item.Successor;
         Item.Successor.Predecessor := Item.Predecessor;
         Item.Successor := null;
      end if;
   end Remove;

   procedure Receive_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             )  is
   begin
      null;
   end Receive_Error;

   procedure Request_Disconnect
             (  Listener  : in out Connections_Server;
                Client    : in out Connection'Class;
                Reconnect : Boolean
             )  is
   begin
      Client.Failed := True;
      if Reconnect then
         Client.Try_To_Reconnect := True;
         Client.Action_Request   := Reconnect_Connection;
      else
         Client.Try_To_Reconnect := False;
         Client.Action_Request   := Shutdown_Connection;
      end if;
      Client.Socket_Listener.Shutdown_Request := True;
      if Client.Socket_Listener.Doer /= null then
         Abort_Selector (Client.Socket_Listener.Selector);
      end if;
   end Request_Disconnect;

   procedure Save_Occurrence
             (  Client : in out Connection;
                Source : Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Client.Last_Error, Source);
   end Save_Occurrence;

   procedure Send
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Buffer : Output_Buffer renames Client.Written;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - 1 > Data'Last
         )  )
      then
         Raise_Exception (Layout_Error'Identity, "Subscript error");
      end if;
      Queue (Client, Data, Pointer);
      if Buffer.Free_To_Write /= Buffer.First_Written then
         Unblock_Send (Client.Socket_Listener.all, Client);
      end if;
   end Send;

   procedure Send
             (  Client  : in out Connection;
                Data    : String;
                Pointer : in out Integer
             )  is
      Buffer : Output_Buffer renames Client.Written;
   begin
      Pointer := Data'Last + 1;
      for Index in Data'Range loop
         if Used (Buffer) + 1 >= Buffer.Written'Length then
            Pointer := Index;
            exit;
         end if;
         Buffer.Written (Buffer.Free_To_Write) :=
            Stream_Element (Character'Pos (Data (Index)));
         if Buffer.Free_To_Write = Buffer.Written'Last then
            Buffer.Free_To_Write := Buffer.Written'First;
         else
            Buffer.Free_To_Write := Buffer.Free_To_Write + 1;
         end if;
      end loop;
      if Buffer.Free_To_Write /= Buffer.First_Written then
         Unblock_Send (Client.Socket_Listener.all, Client);
      end if;
   end Send;

   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      Fill_From_Stream
      (  Buffer  => Buffer,
         Stream  => Stream,
         Count   => Stream_Element_Count'Last,
         Reserve => 1,
         Last    => Last,
         Next    => Next,
         Done    => End_Of_Stream
      );
      Buffer.Free_To_Write := Next;
      if Buffer.Free_To_Write /= Buffer.First_Written then
         Unblock_Send (Client.Socket_Listener.all, Client);
      end if;
   end Send;

   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                Count  : in out Stream_Element_Count;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      Fill_From_Stream
      (  Buffer  => Buffer,
         Stream  => Stream,
         Count   => Count,
         Reserve => 1,
         Last    => Last,
         Next    => Next,
         Done    => End_Of_Stream
      );
      Count := Count - (Last + 1 - Buffer.Free_To_Write);
      Buffer.Free_To_Write := Next;
      if Buffer.Free_To_Write /= Buffer.First_Written then
         Unblock_Send (Client.Socket_Listener.all, Client);
      end if;
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             )  is
      Count : Stream_Element_Count := Stream_Element_Count'Last;
   begin
      Send
      (  Client        => Client,
         Stream        => Stream,
         Count         => Count,
         Reserve       => Reserve,
         Get_Prefix    => Get_Prefix,
         Get_Suffix    => Get_Suffix,
         End_Of_Stream => End_Of_Stream
      );
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      if Buffer.Free_To_Write = Buffer.First_Written then
         if Buffer.Written'Length <= Reserve then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Output buffer size"
               &  Stream_Element_Count'Image (Buffer.Written'Length)
               &  " is less than required"
               &  Stream_Element_Count'Image (Reserve + 1)
               &  " elements"
            )  );
         end if;
         Fill_From_Stream
         (  Buffer  => Buffer,
            Stream  => Stream,
            Count   => Count,
            Reserve => Reserve + 1,
            Last    => Last,
            Next    => Next,
            Done    => End_Of_Stream
         );
         Count := Count - (Last + 1 - Buffer.Free_To_Write);
         declare
            Header : constant Stream_Element_Array :=
                     Get_Prefix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
            Tail   : constant Stream_Element_Array :=
                     Get_Suffix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
         begin
            if Header'Length + Tail'Length > Reserve then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Prefix returns more than"
                  &  Stream_Element_Count'Image (Reserve)
                  &  " elements"
               )  );
            elsif Header'Length > 0 then
               Last := Buffer.First_Written;
               for Index in reverse Header'Range loop
                  if Last = Buffer.Written'First then
                     Last := Buffer.Written'Last;
                  else
                     Last := Last - 1;
                  end if;
                  Buffer.Written (Last) := Header (Index);
               end loop;
            end if;
            Buffer.First_Written := Last;
            Buffer.Free_To_Write := Next;
            if Tail'Length > 0 then
               Last := Tail'First;
               Queue (Client, Tail, Last);
            end if;
         end;
      else
         End_Of_Stream := False;
      end if;
      if Buffer.Free_To_Write /= Buffer.First_Written then
         Unblock_Send (Client.Socket_Listener.all, Client);
      end if;
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      if Buffer.Free_To_Write = Buffer.First_Written then
         if Buffer.Written'Length <= Reserve then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Output buffer size"
               &  Stream_Element_Count'Image (Buffer.Written'Length)
               &  " is less than required"
               &  Integer'Image (Reserve + 1)
               &  " elements"
            )  );
         end if;
         Fill_From_Stream
         (  Buffer  => Buffer,
            Stream  => Stream,
            Count   => Count,
            Reserve => Stream_Element_Count (Reserve) + 1,
            Last    => Last,
            Next    => Next,
            Done    => End_Of_Stream
         );
         Count := Count - (Last + 1 - Buffer.Free_To_Write);
         declare
            Header : constant String :=
                     Get_Prefix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
            Tail   : constant String :=
                     Get_Suffix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
         begin
            if Header'Length + Tail'Length > Reserve then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Prefix returns more than"
                  &  Integer'Image (Reserve)
                  &  " elements"
               )  );
            elsif Header'Length > 0 then
               Last := Buffer.First_Written;
               for Index in reverse Header'Range loop
                  if Last = Buffer.Written'First then
                     Last := Buffer.Written'Last;
                  else
                     Last := Last - 1;
                  end if;
                  Buffer.Written (Last) :=
                     Stream_Element (Character'Pos (Header (Index)));
               end loop;
            end if;
            Buffer.First_Written := Last;
            Buffer.Free_To_Write := Next;
            if Tail'Length > 0 then
               declare
                  Pointer : Integer := Tail'First;
               begin
                  Send (Client, Tail, Pointer);
               end;
            end if;
         end;
      else
         End_Of_Stream := False;
      end if;
      if Buffer.Free_To_Write /= Buffer.First_Written then
         Unblock_Send (Client.Socket_Listener.all, Client);
      end if;
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             )  is
      Count : Stream_Element_Count := Stream_Element_Count'Last;
   begin
      Send
      (  Client        => Client,
         Stream        => Stream,
         Count         => Count,
         Reserve       => Reserve,
         Get_Prefix    => Get_Prefix,
         Get_Suffix    => Get_Suffix,
         End_Of_Stream => End_Of_Stream
      );
   end Send;

   procedure Send_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             )  is
   begin
      null;
   end Send_Error;

   procedure Send_Socket
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class;
                Data     : Stream_Element_Array;
                Last     : out Stream_Element_Offset
             )  is
   begin
      Send_Socket (Client.Socket, Data, Last);
   end Send_Socket;

   procedure Sent (Client : in out Connection) is
   begin
      null;
   end Sent;

   procedure Set_Client_Data
             (  Client   : in out Connection;
                Address  : Sock_Addr_Type;
                Listener : Connections_Server_Ptr
             )  is
   begin
      if Client.Socket_Listener /= null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The client has a connections server set"
         );
      end if;
      Client.Client_Address  := Address;
      Client.Socket_Listener := Listener;
   end Set_Client_Data;

   procedure Set_Expected_Count
             (  Client : in out Connection;
                Count  : Stream_Element_Count
             )  is
   begin
      Client.Read.Expected := Count;
   end Set_Expected_Count;

   procedure Service_Postponed (Listener : in out Connections_Server) is
      Leftover  : Connection_Ptr;
      Client    : Connection_Ptr;
      Data_Left : Boolean;
   begin
      loop
         Client := Listener.Postponed;
         exit when Client = null;
         Remove
         (  Listener.Postponed,
            Client.all,
            Listener.Postponed_Count
         );
         begin
            Process
            (  Connections_Server'Class (Listener),
               Client,
               Data_Left
            );
            if Data_Left then
               Append (Leftover, Client, Listener.Postponed_Count);
            end if;
         exception
            when Connection_Error =>
               Stop (Listener, Client);
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Postponed service",
                  Error
               );
               Stop (Listener, Client);
         end;
      end loop;
      Listener.Postponed := Leftover;
   end Service_Postponed;

   procedure Set_Failed
             (  Client : in out Connection;
                Error  : Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Client.Last_Error, Error);
      Client.Failed := True;
   end Set_Failed;

   procedure Set_Overlapped_Size
             (  Client : in out Connection;
                Size   : Stream_Element_Count
             )  is
   begin
      Client.Overlapped_Read := Size;
   end Set_Overlapped_Size;

   procedure Shutdown (Client : in out Connection) is
   begin
      if Client.Session /= Session_Down then
         if Client.Socket_Listener = null then
            Client.Session := Session_Down;
         else
            Request_Disconnect
            (  Client.Socket_Listener.all,
               Client,
               False
            );
            Shutdown (Client.Socket_Listener.all, Client);
         end if;
      end if;
   end Shutdown;

   procedure Shutdown
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             )  is
   begin
      null;
   end Shutdown;

   procedure Stop
             (  Listener : in out Connections_Server'Class;
                Client   : in out Connection_Ptr
             )  is
      Old_Socket : constant Socket_Type := Client.Socket;
      Reconnect  : Boolean :=
                      Client.Action_Request /= Shutdown_Connection;
   begin
      Trace_Sending
      (  Listener.Factory.all,
         Client.all,
         False,
         ", dropping connection"
      );
      Client.Action_Request := Keep_Connection;
      Clear (Listener.Read_Sockets,    Client.Socket);
      Clear (Listener.Blocked_Sockets, Client.Socket);
      Clear (Listener.Ready_To_Read,   Client.Socket);
      Clear (Listener.Ready_To_Write,  Client.Socket);
      Clear (Listener.Write_Sockets,   Client.Socket);
      Clear (Listener.Ready_To_Write,  Client.Socket);
      Free (Client.Transport);
      if Client.Session in Session_Connected..Session_Busy then
         begin
            Disconnected (Listener, Client.all);
         exception
            when Connection_Error =>
               Reconnect := False;
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Disconnected (server)",
                  Error
               );
         end;
         begin -- Disconnected
            Client.Session := Session_Disconnected;
            Disconnected (Client.all);
         exception
            when Connection_Error =>
               Reconnect := False;
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Disconnected (client)",
                  Error
               );
         end;
      end if;
      if Client.Client then -- Try to reconnect
         if (  Reconnect
            and then
               not Listener.Finalizing
            and then
               Client.Try_To_Reconnect
            and then
               Client.Session /= Session_Down
            and then
               Client.Action_Request /= Shutdown_Connection
            )
         then
            begin
               Close (Client.Socket);
               declare
                  Option : Request_Type := (Non_Blocking_IO, True);
               begin
                  Create_Socket (Client.Socket);
                  Set_Socket_Option
                  (  Client.Socket,
                     Socket_Level,
                     (Reuse_Address, True)
                  );
               end;
               if Old_Socket /= Client.Socket then -- Move client
                  Put (Listener.Connections, Client.Socket, Client);
                  Put (Listener.Connections, Old_Socket,    null);
               end if;
               Set (Listener.Write_Sockets, Client.Socket);
               Do_Connect (Listener, Client);
               return;
            exception
               when Error : Socket_Error => -- Kill the object
                  Trace_Error
                  (  Listener.Factory.all,
                     "Reconnecting",
                     Error
                  );
            end;
         end if;
         Listener.Servers := Listener.Servers - 1;
      else
         Listener.Clients := Listener.Clients - 1;
      end if;
      Close (Client.Socket);
      Client.Session := Session_Down;
      Client.Failed  := False;
      Client.Action_Request := Keep_Connection;
      begin
         Downed (Listener, Client.all);
      exception
         when Error : others =>
            Trace_Error
            (  Listener.Factory.all,
               "Downed (server)",
               Error
            );
      end;
      begin
         Downed (Client.all);
      exception
         when Error : others =>
            Trace_Error
            (  Listener.Factory.all,
               "Downed (client)",
               Error
            );
      end;
      begin
         Released (Client.all);
      exception
         when others =>
            null;
      end;
      if Get (Listener.Connections, Old_Socket) = Client then
         Put (Listener.Connections, Old_Socket, null);
      end if;
      Client := null;
   exception
      when Error : others =>
         Trace_Error (Listener.Factory.all, "Stopping", Error);
         raise;
   end Stop;

   function To_Addr (Host : String) return Inet_Addr_Type is
   begin
      for Index in Host'Range loop
         case Host (Index) is
            when '.' | '0'..'9' =>
               null;
            when others =>
               return Addresses (Get_Host_By_Name (Host), 1);
         end case;
      end loop;
      return Inet_Addr (Host);
   end To_Addr;

   function To_String (Data : Stream_Element_Array) return String is
      Result : String (1..Data'Length);
      Index  : Integer := Result'First;
   begin
      for Item in Data'Range loop
         Result (Index) := Character'Val (Data (Item));
         Index := Index + 1;
      end loop;
      return Result;
   end To_String;

   procedure Trace
             (  Factory : in out Connections_Factory;
                Message : String
             )  is
      use Ada.Text_IO;
   begin
      if 0 /= (Factory.Trace_Flags and Standard_Output) then
         Put_Line (Message);
      end if;
      if Is_Open (Factory.Trace_File) then
         Put_Line (Factory.Trace_File, Message);
      end if;
   end Trace;

   procedure Trace_Error
             (  Factory    : in out Connections_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      Trace
      (  Connections_Factory'Class (Factory),
         Context & ": " & Exception_Information (Occurrence)
      );
   end Trace_Error;

   procedure Trace_Off (Factory : in out Connections_Factory) is
      use Ada.Text_IO;
   begin
      if Is_Open (Factory.Trace_File) then
         Close (Factory.Trace_File);
      end if;
      Factory.Trace_Flags :=
         Factory.Trace_Flags and not Standard_Output;
   end Trace_Off;

   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Received : IO_Tracing_Mode := Trace_None;
                Sent     : IO_Tracing_Mode := Trace_None
             )  is
      Flags : Factory_Flags := Standard_Output;
   begin
      case Received is
         when Trace_Any =>
            Flags := Flags
                  or Trace_Decoded_Received
                  or Trace_Encoded_Received;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Received;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Received;
         when Trace_None =>
            null;
      end case;
      case Sent is
         when Trace_Any =>
            Flags := Flags or Trace_Decoded_Sent or Trace_Encoded_Sent;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Sent;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Sent;
         when Trace_None =>
            null;
      end case;
      Factory.Trace_Flags := Flags;
   end Trace_On;

   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Name     : String;
                Received : IO_Tracing_Mode := Trace_None;
                Sent     : IO_Tracing_Mode := Trace_None
             )  is
      use Ada.Text_IO;
      Flags : Factory_Flags := 0;
   begin
      if Is_Open (Factory.Trace_File) then
         Close (Factory.Trace_File);
      end if;
      Create (File => Factory.Trace_File, Name => Name);
      case Received is
         when Trace_Any =>
            Flags := Flags
                  or Trace_Decoded_Received
                  or Trace_Encoded_Received;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Received;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Received;
         when Trace_None =>
            null;
      end case;
      case Sent is
         when Trace_Any =>
            Flags := Flags or Trace_Decoded_Sent or Trace_Encoded_Sent;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Sent;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Sent;
         when Trace_None =>
            null;
      end case;
      Factory.Trace_Flags := Flags;
   end Trace_On;

   procedure Trace_Received
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
      This : Connections_Factory'Class renames
             Connections_Factory'Class (Factory);
   begin
      if Encoded then
         Trace
         (  This,
            (  Get_Client_Name (This, Client)
            &  " encoded> |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      else
         Trace
         ( This,
            (  Get_Client_Name (This, Client)
            &  " > |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      end if;
   end Trace_Received;

   procedure Trace_Sending
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Enabled : Boolean;
                Reason  : String
             )  is
      This : Connections_Factory'Class renames
             Connections_Factory'Class (Factory);
   begin
      if Enabled then
         Trace
         (  This,
            (  Get_Client_Name (This, Client)
            &  " < +++ Resume polling"
            &  Reason
         )  );
      else
         Trace
         (  This,
            (  Get_Client_Name (This, Client)
            &  " < --- Stop polling"
            &  Reason
         )  );
      end if;
   end Trace_Sending;

   procedure Trace_Sent
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
      This : Connections_Factory'Class renames
             Connections_Factory'Class (Factory);
   begin
      if Encoded then
         Trace
         (  This,
            (  Get_Client_Name (This, Client)
            &  " <encoded |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      else
         Trace
         (  This,
            (  Get_Client_Name (This, Client)
            &  " < |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      end if;
   end Trace_Sent;

   procedure Trace_Service_Loop
             (  Factory : in out Connections_Factory;
                Stage   : Service_Loop_Stage;
                Server  : in out Connections_Server'Class
             )  is
   begin
      null;
   end Trace_Service_Loop;

   procedure Unblock_Send
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             )  is
      Buffer : Output_Buffer renames Client.Written;
   begin
      if Buffer.Send_Blocked then -- Request socket unblocking
         Buffer.Send_Blocked   := False;
         Listener.Unblock_Send := True;
         if Listener.Doer /= null then
            Abort_Selector (Listener.Selector);
         end if;
      end if;
   end Unblock_Send;

   procedure Unblock_Send (Client : in out Connection) is
   begin
      Unblock_Send (Client.Socket_Listener.all, Client);
   end Unblock_Send;

   function Used (Buffer : Input_Buffer) return Stream_Element_Count is
      Diff : constant Stream_Element_Offset :=
             Buffer.Free_To_Read - Buffer.First_Read;
   begin
      if Diff < 0 then
         return Buffer.Read'Length - Diff;
      else
         return Diff;
      end if;
   end Used;

   function Used (Buffer : Output_Buffer) return Stream_Element_Count is
   begin
      if Buffer.Free_To_Write >= Buffer.First_Written then
         return Buffer.Free_To_Write - Buffer.First_Written;
      else
         return Buffer.Written'Length - Buffer.First_Written +
                Buffer.Free_To_Write;
      end if;
   end Used;

   procedure Write
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Next   : Stream_Element_Count;
   begin
      Blocked := Buffer.First_Written = Buffer.Free_To_Write;
      if Blocked then
         if Client.Dont_Block then
            Blocked           := False;
            Client.Data_Sent  := True;
            Client.Dont_Block := False;
         end if;
      else
         loop
            if Buffer.First_Written > Buffer.Free_To_Write then
               --
               -- [XXXXX               XXXXXXX]
               --       |              |
               --       Free_To_Write  First_Written
               --
               if Client.Transport = null then
                  Send_Socket
                  (  Client.Socket_Listener.all,
                     Client,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Written'Last
                     ),
                     Next
                  );
               else
                  Encode
                  (  Client.Transport.all,
                     Client,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Written'Last
                     ),
                     Next
                  );
               end if;
               Next := Next + 1;
               if Next = Buffer.First_Written then
                  exit; -- Cannot send anything right now
               elsif Next <= Buffer.Written'Last then
                  if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent)
                  then
                     Trace_Sent
                     (  Factory => Factory,
                        Client  => Client,
                        Data    => Buffer.Written,
                        From    => Buffer.First_Written,
                        To      => Next - 1,
                        Encoded => False
                     );
                  end if;
                  Buffer.First_Written := Next;
                  Client.Data_Sent := True;
                  exit;
               end if;
               if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent) then
                  Trace_Sent
                  (  Factory => Factory,
                     Client  => Client,
                     Data    => Buffer.Written,
                     From    => Buffer.First_Written,
                     To      => Next - 1,
                     Encoded => False
                  );
               end if;
               Buffer.First_Written := 0;
               Client.Data_Sent := True;
            else
               --
               -- [     XXXXXXXXXXXXXXX        ]
               --       |              |
               --       First_Written  Free_To_Write
               --
               if Client.Transport = null then
                  Send_Socket
                  (  Client.Socket_Listener.all,
                     Client,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Free_To_Write - 1
                     ),
                     Next
                  );
               else
                  Encode
                  (  Client.Transport.all,
                     Client,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Free_To_Write - 1
                     ),
                     Next
                  );
               end if;
               Next := Next + 1;
               if Next = Buffer.First_Written then
                  exit;
               elsif Next <= Buffer.Free_To_Write then
                  if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent)
                  then
                     Trace_Sent
                     (  Factory => Factory,
                        Client  => Client,
                        Data    => Buffer.Written,
                        From    => Buffer.First_Written,
                        To      => Next - 1,
                        Encoded => False
                     );
                  end if;
                  Buffer.First_Written := Next;
                  Client.Data_Sent := True;
                  exit;
               end if;
               if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent) then
                  Trace_Sent
                  (  Factory => Factory,
                     Client  => Client,
                     Data    => Buffer.Written,
                     From    => Buffer.First_Written,
                     To      => Next - 1,
                     Encoded => False
                  );
               end if;
               Buffer.First_Written := Next;
               Client.Data_Sent := True;
            end if;
            exit when Buffer.First_Written = Buffer.Free_To_Write;
         end loop;
      end if;
   end Write;

   task body Worker is
      Address       : Sock_Addr_Type :=
                      Get_Server_Address (Listener.all);
      Server_Socket : Socket_Type := No_Socket;
      Client_Socket : Socket_Type;
      That_Time     : Time := Clock;
      This_Time     : Time;
      Status        : Selector_Status;

      function Set_Image (Socket : Socket_Type) return String is
      begin
         return
         (  Image (Socket)
         &  ", listener"
         &  " read: "     & Image (Listener.Read_Sockets)
         &  ", write: "   & Image (Listener.Write_Sockets)
         &  ", blocked: " & Image (Listener.Blocked_Sockets)
         &  ", ready"
         &  " read: "     & Image (Listener.Ready_To_Read)
         &  ", write: "   & Image (Listener.Ready_To_Write)
         );
      end Set_Image;

      procedure Check (Sockets : Socket_Set_Type) is
         Socket : Socket_Type;
         List   : Socket_Set_Type := Sockets;
      begin
         loop
            Get (List, Socket);
            exit when Socket = No_Socket;
            if Socket /= Server_Socket then
               declare
                  Client : Connection_Ptr :=
                           Get (Listener.Connections, Socket);
               begin
                  if Client = null then
                     Trace
                     (  Listener.Factory.all,
                        (  "Missing client when checking socket "
                        &  Set_Image (Socket)
                     )  );
                     Clear (Listener.Read_Sockets,    Socket);
                     Clear (Listener.Write_Sockets,   Socket);
                     Clear (Listener.Blocked_Sockets, Socket);
                  elsif Client.Failed then
                     if (  Client.Action_Request = Keep_Connection
                        and then
                           (  Exception_Identity (Client.Last_Error)
                           /= Connection_Error'Identity
                        )  )
                     then
                        Trace_Error
                        (  Listener.Factory.all,
                           "Dropping connection on request",
                           Client.Last_Error
                        );
                     end if;
                     Stop (Listener.all, Client);
                  end if;
               end;
            end if;
         end loop;
      end Check;

      procedure Unblock (Requested_Only : Boolean) is
         Socket : Socket_Type;
      begin
         while not Listener.Finalizing loop
            Get (Listener.Blocked_Sockets, Socket);
            exit when Socket = No_Socket;
            if Socket /= Server_Socket then
               declare
                  Client : Connection_Ptr :=
                           Get (Listener.Connections, Socket);
               begin
                  if Client = null then
                     Trace
                     (  Listener.Factory.all,
                        (  "Missing client when unblocking socket "
                        &  Set_Image (Socket)
                     )  );
                     Clear (Listener.Read_Sockets,   Socket);
                     Clear (Listener.Write_Sockets,  Socket);
                     Clear (Listener.Ready_To_Write, Socket);
                  elsif Client.Failed then
                     if (  Client.Session /= Session_Down
                        and then
                           Client.Action_Request = Keep_Connection
                        and then
                           (  Exception_Identity (Client.Last_Error)
                           /= Connection_Error'Identity
                        )  )
                     then
                        Trace_Error
                        (  Listener.Factory.all,
                           "Unblocking socket",
                           Client.Last_Error
                        );
                     end if;
                     Stop (Listener.all, Client);
                  elsif (  Requested_Only
                        and then
                           Client.Written.Send_Blocked
                        )  then -- Keep it blocked
                     Set (Listener.Ready_To_Read, Client.Socket);
                  else -- Unblock
                     Set (Listener.Write_Sockets,  Client.Socket);
                     Set (Listener.Ready_To_Write, Client.Socket);
                     Status := Completed;  -- Make sure it written later
                     Client.Written.Send_Blocked := False;
                     Client.Data_Sent := True;
                     if (  0
                        /= (  Listener.Factory.Trace_Flags
                           and
                              (Trace_Encoded_Sent or Trace_Decoded_Sent)
                        )  )
                     then
                        if Requested_Only then
                           Trace_Sending
                           (  Listener.Factory.all,
                              Client.all,
                              True,
                              ", some data to send"
                           );
                        else
                           Trace_Sending
                           (  Listener.Factory.all,
                              Client.all,
                              True,
                              ", blocking timeout expired"
                           );
                        end if;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Unblock;

      Exit_Error : exception;
   begin
      On_Worker_Start (Listener.all);
      if Address.Port /= 0 then
         Create_Socket (Listener.all, Server_Socket, Address);
         if Server_Socket = No_Socket then
            raise Exit_Error;
         end if;
         Set (Listener.Read_Sockets, Server_Socket);
      end if;
      Listener.Request.Activate;
      loop
         Trace_Service_Loop
         (  Listener.Factory.all,
            Service_Loop_Begin,
            Listener.all
         );
         if Listener.Shutdown_Request then
            Listener.Shutdown_Request := False;
            Check (Listener.Read_Sockets);
         end if;
         if Listener.Connect_Request then
            declare
               Client : Connection_Ptr;
            begin
               loop
                  Listener.Request.Get (Client);
                  exit when Client = null;
                  Set (Listener.Write_Sockets, Client.Socket);
                  Put (Listener.Connections, Client.Socket, Client);
                  declare
                     use Object;
                     Ptr : Entity_Ptr := Client.all'Unchecked_Access;
                  begin
                     Release (Ptr);
                  end;
                  Listener.Servers := Listener.Servers + 1;
                  Do_Connect (Listener.all, Client);
               end loop;
            end;
         end if;
         Copy (Listener.Read_Sockets,  Listener.Ready_To_Read);
         Copy (Listener.Write_Sockets, Listener.Ready_To_Write);
         Check_Selector
         (  Selector     => Listener.Selector,
            R_Socket_Set => Listener.Ready_To_Read,
            W_Socket_Set => Listener.Ready_To_Write,
            Status       => Status,
            Timeout      => Listener.IO_Timeout
         );
         exit when Listener.Finalizing;
         if Status = Completed then
            Trace_Service_Loop
            (  Listener.Factory.all,
               Service_Loop_Reading,
               Listener.all
            );
            loop -- Reading from sockets
               Get (Listener.Ready_To_Read, Client_Socket);
               exit when Client_Socket = No_Socket;
               if Client_Socket = Server_Socket then
                  Accept_Socket
                  (  Server_Socket,
                     Client_Socket,
                     Address
                  );
                  declare
                     Client : Connection_Ptr;
                  begin
                     Client :=
                        Create (Listener.Factory, Listener, Address);
                     if Client = null then
                        Close (Client_Socket);
                     else
                        declare
                           This : Connection'Class renames Client.all;
                        begin
                           This.Client           := False;
                           This.Connect_No       := 0;
                           This.Client_Address   := Address;
                           This.Socket           := Client_Socket;
                           This.Try_To_Reconnect := False;
                           Clear (This);
                           This.Socket_Listener :=
                              Listener.all'Unchecked_Access;
                           Set (Listener.Read_Sockets,  Client_Socket);
                           Set (Listener.Write_Sockets, Client_Socket);
                           Put
                           (  Listener.Connections,
                              Client_Socket,
                              Client
                           );
                           Listener.Clients := Listener.Clients + 1;
                           if not Is_Opportunistic (This) then
                              This.Transport :=
                                 Create_Transport
                                 (  Listener.Factory,
                                    Listener,
                                    Client
                                 );
                           end if;
                           if This.Transport = null then -- Ready
                              This.Session := Session_Connected;
                              Connected (This);
                              Connected (Listener.all, This);
                              This.Session := Session_Active;
                              Activated (This);
                           else
                              This.Session := Session_Handshaking;
                           end if;
                        end;
                     end if;
                  exception
                     when Connection_Error =>
                        if Client /= null then
                           Stop (Listener.all, Client);
                        end if;
                     when Error : others =>
                        Trace_Error
                        (  Listener.Factory.all,
                           "Accept socket",
                           Error
                        );
                        if Client /= null then
                           Stop (Listener.all, Client);
                        end if;
                  end;
               else
                  declare
                     Client : Connection_Ptr :=
                              Get (Listener.Connections, Client_Socket);
                  begin
                     if Client = null then
                        Trace
                        (  Listener.Factory.all,
                           (  "Missing client when reading from socket "
                           &  Set_Image (Client_Socket)
                        )  );
                        Clear (Listener.Read_Sockets,    Client_Socket);
                        Clear (Listener.Write_Sockets,   Client_Socket);
                        Clear (Listener.Blocked_Sockets, Client_Socket);
                        Clear (Listener.Ready_To_Write,  Client_Socket);
                     elsif Client.Failed then
                        if (  Client.Session /= Session_Down
                           and then
                              Client.Action_Request = Keep_Connection
                           and then
                              (  Exception_Identity (Client.Last_Error)
                              /= Connection_Error'Identity
                           )  )
                        then
                           Trace_Error
                           (  Listener.Factory.all,
                              "Preparing to receive",
                              Client.Last_Error
                           );
                        end if;
                        Stop (Listener.all, Client);
                     else
                        begin
                           Read (Client.all, Listener.Factory.all);
                        exception
                           when Connection_Error =>
                              Stop (Listener.all, Client);
                           when Error : Socket_Error =>
                              Send_Error (Client.all, Error);
                              Stop (Listener.all, Client);
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Receive socket",
                                 Error
                              );
                              Stop (Listener.all, Client);
                        end;
                        declare
                           Data_Left : Boolean;
                        begin
                           if Client /= null then
                              Process (Listener.all, Client, Data_Left);
                              if Data_Left then
                                 Append
                                 (  Listener.Postponed,
                                    Client,
                                    Listener.Postponed_Count
                                 );
                              end if;
                           end if;
                        exception
                           when Connection_Error =>
                              Stop (Listener.all, Client);
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Processing received",
                                 Error
                              );
                              Stop (Listener.all, Client);
                        end;
                     end if;
                  end;
               end if;
            end loop;
         else
            Empty (Listener.Ready_To_Read); -- Clear the set
         end if;
         Trace_Service_Loop
         (  Listener.Factory.all,
            Service_Loop_Unblocking,
            Listener.all
         );
         This_Time := Clock;
         if This_Time - That_Time > Listener.Polling_Timeout then
            -- Unblock everything now
            That_Time := This_Time;
            Unblock (False);
            if (  Server_Socket /= No_Socket
               and then
                  not Is_Set (Listener.Read_Sockets, Server_Socket)
               )
            then
               Trace
               (  Listener.Factory.all,
                  "Server socket fell out of the read sockets list"
               );
               Set (Listener.Read_Sockets, Server_Socket);
            end if;
         else
            -- Checking for explicit unblocking requests
            while Listener.Unblock_Send loop
               Listener.Unblock_Send := False;
               Unblock (True); -- Still blocked are now in Ready_To_Read
               Copy (Listener.Ready_To_Read, Listener.Blocked_Sockets);
               Empty (Listener.Ready_To_Read); -- Clear the set
            end loop;
         end if;
         if Status = Completed then
            Trace_Service_Loop
            (  Listener.Factory.all,
               Service_Loop_Writing,
               Listener.all
            );
            loop -- Writing sockets
               Get (Listener.Ready_To_Write, Client_Socket);
               exit when Client_Socket = No_Socket;
               if Client_Socket /= Server_Socket then
                  declare
                     Client : Connection_Ptr :=
                              Get (Listener.Connections, Client_Socket);
                  begin
                     if Client = null then
                        Trace
                        (  Listener.Factory.all,
                           (  "Missing client when writing to socket "
                           &  Set_Image (Client_Socket)
                        )  );
                        Clear (Listener.Read_Sockets,    Client_Socket);
                        Clear (Listener.Write_Sockets,   Client_Socket);
                        Clear (Listener.Blocked_Sockets, Client_Socket);
                     elsif Client.Failed then
                        if (  Client.Session /= Session_Down
                           and then
                              Client.Action_Request = Keep_Connection
                           and then
                              (  Exception_Identity (Client.Last_Error)
                              /= Connection_Error'Identity
                           )  )
                        then
                           Trace_Error
                           (  Listener.Factory.all,
                              "Preparing to send",
                              Client.Last_Error
                           );
                        end if;
                        Stop (Listener.all, Client);
                     elsif Client.Session = Session_Connecting then
                        declare
                            This : Connection'Class renames Client.all;
                            Code : constant Error_Type :=
                                      Get_Socket_Option
                                      (  Client.Socket,
                                         Socket_Level,
                                         Error
                                      ) .Error;
                        begin
                           if Code = Success then -- Connected
                              On_Connected (Listener.all, This);
                              Set
                              (  Listener.Read_Sockets,
                                 Client_Socket
                              );
                           else -- Connect error
                              Trace_Sending
                              (  Listener.Factory.all,
                                 This,
                                 False,
                                 (  ", failed to connect to "
                                 &  Image (This.Client_Address)
                                 &  ": " & Image (Code)
                              )  );
                              Connect_Error (This, Code);
                              Do_Connect (Listener.all, Client);
                           end if;
                        exception
                           when Connection_Error =>
                              if Client /= null then
                                 Client.Try_To_Reconnect := False;
                                 Stop (Listener.all, Client);
                              end if;
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Connect socket",
                                 Error
                              );
                              if Client /= null then
                                 Client.Try_To_Reconnect := False;
                                 Stop (Listener.all, Client);
                              end if;
                        end;
                     else -- Have space to write
                        declare
                           Block : Boolean;
                        begin
                           Write
                           (  Client.all,
                              Listener.Factory.all,
                              Block
                           );
                           if (  Block
                              and then
                                 not Client.Written.Send_Blocked
                              )
                           then
                              Client.Written.Send_Blocked := True;
                              Set
                              (  Client.Socket_Listener.Blocked_Sockets,
                                 Client.Socket
                              );
                              Clear
                              (  Client.Socket_Listener.Write_Sockets,
                                 Client.Socket
                              );
                              if (  0
                                 /= (  Listener.Factory.Trace_Flags
                                    and
                                       (  Trace_Encoded_Sent
                                       or Trace_Decoded_Sent
                                 )  )  )
                              then
                                 Trace_Sending
                                 (  Listener.Factory.all,
                                    Client.all,
                                    False,
                                    ", nothing to send"
                                 );
                              end if;
                           end if;
                        exception
                           when Connection_Error =>
                              Stop (Listener.all, Client);
                           when Error : Socket_Error =>
                              Send_Error (Client.all, Error);
                              Stop (Listener.all, Client);
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Send socket",
                                 Error
                              );
                              Stop (Listener.all, Client);
                           end;
                        begin
                           if Client /= null and then Client.Data_Sent
                           then
                              Data_Sent (Listener.all, Client);
                           end if;
                        exception
                           when Connection_Error =>
                              Stop (Listener.all, Client);
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Processing sent notification",
                                 Error
                              );
                              Stop (Listener.all, Client);
                        end;
                     end if;
                  end;
               end if;
            end loop;
         else
            Empty (Listener.Ready_To_Write); -- Clear the set
         end if;
         exit when Listener.Finalizing;
         Trace_Service_Loop
         (  Listener.Factory.all,
            Service_Loop_Postponed,
            Listener.all
         );
         Service_Postponed (Listener.all);
      end loop;
      declare
         Client : Connection_Ptr;
      begin
         loop
            Listener.Request.Get (Client);
            exit when Client = null;
            declare
               use Object;
               Ptr : Entity_Ptr := Client.all'Unchecked_Access;
            begin
               Release (Ptr);
            end;
         end loop;
      end;
      Close (Server_Socket);
      Trace (Listener.Factory.all, "Worker task exiting");
   exception
      when Exit_Error =>
         Trace (Listener.Factory.all, "Worker task exiting");
      when Error : others =>
         Close (Server_Socket);
         Trace_Error (Listener.Factory.all, "Worker task", Error);
   end Worker;

   protected body Box is
      entry Connect (Client : Connection_Ptr)
         when Active and then Pending = null is
      begin
         if Client /= null then
            Pending := Client;
            Client.Socket_Listener.Connect_Request := True;
            Abort_Selector (Listener.Selector);
         end if;
      end Connect;

      procedure Activate is
      begin
          Active := True;
      end Activate;

      procedure Get (Client : out Connection_Ptr) is
      begin
         if Pending = null then
            Client := null;
         else
            Client  := Pending;
            Pending := null;
            Client.Socket_Listener.Connect_Request := False;
         end if;
      end Get;
   end Box;

end GNAT.Sockets.Server;

--                                                                    --
--  package GNAT.Sockets.Server     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  14:52 29 Feb 2020  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Streams;     use Ada.Streams;

with Ada.Finalization;
with Ada.Text_IO;
with Object.Handle.Generic_Unbounded_Array;
with Strings_Edit.Integer_Edit;

package GNAT.Sockets.Server is
   Connection_Error : exception;

   subtype Buffer_Length is
      Stream_Element_Offset range 1..Stream_Element_Offset'Last;
--
-- IO_Tracing_Mode -- Tracing incoming and outgoing data
--
   type IO_Tracing_Mode is
        (  Trace_None,    -- Don't trace anything
           Trace_Encoded, -- Trace encoded/ciphered data
           Trace_Any,     -- Trace all data
           Trace_Decoded  -- Trace plain data
        );
--
-- Session_State -- Of a connection
--
   type Session_State is
        (  Session_Down,         -- Not in use
           Session_Disconnected, -- Not connected
           Session_Connecting,   -- Pending connection to a remote host
           Session_Handshaking,  -- Pending handshake
           Session_Connected,    -- Connected
           Session_Active,       -- Connected and operating
           Session_Busy          -- Pending modal operation
        );
--
-- To_Addr -- Convert host name or dotted address to address
--
--    Host - The host name or address
--
-- Returns :
--
--    The corresponding address
--
-- Exceptions :
--
--    Socket_Error - Invalid address or host name
--
   function To_Addr (Host : String) return Inet_Addr_Type;
--
-- Connection -- To derive custom object handling a client connection
--
--    Input_Size  - Input buffer size
--    Output_Size - Output buffer size
--
-- The input buffer determines  maximal  number  of octets received from
-- the client  in one piece.  Usually  it is around  the average  packet
-- size, though one  must not expect that packets will be delivered as a
-- whole, per single call to Received.  The output buffer is the maximal
-- number of octets  which can be sent in one piece  without  postponing
-- sending.
--
   type Connection
        (  Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is abstract new Object.Entity with private;
   type Connection_Ptr is access all Connection'Class;
--
-- Connections_Factory -- Factory of client connection objects
--
   type Connections_Factory is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Encoder -- The transport layer, connection is encoded/ciphered
--
--    Size - The decoded content buffer size
--
   type Encoder (Size : Buffer_Length) is abstract
      new Ada.Finalization.Limited_Controlled with private;
   type Encoder_Ptr is access all Encoder'Class;
--
-- Server -- To derive a custom multiple connections server from
--
--    Port - The port to listen
--
   type Connections_Server
        (  Factory : access Connections_Factory'Class;
           Port    : Port_Type
        )  is new Ada.Finalization.Limited_Controlled with private;
   type Connections_Server_Ptr is access all Connections_Server'Class;
--
-- Activated -- Session activation notification
--
--    Client - The client connection object
--
-- This procedure  is  called  when  the session  becomes  active  after
-- handshake  completion.   From here  a server or  client  can initiate
-- communication with the pier. The default implementation does nothing.
--
   procedure Activated (Client : in out Connection);
--
-- Available_To_Process -- Stream elements available to process
--
--    Client - The client connection object
--
-- Returns :
--
--    Count of received but not yet processed stream elements
--
   function Available_To_Process (Client : Connection)
      return Stream_Element_Count;
--
-- Available_To_Send -- Stream elements available to send
--
--    Client - The client connection object
--
-- The result is the maximum  number of stream  elements  which  Send is
-- guaranteed to accept.  Larger number may cause Send returning Pointer
-- less or equal to Data'Last.
--
-- Returns :
--
--    Stream elements count
--
   function Available_To_Send (Client : Connection)
      return Stream_Element_Count;
--
-- Connect -- Connect to a server
--
--    Listener       - The server object
--    Client         - The client connection object
--    Host           - The host name or IP address
--    Port           - The port number
--    Max_Connect_No - Maximal number of connection attempts
--    Overlapped     - The overlapped read size (full-duplex by default)
--
-- This procedure  is used  to create  a client  connection to a server.
-- This is opposite to accepting connection. Connection is asynchronous.
-- When successful, Connected  is called  as usual.  When the connection
-- is dropped  Disconnect is called and  an attempt made  to  reconnect.
-- Note that the connection object is passed by pointer an maintained by
-- Listener.
--
-- Exceptions :
--
--    Host_Error   - Invalid host name
--    Socket_Error - Socket error
--    Use_Error    - Client is already in use
--
   procedure Connect
             (  Listener       : in out Connections_Server;
                Client         : Connection_Ptr;
                Host           : String;
                Port           : Port_Type;
                Max_Connect_No : Positive := Positive'Last;
                Overlapped     : Stream_Element_Count :=
                                 Stream_Element_Count'Last
             );
--
-- Connect_Error -- Failed to connect
--
--    Client - The client connection object
--    Error  - The socket error
--
-- This  procedure  is called on a connection  object  if  an attempt to
-- connect to a remote host initiated by Connect failed. Next connection
-- attempt  will be made after  return from the procedure.  The  default
-- implementation does nothing.
--
-- Exceptions :
--
--    Connection_Error - Stop attempts and collect the object
--
   procedure Connect_Error
             (  Client : in out Connection;
                Error  : Error_Type
             );
--
-- Connect_Parameters_Set -- Notification
--
--    Client         - The client connection object
--    Host           - Remote host name or address as given in Connect
--    Address        - Remote host address
--    Max_Connect_No - Maximal number of connection attempts
--
-- This  procedure  is called when connection  parameters are set for an
-- outgoing  connection.  The  default  implementation does nothing.  If
-- overridden,  the new implementation should probably call the parent's
-- implementation.
--
   procedure Connect_Parameters_Set
             (  Client         : in out Connection;
                Host           : String;
                Address        : Sock_Addr_Type;
                Max_Connect_No : Positive
             );
--
-- Connected -- The first operation called on a connection object
--
--    Client - The client connection object
--
-- The default  implementation  does nothing.  Typically  the server may
-- set  some  socket options here.  The  implementation  must  call  the
-- parent's implementation of this procedure from the overriding.
--
-- Exceptions :
--
--    Connection_Error - Is propagated when decided to refuse connection
--    other            - Other errors
--
   procedure Connected (Client : in out Connection);
--
-- Connected -- Server notification about client connection
--
--    Listener - The server object
--    Client   - The client object
--
-- The server may do some bookkeeping here.  It is called after Client's
-- Connected is. The default implementation does nothing.
--
-- Exceptions :
--
--    Connection_Error - Is propagated when decided to refuse connection
--    other            - Other errors
--
   procedure Connected
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             );
--
-- Create -- Client connection object
--
--    Factory  - The factory object
--    Listener - The server object
--    From     - The client address
--
-- This function is  called  to accept  connection  from  a client.  The
-- implementation  may refuse connection  in which case it returns null.
-- When  connection  is  accepted  the  implementation  allocates  a new
-- connection  object and returns a pointer to it.  Note that  it is the
-- server object responsibility to free the object.  The  implementation
-- may deploy client  filtering based on the address From and/or  number
-- of connections. The default implementation returns null.
--
-- Returns :
--
--    Pointer to the connection object or null
--
   function Create
            (  Factory  : access Connections_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
--
-- Create_Socket -- Listener socket creation
--
--    Listener - The server object
--    Socket   - The socket to create
--    Address  - The address to bind the socket to
--
-- This procedure  is called to create the socket the server will listen
-- to.  The default implementation creates the socket, sets socket reuse
-- option, binds the socket to Address, listens to the socket. If Socket
-- remains No_Socket the worker task of connection server will ends.
--
   procedure Create_Socket
             (  Listener : in out Connections_Server;
                Socket   : in out Socket_Type;
                Address  : Sock_Addr_Type
             );
--
-- Create_Transport -- Client connection object
--
--    Factory  - The factory object
--    Listener - The server object
--    Client   - The client for which the transport is created
--
-- This function is  called after  accepting connection from a client or
-- successfull connection  to a server to setup  transport  if  enoding/
-- ciphering is required. The returned object is owned by the server, it
-- is finalized and freed when no more used.  The default implementation
-- returns  null to indicate no encoding.
--
-- Returns :
--
--    Pointer to the encoder object or null
--
   function Create_Transport
            (  Factory  : access Connections_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr;
--
-- Create_Transport -- Continue with a transport layer
--
--    Client - The connection object
--
-- The connections  with  Is_Opportunistic  returning true  do not start
-- secure exchange until this procedure is called.
--
-- Exceptions :
--
--    Status_Error - The connection is already secured
--    Use_Error    - No secure secure connections support
--
   procedure Create_Transport (Client : in out Connection);
--
-- Disconnected -- Client disconnection notification
--
--    Client - The client object
--
-- The  default  implementation  does nothing.  If  overridden  the  new
-- implementation shall call the parent's one.
--
-- Exceptions :
--
--    Connection_Error - Do not reconnect
--
   procedure Disconnected (Client : in out Connection);
--
-- Disconnected -- Server notification about client disconnection
--
--    Listener - The server object
--    Client   - The client object being deleted
--
-- The server may do some bookkeeping here. It is called before Client's
-- Disconnected is.
--
-- Exceptions :
--
--    Connection_Error - Do not reconnect
--
   procedure Disconnected
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             );
--
-- Downed -- Client shutdown notification
--
--    Client - The client object
--
-- The default implementation does nothing.
--
   procedure Downed (Client : in out Connection);
--
-- Downed -- Server notification about client shutdown
--
--    Listener - The server object
--    Client   - The client object being deleted
--
-- The server may do some bookkeeping here. It is called before Client's
-- Downed is.
--
   procedure Downed
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             );
--
-- Elevated -- Notification of opportunistic transport creation
--
--    Client - The connection object
--
-- This procedure is called when an  opportunistic  transport  layer  is
-- created and ready to use. When  a  connection  with  Is_Opportunistic
-- returning True calls Create_Transport this creates a transport layer,
-- usually  encrypted,  and after successful handshake this procedure is
-- called.
--
   procedure Elevated (Client : in out Connection);
--
-- Encode -- Encode and send
--
--    Transport - The transport object
--    Client    - The client connection object
--    Data      - To encode
--    Last      - The last element encoded
--
-- This procedure  is called  when  the  transport  encoder is set.  The
-- implementation  must  encode  a portion  of  Data  and send it to the
-- client. Last is set to the last encoded element.
--
   procedure Encode
             (  Transport : in out Encoder;
                Client    : in out Connection'Class;
                Data      : Stream_Element_Array;
                Last      : out Stream_Element_Offset
             )  is abstract;
--
-- Finalize -- Destruction
--
--    Listener - The server object
--
-- The derived type must call this procedure when it overrides this.
--
   procedure Finalize (Listener : in out Connections_Server);
--
-- Finalize -- Destruction
--
--    Client - The connection object
--
-- The derived type must call this procedure when it overrides this.
--
   procedure Finalize (Client : in out Connection);
--
-- From_String -- Conversion from string
--
--    Data - To convert
--
-- Returns :
--
--    The result
--
   function From_String (Data : String) return Stream_Element_Array;
--
-- Get_Clients_Count -- Number of clients connected
--
--    Listener - The server object
--
-- Returns :
--
--    Number of connected clients
--
   function Get_Clients_Count (Listener : Connections_Server)
      return Natural;
--
-- Get_Client_Address -- Get address of the client
--
--    Client - The client connection object
--
-- Returns :
--
--    The client's address
--
   function Get_Client_Address (Client : Connection)
      return Sock_Addr_Type;
--
-- Get_Client_Name -- Get the name of the client
--
--    Factory - The factory object
--    Client  - The client connection object
--
-- The default implementation  uses IP address  returned by the function
-- Get_Client_Address for the client name.
--
-- Returns :
--
--    The client's name used in tracing
--
   function Get_Client_Name
            (  Factory : Connections_Factory;
               Client  : Connection'Class
            )  return String;
--
-- Get_Connections_Server -- Get client's connections server
--
--    Client - The client connection object
--
-- Returns :
--
--    A pointer to the connections server or null
--
   function Get_Connections_Server (Client : Connection)
      return Connections_Server_Ptr;
--
-- Get_IO_Timeout -- The I/O timeout used by connections server
--
--    Factory - The factory object
--
-- When the connections server  waits for a socket to become readable or
-- writable  this value specifies waiting timeout.  Upon the timeout the
-- server re-enters the waiting.  The default value is 20ms.  It  can be
-- changed by overriding this function.
--
-- Returns :
--
--    The timeout when waiting for sockets events
--
   function Get_IO_Timeout (Factory : Connections_Factory)
      return Duration;
--
-- Get_Occurrence -- Get saved client error
--
--    Client - The client connection object
--    Source - Saved exception occurence
--
   procedure Get_Occurrence
             (  Client : Connection;
                Source : out Exception_Occurrence
             );
--
-- Get_Overlapped_Size -- Get read policy
--
--    Client - The client connection object
--
-- See Set_Overlapped_Size.
--
-- Returns :
--
--    Maximum number of elements queued to send before blocking receive
--
   function Get_Overlapped_Size (Client : Connection)
      return Stream_Element_Count;
--
-- Get_Polling_Timeout -- The polling timeout used by connections server
--
--    Factory - The factory object
--
-- The connections  server stops polling a socket for being writable for
-- no longer than the value returned by this function. The default value
-- is 0.5s. The function can be overridden in order to change the value.
--
-- Returns :
--
--    The timeout before unblocking sending
--
   function Get_Polling_Timeout (Factory : Connections_Factory)
      return Duration;
--
-- Get_Server_Address -- Get the server socket address
--
--    Listener - The server object
--
-- This function is called  before  the  server  starts  listening.  The
-- result  is the address to listen.  The default implementation returns
-- an addressof  the  INET  family with  any  address and the port taken
-- from the argument's port discriminant.
--
-- Returns :
--
--    The address to listen
--
   function Get_Server_Address
            (  Listener : Connections_Server
            )  return Sock_Addr_Type;
--
-- Get_Session_State -- Session state
--
--    Client - The client connection object
--
-- Returns :
--
--    Current state
--
   function Get_Session_State (Client : Connection)
      return Session_State;
--
-- Get_Socket -- Get the socket
--
--    Client - The client connection object
--
-- Returns :
--
--    The socket used
--
   function Get_Socket (Client : Connection) return Socket_Type;
--
-- Has_Data -- Check if there is input data ready to process
--
--    Client - The client connection object
--
-- Returns :
--
--    True if the are data to process
--
   function Has_Data (Client : Connection) return Boolean;
--
-- Image -- Conversion to string
--
--    Code - Socket error type
--
-- Returns :
--
--    Textual representation of
--
   function Image (Code : Error_Type) return String;
--
-- Image -- Printable representation of stream array
--
--    Data        - To convert
--    Hexadecimal - If true HH HH HH form is used intead of cc%hhc%hh
--
-- The default output looks like:
--
--    source line%0D%0A
--
-- The hexadecimal output of the same data is
--
--    73 6F 75 72 63 65 20 6C 69 6E 65 0D 0A
--
-- Returns :
--
--    Printable representation of
--
   function Image
            (  Data        : Stream_Element_Array;
               Hexadecimal : Boolean := False
            )  return String;
--
-- Initialize -- Construction
--
--    Listener - The server object
--
-- The derived  type must  call this  procedure from  its implementation
-- when it replaces it.
--
   procedure Initialize (Listener : in out Connections_Server);
--
-- Is_Connected -- Connection status
--
--    Client - The client connection object
--
-- Returns :
--
--    True if the client is connected
--
   function Is_Connected (Client : Connection) return Boolean;
--
-- Is_Down -- Connection object status
--
--    Client - The client connection object
--
-- This function  is used  to check  if the object  is still functional,
-- that whether  it is connected or else tries to connect.  When True is
-- returned an outgoing connection object can be reused,  e.g. connected
-- again.
--
-- Returns :
--
--    True if the client is down
--
   function Is_Down (Client : Connection) return Boolean;
--
-- Is_Elevated -- Check if connection uses a transport layer
--
--    Client - The client connection object
--
-- Returns :
--
--    True if a transport layer is present
--
   function Is_Elevated (Client : Connection) return Boolean;
--
-- Is_Incoming -- Connection type
--
--    Client - The client connection object
--
-- Returns :
--
--    True if client handles and incoming connection
--
   function Is_Incoming (Client : Connection) return Boolean;
--
-- Is_Opportunistic -- Secure connection type
--
--    Client - The client connection object
--
-- This function returns True if the  connection  deploys  opportunistic
-- security  encoding.  The  default is False, that means the connection
-- engages  security  transport  immediately once established. When True
-- the  exchange starts not secured and turns into secure mode only when
-- both  parties  agree  on  that.  An  example  of opportunistic secure
-- connection is SMTP's STARTTLS command.
--
-- Returns :
--
--    True if the connection deploys opportunistic security encoding
--
   function Is_Opportunistic (Client : Connection) return Boolean;
--
-- Is_TLS_Capable -- Secure connection type
--
--    Factory - The factory object
--
-- The default implementation returns False
--
-- Returns :
--
--    True if the factory can create TLS transport
--
   function Is_TLS_Capable
            (  Factory : Connections_Factory
            )  return Boolean;
--
-- Is_Trace_Received_On -- Tracing state
--
--    Factory - The factory object
--    Encoded - The type of content trace to check
--
-- Returns :
--
--    True if tracing received data is on
--
   function Is_Trace_Received_On
            (  Factory : Connections_Factory;
               Encoded : IO_Tracing_Mode
            )  return Boolean;
--
-- Is_Trace_Sent_On -- Tracing state
--
--    Factory - The factory object
--    Encoded - The type of content trace to check
--
-- Returns :
--
--    True if tracing sent data is on
--
   function Is_Trace_Sent_On
            (  Factory : Connections_Factory;
               Encoded : IO_Tracing_Mode
            )  return Boolean;
--
-- Is_Unblock_Send_Queued -- Tracing state
--
--    Listener - The connections server
--
-- Returns :
--
--    True if unblock send is queued
--
   function Is_Unblock_Send_Queued
            (  Listener : Connections_Server
            )  return Boolean;
--
-- Keep_On_Sending -- Delay stopping sending
--
--    Client - The client connection object
--
-- This procedure  is called  to hint  the connections  server  that  it
-- should  not stop polling the socket for being writable,  because some
-- content to send is about to come.
--
   procedure Keep_On_Sending (Client : in out Connection);
--
-- On_Worker_Start -- Worker task notification start
--
--    Listener - The server object
--
-- This procedure  is called when  the worker  task starts.  The default
-- implementation does nothing.
--
   procedure On_Worker_Start (Listener : in out Connections_Server);
--
-- Process -- Encoded data processing
--
--    Transport - The encoder object
--    Listener  - The server object
--    Client    - The connection client
--    Data_Left - Unprocessed encoded input left
--
-- This  procedure  is called  to handle  a portion  of incoming encoded
-- data.
--
   procedure Process
             (  Transport : in out Encoder;
                Listener  : in out Connections_Server'Class;
                Client    : in out Connection'Class;
                Data_Left : out Boolean
             )  is abstract;
--
-- Process_Packet -- Packet processing
--
--    Client - The connection client
--
-- This procedure  is  called  when all fields  of Client, e.g. with the
-- types derived from Data_Item have  been  received  from  the  client.
-- The default implementation does nothing.
--
   procedure Process_Packet (Client : in out Connection);
--
-- Queued_To_Send -- Stream elements queued to send
--
--    Client - The client connection object
--
-- Returns :
--
--    Stream elements count
--
   function Queued_To_Send (Client : Connection)
      return Stream_Element_Count;
--
-- Received -- Data received notification
--
--    Client  - The client connection object
--    Data    - The data
--    Pointer - First unprocessed element
--
-- This  procedure  is called  when  a portion  of data is read from the
-- socket. The parameter Pointer is Data'Last + 1.  It can be changed to
-- indicate the  first  unprocessed  element.  Data (Pointer..Data'Last)
-- stay  in  the buffer  until a next  call  to  Received.  The  default
-- implementation raises Connection_Error.
--
-- Exceptions :
--
--    Connection_Error - Propagated to close connection
--
   procedure Received
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Received -- Data received notification
--
--    Factory - The factory object
--    Client  - The client connection object
--    Data    - The data received
--    From    - The first received element in Data
--    To      - The las received element in Data
--
-- This  procedure  is called  when  a portion  of data is read from the
-- socket. The default implementation traces the input.
--
   procedure Received
             (  Factory : in out Connections_Factory;
                Client  : in out Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset
             );
--
-- Receive_Error -- Data receive error
--
--    Client     - The client connection object
--    Occurrence - The socket error
--
-- This  procedure  is called  upon  socket  receive  error  before  the
-- connection is dropped. The default implementation does nothing.
--
   procedure Receive_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             );
--
-- Reconnect -- Drop current connection and try to reconnect
--
--    Client - The client connection object
--
-- Exception :
--
--    Mode_Error   - A server or permanent connection
--    Status_Error - The client is down
--    Use_Error    - Never connected before
--
   procedure Reconnect (Client : in out Connection);
--
-- Released -- Notification
--
--    Client - The connection client
--
-- This procedure is called when Client is no more in use.  The  default
-- implementation does nothing.
--
   procedure Released (Client : in out Connection);
--
-- Save_Occurrence -- Save client error occurence
--
--    Client - The client connection object
--    Source - The exception occurence to save
--
   procedure Save_Occurrence
             (  Client : in out Connection;
                Source : Exception_Occurrence
             );
--
-- Send -- Data to the client
--
--    Client  - The client connection object
--    Data    - The data to send
--    Pointer - The first element to send
--
-- This  procedure does not block.  When no send is pending  it sends as
-- much data the socket accepts without blocking. The rest is queued for
-- later which is also the case when some data  are pending.  The number
-- of elements  available  for queuing is returned by Available_To_Send.
-- The elements  which cannot  be queued  are Data (Pointer..Data'Last).
-- When this happens they should be kept until Send is called and passed
-- to Send again from there.
--
-- Exceptions :
--
--    Socket_Error - Send error
--    Layout_Error - Pointer is not in Data'First..Data'Last + 1
--
   procedure Send
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Send
             (  Client  : in out Connection;
                Data    : String;
                Pointer : in out Integer
             );
--
-- Send -- Data to the client
--
--    Client        - The client connection object
--    Stream        - To get data from to send
--  [ Count ]       - Maximal number of elements to send
--  [ Reserve       - Number of elements to reserve
--    Get_Prefix    - Called to get prefix
--    Get_Suffix ]  - Called to get suffix
--    End_Of_Stream - True if end of stream was reached
--
-- These procedures do not block. End_Of_Stream is set to False when the
-- caller should attempt later.  The procedures  read as  much data from
-- Stream as possible and send it to the client. The length of the chunk
-- is determined  by free contiguous space  in the output buffer and the
-- number  of  elements  returned  by  Stream.   The  parameter  Reserve
-- specifies the  maximum elements  to reserve in the  output buffer for
-- the prefix and suffix  returned by  Get_Prefix  and  Get_Suffix.  The
-- prefix  is sent  before and the suffix after the  chunk of  data read
-- from  Stream.  When the parameter Count is specified,  it limits  the
-- number  of elements sent.  The procedure  ends when  Count reaches 0.
-- When Stream  ends  prematurely  End_Of_Stream  is True  and  Count is
-- non-zero. Count value does not limit prefix and suffix when these are
-- used.
--
-- Exceptions :
--
--    Data_Error   - Output buffer is too small for prefix and suffix
--    Socket_Error - Send error
--
   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                End_Of_Stream : out Boolean
             );
   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                Count  : in out Stream_Element_Count;
                End_Of_Stream : out Boolean
             );
   type Create_Stream_Element_Array is access function
        (  Client : access Connection'Class;
           Data   : Stream_Element_Array;
           End_Of_Stream : Boolean
        )  return Stream_Element_Array;
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             );
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             );
   type Create_String is access function
        (  Client : access Connection'Class;
           Data   : Stream_Element_Array;
           End_Of_Stream : Boolean
        )  return String;
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             );
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             );
--
-- Send_Error -- Data send error
--
--    Client     - The client connection object
--    Occurrence - The socket error
--
-- This procedure is called upon socket send error before the connection
-- is dropped. The default implementation does nothing.
--
   procedure Send_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             );
--
-- Sent -- Data sent notification
--
--    Client - The client connection object
--
-- This procedure  is called  when some portion of data was successfully
-- sent leaving free space in the output buffer.  The implementation may
-- try to send data  pending  after the  last  call  to Send,  e.g.  the
-- contents  of  Data (Pointer..Data'Last).  The default  implementation
-- does nothing.
--
   procedure Sent (Client : in out Connection);
--
-- Set_Client_Data -- Set the client address and server
--
--    Client   - The connection object
--    Address  - The address
--    Listener - The client's handler
--
-- The procedure  is used  for  clients  running  over  a virtual socket
-- connections, e.g. WebSockets.  For conventional  clients address  and
-- handler are preset and this call raises Constraint_Error.
--
-- Exceptions :
--
--    Constraint_Error -- The client has a handler already
--
   procedure Set_Client_Data
             (  Client   : in out Connection;
                Address  : Sock_Addr_Type;
                Listener : Connections_Server_Ptr
             );
--
-- Set_Expected_Count -- Set minimal elements to read
--
--    Client - The client connection object
--    Count  - The number of elements to read before calling Received
--
-- This procedure  sets  the number  of elements  to  accumulate  before
-- calling Received.  When Count is 0 any number of read elements causes
-- a call to Received. When Count  is larger than the input buffer size,
-- Received is called for each full buffer. Note that Set_Expected_Count
-- has effect only once.  When all elements are read the count is set to
-- 0.
--
   procedure Set_Expected_Count
             (  Client : in out Connection;
                Count  : Stream_Element_Count
             );
--
-- Set_Overlapped_Size -- Read policy
--
--    Client - The client connection object
--    Size   - Maximum sent elements queued when read from socket
--
-- This procedure sets the read socket policy when there are  sent  data
-- pending.  The  parameter  Size  specifies  the maximum amount of data
-- queued for send without blocking read. The default value is 0 meaning
-- strictly  half-duplex  behavior.  That is when nothing is read before
-- the  client  accepts  all  data.  Typically  for  a   packet-oriented
-- protocol,  the  server  reads  a  packet  completely and then sends a
-- response packet or a set of packets back. Doing that it stops reading
-- new packets. To implement such policy Set_Overlapped_Size is set to 0
-- and the output buffer size is set to the maximum packet length.  This
-- would  guarantee  that Send called from Received would always be able
-- to queue a complete packet.
--
   procedure Set_Overlapped_Size
             (  Client : in out Connection;
                Size   : Stream_Element_Count
             );
--
-- Shutdown -- Request connection shutdown
--
--    Client - The client connection object
--
-- This procedure  is called to request connection shutdown.  The object
-- shall  not be  used afterwards.  As soon  as  the server  closes  the
-- connection the object is finalized and deallocated.
--
   procedure Shutdown (Client : in out Connection);
--
-- Shutdown -- Request connection shutdown notification
--
--    Listener - The server object
--    Client   - The client connection object
--
-- This  procedure is called when client requests shutdown.  The default
-- implementation does nothing.
--
-- Exceptions :
--
--    Mode_Error - The connection is permanent
--
   procedure Shutdown
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             );
--
-- Trace -- Tracing facility
--
--    Factory - The factory object
--    Message - Text to trace
--
-- This procedure is called to trace message.
--
   procedure Trace
             (  Factory : in out Connections_Factory;
                Message : String
             );
--
-- Trace_Error -- Error tracing
--
--    Factory    - The factory object
--    Context    - Text description of the error context
--    Occurrence - The error occurrence
--
-- This procedure  is called when  an unanticipated exception is caught.
-- The default implementation calls to Trace.
--
   procedure Trace_Error
             (  Factory    : in out Connections_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             );
--
-- Trace_Off -- Disable tracing
--
--    Factory - The factory object
--
   procedure Trace_Off (Factory : in out Connections_Factory);
--
-- Trace_On -- Enable tracing onto strandard output
--
--    Factory  - The factory object
--    Received - Trace incoming data
--    Sent     - Trace outgoing data
--
   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Received : IO_Tracing_Mode := Trace_None;
                Sent     : IO_Tracing_Mode := Trace_None
             );
--
-- Trace_On -- Enable tracing onto a file
--
--    Factory  - The factory object
--    Name     - The trace file name
--    Received - Trace incoming data
--    Sent     - Trace outgoing data
--
-- If  there is already trace file,  it is closed.  Then  the  procedure
-- opens or creates the file Name.
--
-- Exceptions :
--
--    I/O errors upon closing and opening files
--
   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Name     : String;
                Received : IO_Tracing_Mode := Trace_None;
                Sent     : IO_Tracing_Mode := Trace_None
             );
--
-- Trace_Received -- Tracing facility
--
--    Factory - The factory object
--    Client  - The client
--    Data    - The client's input buffer
--    From    - The first element received in the buffer
--    To      - The last element received in the buffer
--    Encoded - True if Data is encoded/ciphered
--
-- This procedure  is called when  tracing incoming data is active.  The
-- default implementation calls to Trace.
--
   procedure Trace_Received
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             );
--
-- Trace_Sending -- Tracing facility
--
--    Factory - The factory object
--    Client  - The client
--    Enabled - Polling socket for writing is enabled/disabled
--    Reason  - Of the action
--
-- This  procedure is  called when the socket is enabled or disabled for
-- polling.  Polling is  disabled  when  there  is nothing  to send  and
-- enabled  when  output buffer is filled.  The  default  implementation
-- calls to Trace.
--
   procedure Trace_Sending
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Enabled : Boolean;
                Reason  : String
             );
--
-- Trace_Sent -- Tracing facility
--
--    Factory - The factory object
--    Client  - The client
--    Data    - The client's output buffer
--    From    - The first element sent in the buffer
--    To      - The last element sent in the buffer
--    Encoded - True if Data is encoded/ciphered
--
-- This procedure is called  when tracing  outgoing data is active.  The
-- default implementation calls to Trace.
--
   procedure Trace_Sent
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             );
--
-- Service_Loop_Stage -- Phases of servicing connections
--
   type Service_Loop_Stage is
        (  Service_Loop_Begin,      -- Begin of the service loop
           Service_Loop_Reading,    -- Reading from ready sockets
           Service_Loop_Unblocking, -- Unblocking sockets with data
           Service_Loop_Writing,    -- Writing to ready sockets
           Service_Loop_Postponed   -- Servicing postponed requests
        );
--
-- Trace_Service_Loop -- Tracing facility
--
--    Factory - The factory object
--    Stage   - The service loop stage
--    Server  - The connections server
--
-- This procedure is called at different stages of the service loop. The
-- default implementation does nothing.
--
   procedure Trace_Service_Loop
             (  Factory : in out Connections_Factory;
                Stage   : Service_Loop_Stage;
                Server  : in out Connections_Server'Class
             );
--
-- To_String -- Conversion to string
--
--    Data - To convert
--
-- Returns :
--
--    The result
--
   function To_String (Data : Stream_Element_Array) return String;
--
-- Unblock_Send -- Request send socket polling
--
--    Listener - The server
--    Client   - The client connection object
--
-- Normally the socket polling is stopped when there is nothing to send.
-- It is resumed  once a portion of data is sent, e.g. a Send is called.
-- This procedure has the same effect without sending any data.
--
   procedure Unblock_Send
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             );
--
-- Unblock_Send -- Request send socket polling
--
--    Client - The client connection object
--
-- Normally the socket polling is stopped when there is nothing to send.
-- It is resumed  once a portion of data is sent, e.g. a Send is called.
-- This procedure has the same effect without sending any data.
--
   procedure Unblock_Send (Client : in out Connection);

   package Stream_Element_Offset_Edit is
      new Strings_Edit.Integer_Edit (Stream_Element_Offset);
   use Stream_Element_Offset_Edit;
------------------------------------------------------------------------
--
-- Internal low-level socket I/O operations
--
-- Process -- Input data processing
--
--    Listener  - The server
--    Client    - The client connection to handle
--    Data_Left - Unprocessed input left
--
-- This procedure  is used internally  to handle a portion  of  incoming
-- data. The implementation is aware of the transport encoding.
--
   procedure Process
             (  Listener  : in out Connections_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             );
--
-- Push -- Transport direct write
--
--    Client  - The client
--    Data    - To send
--    Last    - The last element sent
--
   procedure Push
             (  Client : in out Connection;
                Data   : Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Read -- Socket buffered read
--
--    Client  - The client
--    Factory - The factory object
--
   procedure Read
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class
             );
--
-- Receive_Socket -- Read data from the client's socket
--
--    Listener - The connections server
--    Client   - The client
--    Data     - To send
--    Last     - The last element sent, can be less than Data'Last
--
-- The default implementation calls socket's Read_Socket.
--
   procedure Receive_Socket
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class;
                Data     : in out Stream_Element_Array;
                Last     : out Stream_Element_Offset
             );
--
-- Request_Disconnect -- Engage disconnection
--
--    Listener  - The connections server
--    Client    - The client
--    Reconnect - True if reconnect again, else shutdown
--
-- This procedure implements object connection's operations Shutdown and
-- Reconnect. The default implementation of these call this procedure.
--
   procedure Request_Disconnect
             (  Listener  : in out Connections_Server;
                Client    : in out Connection'Class;
                Reconnect : Boolean
             );
--
-- Send_Socket -- Send data over client's socket
--
--    Listener - The connections server
--    Client   - The client
--    Data     - To send
--    Last     - The last element sent, can be less than Data'Last
--
-- The default implementation calls socket's Send_Socket.
--
   procedure Send_Socket
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class;
                Data     : Stream_Element_Array;
                Last     : out Stream_Element_Offset
             );
--
-- Set_Failed -- Mark connection as failed
--
--    Client - The client connection object
--    Error  - The error occurrence
--
-- This  procedure  is used to indicate  the connection  failed  from an
-- external task. Failed connections are closed as soon as possible.
--
   procedure Set_Failed
             (  Client : in out Connection;
                Error  : Exception_Occurrence
             );
--
-- Write -- Socket buffered write
--
--    Client  - The client
--    Factory - The factory object
--    Blocked - Nothing to send, should block writing
--
   procedure Write
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             );
private
   pragma Inline (Available_To_Process);
   pragma Inline (Available_To_Send);
   pragma Inline (Has_Data);
   pragma Inline (Queued_To_Send);
------------------------------------------------------------------------
--
-- Input_Buffer -- Input ring buffer
--
--    Size - Buffer size
--
   type Input_Buffer (Size : Buffer_Length) is record
      Expected     : Stream_Element_Offset := 0;
      First_Read   : Stream_Element_Offset := 0;
      Free_To_Read : Stream_Element_Offset := 0;
      Read         : Stream_Element_Array (0..Size);
      pragma Atomic (First_Read);
      pragma Atomic (Free_To_Read);
   end record;
--
-- Has_Data -- Buffer has data
--
--    Buffer - The buffer
--
-- Returns :
--
--    True if the buffer contains data and not blocked
--
   function Has_Data (Buffer : Input_Buffer) return Boolean;
--
-- Process -- Data from the buffer
--
--    Buffer    - The buffer
--    Receiver  - The data consumer (its Receive is called)
--    Data_Left - Set to True if some data are left in the buffer
--
   procedure Process
             (  Buffer    : in out Input_Buffer;
                Receiver  : in out Connection'Class;
                Data_Left : out Boolean
             );
--
-- Pull -- Get data from the buffer
--
--    Buffer  - The buffer
--    Data    - To store extracted data
--    Pointer - The first position to store data, advanced
--
   procedure Pull
             (  Buffer  : in out Input_Buffer;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Used -- Number of elements stored in the buffer
--
--    Buffer - The buffer
--
-- Returns :
--
--    Number of elements
--
   function Used (Buffer : Input_Buffer) return Stream_Element_Count;
------------------------------------------------------------------------
   type Output_Buffer (Size : Buffer_Length) is record
      First_Written : Stream_Element_Offset := 0;
      Free_To_Write : Stream_Element_Offset := 0;
      Send_Blocked  : Boolean := False;
      Written       : Stream_Element_Array (0..Size);
      pragma Atomic (First_Written);
      pragma Atomic (Free_To_Write);
      pragma Atomic (Send_Blocked);
   end record;
--
-- Fill_From_Stream -- Store stream output into the buffer
--
--    Buffer  - The buffer
--    Count   - Maximal number of elements to store
--    Reserve - Number of elements to reserve
--    Last    - Points to the last written element
--    Next    - Points to the element next to the last written
--    Done    - True if end of stream was reached
--
   procedure Fill_From_Stream
             (  Buffer  : in out Output_Buffer;
                Stream  : in out Root_Stream_Type'Class;
                Count   : Stream_Element_Count;
                Reserve : Stream_Element_Count;
                Last    : out Stream_Element_Offset;
                Next    : out Stream_Element_Offset;
                Done    : out Boolean
             );
--
-- Free -- Number of free elements in the buffer
--
--    Buffer - The buffer
--
-- Returns :
--
--    Number of elements
--
   function Free (Buffer : Output_Buffer) return Stream_Element_Count;
--
-- Used -- Number of elements stored in the buffer
--
--    Buffer - The buffer
--
-- Returns :
--
--    Number of elements
--
   function Used (Buffer : Output_Buffer) return Stream_Element_Count;
------------------------------------------------------------------------
   type Connection_Action_Type is
        (  Keep_Connection,
           Reconnect_Connection,
           Shutdown_Connection
        );
   type Connection
        (  Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is abstract new Object.Entity with
   record
      Socket           : Socket_Type            := No_Socket;
      Overlapped_Read  : Stream_Element_Count   := 0;
      Session          : Session_State          := Session_Down;
      Action_Request   : Connection_Action_Type := Keep_Connection;
      Failed           : Boolean := False;
      External_Action  : Boolean := False;
      Data_Sent        : Boolean := False;
      Dont_Block       : Boolean := False;
      Client           : Boolean := False;
      Try_To_Reconnect : Boolean := True;
      Connect_No       : Natural := 0;
      Max_Connect_No   : Natural := Natural'Last;
      Predecessor      : Connection_Ptr;
      Successor        : Connection_Ptr;
      Socket_Listener  : Connections_Server_Ptr;
      Transport        : Encoder_Ptr;
      Last_Error       : Exception_Occurrence;
      Client_Address   : Sock_Addr_Type;
      Read             : Input_Buffer  (Input_Size);
      Written          : Output_Buffer (Output_Size);
      pragma Atomic (Data_Sent);
      pragma Atomic (Connect_No);
      pragma Atomic (Failed);
      pragma Atomic (Dont_Block);
      pragma Atomic (Try_To_Reconnect);
      pragma Atomic (Session);
      pragma Atomic (Action_Request);
   end record;
--
-- On_Connected -- Start servicing a client socket
--
--    Listener - The server object
--    Client   - The client connection
--
   procedure On_Connected
             (  Listener : in out Connections_Server'Class;
                Client   : in out Connection'Class
             );
--
-- Do_Connect -- Initiate connection
--
--    Listener - The server object
--    Client   - The client connection
--
   procedure Do_Connect
             (  Listener : in out Connections_Server'Class;
                Client   : in out Connection_Ptr
             );
--
-- Data_Sent -- Data sent notification
--
--    Listener - The server object
--    Client   - The client connection to handle
--
-- This procedure is used internally to handle outgoing data.
--
   procedure Data_Sent
             (  Listener : in out Connections_Server;
                Client   : Connection_Ptr
             );

   procedure Queue
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Service_Postponed -- Postponed input data processing
--
--    Listener - The server object
--
   procedure Service_Postponed (Listener : in out Connections_Server);
--
-- Stop -- Stop servicing a socket
--
--    Listener - The server object
--    Client   - The client connection
--
   procedure Stop
             (  Listener : in out Connections_Server'Class;
                Client   : in out Connection_Ptr
             );

   package Connection_Handles is
      new Object.Handle (Connection, Connection_Ptr);
   --  use Connection_Handles;

   package Connection_Arrays is
      new Connection_Handles.Generic_Unbounded_Array
          (  Index_Type  => Socket_Type,
             Handle_Type => Connection_Handles.Handle
          );
   use Connection_Arrays;

   task type Worker (Listener : access Connections_Server'Class);
   type Worker_Ptr is access Worker;

   type Factory_Flags is mod 2**5;
   Standard_Output        : constant Factory_Flags := 2**0;
   Trace_Encoded_Sent     : constant Factory_Flags := 2**1;
   Trace_Decoded_Sent     : constant Factory_Flags := 2**2;
   Trace_Encoded_Received : constant Factory_Flags := 2**3;
   Trace_Decoded_Received : constant Factory_Flags := 2**4;

   type Connections_Factory is
      new Ada.Finalization.Limited_Controlled with
   record
      Trace_Flags : Factory_Flags := 0;
      Trace_File  : Ada.Text_IO.File_Type;

      pragma Atomic (Trace_Flags);
   end record;

   type Encoder (Size : Buffer_Length) is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Buffer  : Input_Buffer (Size);
   end record;

   protected type Box (Listener : access Connections_Server'Class) is
      entry Connect (Client : Connection_Ptr);
      procedure Get (Client : out Connection_Ptr);
      procedure Activate;
   private
      Pending : Connection_Ptr := null;
      Active  : Boolean        := False;
   end Box;

   type Connections_Server
        (  Factory : access Connections_Factory'Class;
           Port    : Port_Type
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Clients          : Natural := 0;
      Servers          : Natural := 0;
      Postponed_Count  : Natural := 0;
      Selector         : aliased Selector_Type;
      Read_Sockets     : Socket_Set_Type;
      Write_Sockets    : Socket_Set_Type;
      Blocked_Sockets  : Socket_Set_Type;
      Ready_To_Read    : Socket_Set_Type;
      Ready_To_Write   : Socket_Set_Type;
      Postponed        : Connection_Ptr;
      IO_Timeout       : Duration := 0.02;
      Polling_Timeout  : Duration := 0.5;
      Unblock_Send     : Boolean  := False;
      Shutdown_Request : Boolean  := False;
      Connect_Request  : Boolean  := False;
      Finalizing       : Boolean  := False;
      Connections      : Unbounded_Array;
      Doer             : Worker_Ptr;
      Request          : Box (Connections_Server'Unchecked_Access);
      pragma Atomic (Finalizing);
      pragma Atomic (Unblock_Send);
      pragma Atomic (Connect_Request);
      pragma Atomic (Shutdown_Request);
   end record;
--
-- Queue operations
--
   procedure Append
             (  List  : in out Connection_Ptr;
                Item  : Connection_Ptr;
                Count : in out Integer
             );
   procedure Remove
             (  List  : in out Connection_Ptr;
                Item  : in out Connection'Class;
                Count : in out Integer
             );
   pragma Inline (Append);
   pragma Inline (Available_To_Process);
   pragma Inline (Available_To_Send);
   pragma Inline (Has_Data);
   pragma Inline (Free);
   pragma Inline (Queued_To_Send);
   pragma Inline (Remove);
   pragma Inline (Used);

end GNAT.Sockets.Server;

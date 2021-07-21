--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Server                                 Winter, 2013       --
--  Interface                                                         --
--                                Last revision :  20:46 27 Aug 2020  --
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

with Ada.Calendar;                   use Ada.Calendar;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Task_Identification;        use Ada.Task_Identification;
with Strings_Edit.Time_Conversions;  use Strings_Edit.Time_Conversions;

with Ada.Finalization;
with Ada.Containers.Ordered_Sets; -- Changed to use standard library by J. Carter 2021
with GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
with Stack_Storage;
with Tables.Names;

package GNAT.Sockets.Connection_State_Machine.HTTP_Server is
--
-- HTTP_Method -- Methods
--
   type HTTP_Method is
        (  HTTP_GET,
           HTTP_HEAD,
           HTTP_POST,
           HTTP_PUT,
           HTTP_DELETE,
           HTTP_TRACE,
           HTTP_OPTIONS,
           HTTP_CONNECT,
           HTTP_PATCH
        );
   type HTTP_Allowed is array (HTTP_Method) of Boolean;
   type HTTP_Version is delta 0.1 digits 6 range 1.0..1_000.0;
--
-- Scheme_Type -- Recognized  schemes  (RFC  3986). Permanentl, historic
--                and provisional schemas registered to 2017-09-13
--
   type Scheme_Type is
        (  Aaa_Scheme,
           Aaas_Scheme,
           About_Scheme,
           Acap_Scheme,
           Acct_Scheme,
           Acr_Scheme,
           Adiumxtra_Scheme,
           AFP_Scheme,
           AFS_Scheme,
           Aim_Scheme,
           Appdata_Scheme,
           APT_Scheme,
           Attachment_Scheme,
           Aw_Scheme,

           Barion_Scheme,
           Beshare_Scheme,
           Bitcoin_Scheme,
           Blob_Scheme,
           Bolo_Scheme,
           Browserext_Scheme,

           Callto_Scheme,
           Cap_Scheme,
           Chrome_Scheme,
           Chrome_Extension_Scheme,
           Cid_Scheme,
           Coap_Scheme,
           Coap_Tcp_Scheme,
           Coaps_Scheme,
           Coaps_Tcp_Scheme,
           Com_Eventbrite_Attendee_Scheme,
           Content_Scheme,
           Crid_Scheme,
           CVS_Scheme,

           Data_Scheme,
           Dav_Scheme,
           Diaspora_Scheme,
           Dict_Scheme,
           DIS_Scheme,
           DLNA_Playcontainer_Scheme,
           DLNA_Playsingle_Scheme,
           DNS_Scheme,
           DNTP_Scheme,
           DTN_Scheme,
           DVB_Scheme,

           Ed2k_Scheme,
           Example_Scheme,

           Facetime_Scheme,
           Fax_Scheme,
           Feed_Scheme,
           Feedready_Scheme,
           File_Scheme,
           Filesystem_Scheme,
           Finger_Scheme,
           Fish_Scheme,
           FTP_Scheme,

           Geo_Scheme,
           Gg_Scheme,
           Git_Scheme,
           Gizmoproject_Scheme,
           Go_Scheme,
           Gopher_Scheme,
           Graph_Scheme,
           Gtalk_Scheme,

           H323_Scheme,
           Ham_Scheme,
           HCP_Scheme,
           HTTP_Scheme,
           HTTPS_Scheme,
           HXXP_Scheme,
           HXXPS_Scheme,
           Hydrazone_Scheme,

           Iax_Scheme,
           Icap_Scheme,
           Icon_Scheme,
           Im_Scheme,
           Imap_Scheme,
           Info_Scheme,
           Iotdisco_Scheme,
           IPN_Scheme,
           IPP_Scheme,
           IPPS_Scheme,
           IRC_Scheme,
           IRC6_Scheme,
           IRCS_Scheme,
           Iris_Scheme,
           Iris_Beep_Scheme,
           Iris_LWZ_Scheme,
           Iris_XPC_Scheme,
           Iris_XPCS_Scheme,
           Isostore_Scheme,
           ITMS_Scheme,

           Jabber_Scheme,
           Jar_Scheme,
           Jms_Scheme,

           Keyparc_Scheme,

           Lastfm_Scheme,
           LDAP_Scheme,
           LDAPS_Scheme,
           LVLT_Scheme,

           Magnet_Scheme,
           Mailserver_Scheme,
           Mailto_Scheme,
           Maps_Scheme,
           Market_Scheme,
           Message_Scheme,
           Mid_Scheme,
           MMS_Scheme,
           Modem_Scheme,
           Mongodb_Scheme,
           MOZ_Scheme,
           MS_Access_Scheme,
           MS_Browser_Extension_Scheme,
           MS_Drive_To_Scheme,
           MS_Enrollment_Scheme,
           MS_Excel_Scheme,
           MS_Gamebarservices_Scheme,
           MS_Getoffice_Scheme,
           MS_Help_Scheme,
           MS_Infopath_Scheme,
           MS_Inputapp_Scheme,
           MS_Media_Stream_ID_Scheme,
           MS_Officeapp_Scheme,
           MS_People_Scheme,
           MS_Project_Scheme,
           MS_Powerpoint_Scheme,
           MS_Publisher_Scheme,
           MS_Search_Repair_Scheme,
           MS_Secondary_Screen_Controller_Scheme,
           MS_Secondary_Screen_Setup_Scheme,
           MS_Settings_Scheme,
           MS_Settings_Airplanemode_Scheme,
           MS_Settings_Bluetooth_Scheme,
           MS_Settings_Camera_Scheme,
           MS_Settings_Cellular_Scheme,
           MS_Settings_Cloudstorage_Scheme,
           MS_Settings_Connectabledevices_Scheme,
           MS_Settings_Displays_Topology_Scheme,
           MS_Settings_Emailandaccounts_Scheme,
           MS_Settings_Language_Scheme,
           MS_Settings_Location_Scheme,
           MS_Settings_Lock_Scheme,
           MS_Settings_Nfctransactions_Scheme,
           MS_Settings_Notifications_Scheme,
           MS_Settings_Power_Scheme,
           MS_Settings_Privacy_Scheme,
           MS_Settings_Proximity_Scheme,
           MS_Settings_Screenrotation_Scheme,
           MS_Settings_WiFi_Scheme,
           MS_Settings_Workplace_Scheme,
           MS_SPD_Scheme,
           MS_Sttoverlay_Scheme,
           MS_Transit_To_Scheme,
           MS_Virtualtouchpad_Scheme,
           MS_Visio_Scheme,
           MS_Walk_To_Scheme,
           MS_Whiteboard_Scheme,
           MS_Whiteboard_CMD_Scheme,
           MS_Word_Scheme,
           MSnim_Scheme,
           MSRP_Scheme,
           MSRPS_Scheme,
           MTQP_Scheme,
           Mumble_Scheme,
           Mupdate_Scheme,
           MVN_Scheme,

           News_Scheme,
           NFS_Scheme,
           NI_Scheme,
           NIH_Scheme,
           NNTP_Scheme,
           Notes_Scheme,

           OCF_Scheme,
           OID_Scheme,
           Onenote_Scheme,
           Onenote_CMD_Scheme,
           Opaquelocktoken_Scheme,

           Pack_Scheme,
           Palm_Scheme,
           Paparazzi_Scheme,
           Pkcs11_Scheme,
           Platform_Scheme,
           POP_Scheme,
           Pres_Scheme,
           Prospero_Scheme,
           Proxy_Scheme,
           Pwid_Scheme,
           Psyc_Scheme,

           QB_Scheme,
           Query_Scheme,

           Redis_Scheme,
           Rediss_Scheme,
           Reload_Scheme,
           Res_Scheme,
           Resource_Scheme,
           RMI_Scheme,
           Rsync_Scheme,
           RTMFP_Scheme,
           RTMP_Scheme,
           RTSP_Scheme,
           RTSPS_Scheme,
           RTSPU_Scheme,

           Secondlife_Scheme,
           Service_Scheme,
           Session_Scheme,
           SFTP_Scheme,
           SGN_Scheme,
           SHTTP_Scheme,
           Sieve_Scheme,
           Sip_Scheme,
           Sips_Scheme,
           Skype_Scheme,
           SMB_Scheme,
           SMS_Scheme,
           SMTP_Scheme,
           SNews_Scheme,
           SNTP_Scheme,
           Soap_Beep_Scheme,
           Soap_Beeps_Scheme,
           Soldat_Scheme,
           Spotify_Scheme,
           SSH_Scheme,
           Steam_Scheme,
           Stun_Scheme,
           Stuns_Scheme,
           Submit_Scheme,
           SVN_Scheme,

           Tag_Scheme,
           Teamspeak_Scheme,
           Tel_Scheme,
           Teliaeid_Scheme,
           Telnet_Scheme,
           TFTP_Scheme,
           Things_Scheme,
           Thismessage_Scheme,
           Tip_Scheme,
           Tn3270_Scheme,
           Tool_Scheme,
           Turn_Scheme,
           Turns_Scheme,
           TV_Scheme,

           UDP_Scheme,
           Unreal_Scheme,
           URN_Scheme,
           UT2004_Scheme,

           V_Event_Scheme,
           VEMMI_Scheme,
           Ventrilo_Scheme,
           Videotex_Scheme,
           VNC_Scheme,
           View_Source_Scheme,

           Wais_Scheme,
           Webcal_Scheme,
           Wpid_Scheme,
           WS_Scheme,
           WSS_Scheme,
           WTAI_Scheme,
           Wyciwyg_Scheme,

           Xcon_Scheme,
           Xcon_Userid_Scheme,
           Xfire_Scheme,
           XMLRPC_Beep_Scheme,
           XMLRPC_Beeps_Scheme,
           XMPP_Scheme,
           XRI_Scheme,

           YMSGR_Scheme,

           Z39_50_Scheme,
           Z39_50r_Scheme,
           Z39_50s_Scheme,

           Unknown_Scheme
        );
--
-- Image -- Scheme name
--
--    Scheme - The scheme
--
-- Returns :
--
--    The corresponding name
--
   function Image (Scheme : Scheme_Type) return String;
--
-- Request_Header -- Request header fields
--
   type Request_Header is
        (  Accept_Header,
           Accept_Charset_Header,
           Accept_Encoding_Header,
           Accept_Language_Header,
           Accept_Datetime_Header,
           Allow_Header,
           Authorization_Header,
           Cache_Control_Header,
           Cookie_Header,
           Content_Encoding_Header,
           Content_Disposition_Header,
           Content_Language_Header,
           Content_Location_Header,
           Content_MD5_Header,
           Content_Type_Header,
           Expect_Header,
           Expires_Header,
           From_Header,
           Host_Header,
           If_Match_Header,
           If_None_Match_Header,
           If_Range_Header,
           Max_Forwards_Header,
           Origin_Header,
           Proxy_Authorization_Header,
           Pragma_Header,
           Referer_Header,
           TE_Header,
           Trailer_Header,
           Transfer_Encoding_Header,
           Upgrade_Header,
           Upgrade_Insecure_Requests,
           User_Agent_Header,
           Via_Header,
           Sec_WebSocket_Extensions_Header,
           Sec_WebSocket_Protocol_Header,
           Sec_WebSocket_Version_Header,
           Sec_WebSocket_Key_Header,
           X_Requested_By_Header,   -- Non-standard text header
           X_Requested_With_Header, -- Non-standard text header
           X_XSRF_Token_Header,     -- Non-standard text header
           X_CSRF_Token_Header,     -- Non-standard text header
           Warning_Header,

           Range_Header,

           Connection_Header,
           Content_Length_Header,
           Date_Header,
           If_Modified_Since_Header,
           If_Unmodified_Since_Header,
           Last_Modified_Header
        );
--
-- Image -- Header name
--
--    Header - The header type
--
-- Returns :
--
--    The corresponding name, e.g. Keep-Alive
--
   function Image (Header : Request_Header) return String;
--
-- Value -- String to header type conversion
--
--    Text - The header name (case-insensitive)
--
-- Returns :
--
--    The header type
--
   type Header_Value (None : Boolean := True) is record
      case None is
         when True =>
            null;
         when False =>
            Header : Request_Header;
      end case;
   end record;
   function Value (Text : String) return Header_Value;

   subtype Text_Header is Request_Header
      range Accept_Header..Warning_Header;
   subtype Multipart_Header is Request_Header
      range Content_Encoding_Header..Content_Type_Header;

   type WebSocket_Status is range 0..2**16 - 1;
--
-- Status codes in the range 0-999 are not used.
--
   subtype WebSocket_Unused   is WebSocket_Status range 0..999;
--
-- Status codes in the range 1000-2999 are reserved  for  definition  by
-- this  protocol,  its  future revisions, and extensions specified in a
-- permanent and readily available public specification.
--
   subtype WebSocket_Mandated is WebSocket_Status range 1000..2999;
   WebSocket_Normal_Closure    : constant WebSocket_Mandated := 1000;
   WebSocket_Endpoint_Shutdown : constant WebSocket_Mandated := 1001;
   WebSocket_Protocol_Error    : constant WebSocket_Mandated := 1002;
   WebSocket_Type_Error        : constant WebSocket_Mandated := 1003;
   WebSocket_No_Status_Code    : constant WebSocket_Mandated := 1005;
   WebSocket_Aborted           : constant WebSocket_Mandated := 1006;
   WebSocket_Data_Error        : constant WebSocket_Mandated := 1007;
   WebSocket_Generic_Error     : constant WebSocket_Mandated := 1008;
   WebSocket_Constraint_Error  : constant WebSocket_Mandated := 1009;
   WebSocket_Not_Supported     : constant WebSocket_Mandated := 1010;
   WebSocket_Internal_Error    : constant WebSocket_Mandated := 1011;
   WebSocket_TLS_Error         : constant WebSocket_Mandated := 1012;
--
-- Status  codes  in  the  range  3000-3999  are  reserved  for  use  by
-- libraries, frameworks,  and  applications.  These  status  codes  are
-- registered directly with IANA. The interpretation of these  codes  is
-- undefined by this protocol.
--
   subtype WebSocket_IANA     is WebSocket_Status range 3000..3999;
--
-- Status  codes in the range 4000-4999 are reserved for private use and
-- thus  can't be registered. Such codes can be used by prior agreements
-- between WebSocket applications. The interpretation of these codes  is
-- undefined by this protocol.
--
   subtype WebSocket_Private  is WebSocket_Status range 4000..4999;

   subtype WebSocket_Message_Size is Stream_Element_Count
      range 125..Stream_Element_Count'Last;
--
-- Content_Ranges -- Ranges of content bytes
--
   package Content_Ranges is new Ada.Containers.Ordered_Sets (Element_Type => Stream_Element_Count);
--
-- Range_Type -- Type of range
--
--    Explicit_Range - A range with both bounds specified
--    Suffix_Range   - A range with unknown upper bound
--
   type Range_Type is (Explicit_Range, Suffix_Range);
--
-- Ranges_Set -- Set of ranges with or without one suffix range
--
   type Ranges_Set (Kind : Range_Type := Explicit_Range) is record
      Set : Content_Ranges.Set; -- Specified ranges
      case Kind is
         when Explicit_Range =>
            null;
         when Suffix_Range =>
            Tail : Stream_Element_Offset;
      end case;
   end record;
------------------------------------------------------------------------
   Content_Not_Ready : exception;
--
-- Content_Source -- User-provided content
--
   type Content_Source is
      abstract new Ada.Finalization.Limited_Controlled with null record;
--
-- Get -- Get next piece of data
--
--    Source - The content source
--
-- The length of returned piece  should  not exceed  the capacity of the
-- output buffer including prefix and suffix used for chunked transfer.
--
-- Returns :
--
--    The chunk of data
--
-- Exceptions :
--
--    Content_Not_Ready - No content ready yet
--
   function Get (Source : access Content_Source)
      return String is abstract;
------------------------------------------------------------------------
-- Content_Destination -- User-provided content
--
   type Content_Destination is
      abstract new Ada.Finalization.Limited_Controlled with null record;
--
-- Commit -- Finish receipt
--
--    Destination - The content
--
-- This procedure is called to finish successful receipt of the content.
-- The default implementation does nothing.
--
   procedure Commit (Destination : in out Content_Destination);
--
-- Put -- Store next piece of data
--
--    Destination - The content
--    Data        - Chunk of data to store
--
   procedure Put
             (  Destination : in out Content_Destination;
                Data        : String
             )  is abstract;
--
-- CGI_Keys -- Key to value maps.  Memory  allocation  of  values in the
--             table is managed by the connection object. They shall not
-- be freed or changed.
--
   type String_Ptr is access all String;
   package CGI_Keys is new Tables (String_Ptr);
------------------------------------------------------------------------
-- HTTP_Client -- An object  implementing  HTTP 1.1  connection  from  a
--                client
--
--    Listener       - The connection object
--    Request_Length - The maximum length of one request line
--    Input_Size     - The input buffer size
--    Output_Size    - The output buffer size
--
-- The output  buffer  size should  be large  enough  to accommodate all
-- headers  of a response  and its status line and the body when that is
-- sent as a single string.  The response body sent from  a stream  or a
-- content source  using chunked transfer are  not bound  by t he output
-- buffer size.
--
   type HTTP_Client
        (  Listener       : access Connections_Server'Class;
           Request_Length : Positive;
           Input_Size     : Buffer_Length;
           Output_Size    : Buffer_Length
        )  is new State_Machine with private;
--
-- Status_Line_Type -- Request status line type
--
--    None - Nothing specified
--    File - File path specified
--    URI  - URI specified
--
   type Status_Line_Type is (None, File, URI);
--
-- Status_Line -- Status line
--
   type Status_Line
        (  Kind         : Status_Line_Type;
           Path_Length  : Natural;
           Host_Length  : Natural;
           Query_Length : Natural
        )  is
   record
      Query : String (1..Query_Length);
      case Kind is
         when None =>
            null;
         when File =>
            File : String (1..Path_Length);
         when URI =>
            Scheme : Scheme_Type;
            Host   : String (1..Host_Length);
            Port   : Port_Type;
            Path   : String (1..Path_Length);
      end case;
   end record;
--
-- Finalize -- Destruction
--
--    Client - The client connection object
--
-- This  procedure  shall  be called  from  the  new  implementation  if
-- overridden by the derived type.
--
   procedure Finalize (Client : in out HTTP_Client);
--
-- Get_Allowed -- The list options the server supports
--
--    Client - The client connection object
--
-- Returns :
--
--    The set of supported requests as reperted to OPTIONS
--
   function Get_Allowed (Client : HTTP_Client) return HTTP_Allowed;
--
-- Get_Name -- The HTTP server name
--
--    Client - The client connection object
--
-- This function can be overridden to provide a custom name  of the HTTP
-- server.
--
-- Returns :
--
--    The server name as told to the clients
--
   function Get_Name (Client : HTTP_Client) return String;
--
-- Initialize -- Construction
--
--    Client - The client connection object
--
-- This  procedure  shall  be called  from  the  new  implementation  if
-- overridden by the derived type.
--
   procedure Initialize (Client : in out HTTP_Client);
--
-- Received -- Overrides GNAT.Sockets.Connection_State_Machine...
--
   procedure Received
             (  Client  : in out HTTP_Client;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Set_Allowed -- Change the list options the server supports
--
--    Client  - The client connection object
--    Allowed - The set of supported requests (as reperted to OPTIONS)
--
   procedure Set_Allowed
             (  Client  : in out HTTP_Client;
                Allowed : HTTP_Allowed
             );
--
-- Trace -- Tracing
--
--    Client  - The client connection object
--    Message - To write into the trace
--
   procedure Trace
             (  Client  : in out HTTP_Client;
                Message : String
             );
------------------------------------------------------------------------
--
-- Do_<method> -- Callback on client requests, such as GET and POST
--
--    Client - The client connection object
--
   procedure Do_Connect   (Client : in out HTTP_Client);
   procedure Do_Delete    (Client : in out HTTP_Client);
   procedure Do_Get       (Client : in out HTTP_Client);
   procedure Do_Head      (Client : in out HTTP_Client);
   procedure Do_Options   (Client : in out HTTP_Client);
   procedure Do_Patch     (Client : in out HTTP_Client);
   procedure Do_Post      (Client : in out HTTP_Client);
   procedure Do_Put       (Client : in out HTTP_Client);
   procedure Do_Trace     (Client : in out HTTP_Client);
   procedure Do_WebSocket (Client : in out HTTP_Client);
------------------------------------------------------------------------
-- Request header information
--
-- Get_Closing -- The last connection request header
--
--    Client - The client connection object
--
-- When True is the result the connection  will be closed as soon as the
-- last data are sent to the client.
--
-- Returns :
--
--    True if connection header has "close"
--
   function Get_Closing (Client : HTTP_Client) return Boolean;
--
-- Get_Date -- The last date request header
--
--    Client - The client connection object
--
-- Returns :
--
--    The time stamp
--
-- Exceptions :
--
--    Time_Error - No date specified
--
   function Get_Date (Client : HTTP_Client) return Time;
--
-- Get_Header -- The last request header specified as a text
--
--    Client - The client connection object
--    Header - Request header
--
-- Returns :
--
--    The header value of the current request or empty string
--
   function Get_Header
            (  Client : HTTP_Client;
               Header : Text_Header
            )  return String;
--
-- Get_If_Modified_Since -- The last date request header
--
--    Client - The client connection object
--
-- Returns :
--
--    The time stamp
--
   function Get_If_Modified_Since (Client : HTTP_Client) return Time;
--
-- Get_If_Unmodified_Since -- The last date request header
--
--    Client - The client connection object
--
-- Returns :
--
--    The time stamp
--
   function Get_If_Unmodified_Since (Client : HTTP_Client) return Time;
--
-- Get_Last_Modified -- The last date request header
--
--    Client - The client connection object
--
-- Returns :
--
--    The time stamp
--
   function Get_Last_Modified (Client : HTTP_Client) return Time;
--
-- Get_Method -- The last request method
--
--    Client - The client connection object
--
-- Returns :
--
--    The method of the current request
--
   function Get_Method (Client : HTTP_Client) return HTTP_Method;
--
-- Get_Ranges -- The ranges specified by the last request header
--
--    Client - The client connection object
--
-- Returns :
--
--    The ranges specified (empty if none)
--
   function Get_Ranges (Client : HTTP_Client) return Ranges_Set;
--
-- Get_Status_Line -- The last request status line
--
--    Client - The client connection object
--
-- Returns :
--
--    The status line of the current request
--
   function Get_Status_Line (Client : HTTP_Client) return Status_Line;
--
-- Get_Version -- The last request method
--
--    Client - The client connection object
--
-- Returns :
--
--    The version of the current request
--
   function Get_Version (Client : HTTP_Client) return HTTP_Version;
------------------------------------------------------------------------
-- Sending response header fields:
--
--    Client - The client connection object
--    ...    - Header parameters
--
-- The following  procedures send response header field.  The procedures
-- do not block.  If the output buffer cannot accept the header then the
-- exception Data_Error is propagated.  Note that the output buffer size
-- is controlled by the discriminant Output_Size.
--
-- Exceptions :
--
--    Data_Error - No room for output
--
-- Reply_{HTML|Text} -- Send short reply, usually error response
--
--    Client  - The client connection object
--    Core    - The status code, e.g. 404
--    Reason  - The reason phrase, e.g. Not found
--    Message - The error response (plain text or HTML)
--    Get     - Send body if true (no body is sent for POST requests)
--
   procedure Reply_HTML
             (  Client  : in out HTTP_Client;
                Code    : Positive;
                Reason  : String;
                Message : String;
                Get     : Boolean := True
             );
   procedure Reply_Text
             (  Client  : in out HTTP_Client;
                Code    : Positive;
                Reason  : String;
                Message : String;
                Get     : Boolean := True
             );
--
-- Send_Accept_Ranges -- Accept ranges infurmation
--
--    Client        - The client connection object
--    Accept_Ranges - True if byte ranges are accepted, otherwise none
--
   procedure Send_Accept_Ranges
             (  Client        : in out HTTP_Client;
                Accept_Ranges : Boolean
             );
--
-- Send_Age -- Age field
--
--    Client - The client connection object
--    Age    - The age to send
--
   procedure Send_Age (Client : in out HTTP_Client; Age : Duration);
--
-- Send_Allow -- Allow field
--
--    Client  - The client connection object
--    Allowed - The list of allowed options
--
   procedure Send_Allow
             (  Client  : in out HTTP_Client;
                Allowed : HTTP_Allowed
             );
--
-- Send_Connection -- Connection field
--
--    Client     - The client connection object
--    Persistent - True if "keep-alive"
--
-- Note that Persistent  True is ignored if the client already requested
-- connection closing. In that case "close" is responded anyway.
--
   procedure Send_Connection
             (  Client     : in out HTTP_Client;
                Persistent : Boolean := True
             );
--
-- Send_Content_Range -- Content-range field
--
--    Client        - The client connection object
--    Content_Range - The range of the content
--
   procedure Send_Content_Range
             (  Client        : in out HTTP_Client;
                Content_Range : String := "none"
             );
--
-- Send_Content_Range -- Content-range field
--
--    Client - The client connection object
--    From   - The range's lower bound
--    To     - The range's upper bound
--    Length - The total content length
--
   procedure Send_Content_Range
             (  Client : in out HTTP_Client;
                From   : Stream_Element_Count;
                To     : Stream_Element_Count;
                Length : Stream_Element_Count
             );
--
-- Send_Content_Type -- Content-type field
--
--    Client  - The client connection object
--    Media   - The media type
--    Charset - The character set
--
   procedure Send_Content_Type
             (  Client  : in out HTTP_Client;
                Media   : String := "text/plain";
                Charset : String := "UTF-8"
             );
--
-- Send_Date -- Date field
--
--    Client - The client connection object
--    Date   - The time to send
--
   procedure Send_Date
             (  Client : in out HTTP_Client;
                Date   : Time := Clock
             );
--
-- Send_If_Modified_Since -- Date field
--
--    Client - The client connection object
--    Date   - The time to send
--
   procedure Send_If_Modified_Since
             (  Client : in out HTTP_Client;
                Date   : Time
             );
--
-- Send_If_Unmodified_Since -- Date field
--
--    Client - The client connection object
--    Date   - The time to send
--
   procedure Send_If_Unmodified_Since
             (  Client : in out HTTP_Client;
                Date   : Time
             );
--
-- Send_Last_Modified -- Date field
--
--    Client - The client connection object
--    Date   - The time to send
--
   procedure Send_Last_Modified
             (  Client : in out HTTP_Client;
                Date   : Time
             );
--
-- Send_Length -- Content-length field
--
--    Client - The client connection object
--    Length - The length of content
--
   procedure Send_Length
             (  Client : in out HTTP_Client;
                Length : Natural
             );
   procedure Send_Length
             (  Client : in out HTTP_Client;
                Length : Stream_Element_Count
             );
--
-- Send_Server -- Server field
--
--    Client - The client connection object
--
-- This procedure makes a dispatching call to Client's Get_Name
--
   procedure Send_Server (Client : in out HTTP_Client);
--
-- Send_Status_Line -- Status line of a response
--
--    Client  - The HTTP client connection object
--    Code    - The status code
--    Text    - The reason phrase
--    Version - The HTTP version to use
--
   procedure Send_Status_Line
             (  Client  : in out HTTP_Client;
                Code    : Positive;
                Text    : String;
                Version : String := "HTTP/1.1"
             );
--
-- Send -- To the client
--
--    Client  - The HTTP client connection object
--    Message - To send
--
   procedure Send
             (  Client  : in out HTTP_Client;
                Message : String
             );
   procedure Send
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             );
------------------------------------------------------------------------
-- Sending response body:
--
-- Accumulate_Body -- Add a piece of data to the accumuated body
--
--    Client  - The HTTP client connection object
--    Content - To add to the body
--
-- The accumulated body can be seny using Send_Body.  The body is erased
-- when when the server start to accept a new request.
--
   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : Stream_Element_Array
             );
   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : access Stream_Element_Array
             );
   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : String
             );
   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : access String
             );
--
-- Accumulated_Body_Length -- Length of accumuated body
--
--    Client - The HTTP client connection object
--
-- Dynamically composed bodies can be accumulated in the client's memory
-- before  sending it  back in  the response.  For this one  can use the
-- procedures Accumulate_Body.
--
-- Returns :
--
--    Length of the body accumulated using Accumulate_Body
--
   function Accumulated_Body_Length (Client : HTTP_Client)
      return Stream_Element_Count;
--
-- Body_Sent -- Called on chunked transfer completion
--
--    Client - The HTTP client connection object
--    Stream - Used during transfer
--    Get    - In response to GET or HEAD
--
-- The default implmentation does nothing.
--
   procedure Body_Sent
             (  Client : in out HTTP_Client;
                Stream : in out Root_Stream_Type'Class;
                Get    : Boolean
             );
--
-- Send_Body -- Start chunked transfer using a stream
--
--    Client   - The HTTP client connection object
--    Stream   - That contains data to transfer
--  [ Length ] - The response length or 0 if unknown
--    Get      - In response to GET or HEAD
--
-- When no Length is specified this procedure sends the header:
--
--    Transfer-Encoding: chunked
--
-- followed by the data read from Stream.  The operation  ends  when the
-- stream  ends.  The chunk  size is  at least  the size  of  the output
-- buffer. When Length is specified this procedure sends the header:
--
--    Content-Length: <length>
--
-- followed by the data read from Stream.  Only Length elements are read
-- from the stream.  This variant  is used when the length of the stream
-- is known.  It has the advantage than the client will know  the length
-- and thus be able to indicate the progress.
--
-- When  Get  is  false,  no  body  is sent.  Upon  transfer  completion
-- Body_Sent is called.  This is  done  independently on whether Get was
-- false or true.
--
   procedure Send_Body
             (  Client : in out HTTP_Client;
                Stream : access Root_Stream_Type'Class;
                Get    : Boolean := True
             );
   procedure Send_Body
             (  Client : in out HTTP_Client;
                Stream : access Root_Stream_Type'Class;
                Length : Stream_Element_Count;
                Get    : Boolean := True
             );
--
-- Send_Body -- Start chunked transfer using a stream
--
--    Client   - The HTTP client connection object
--    Content  - That contains data to transfer
--  [ Length ] - The response length or 0 if unknown
--    Get      - In response to GET or HEAD
--
-- This operation sends the header:
--
--    Transfer-Encoding: chunked
--
-- followed by the data read from Stream.  The operation  ends  when the
-- stream  ends.  The chunk  size is  at least  the size  of  the output
-- buffer. When Length is specified this procedure sends the header:
--
--    Content-Length: <length>
--
-- followed by the data read from Stream. Only Length elements are sent.
-- This variant is used when the length of the contents is known. It has
-- the advantage than the client will know  the length  and thus be able
-- to indicate the progress.
--
   procedure Send_Body
             (  Client  : in out HTTP_Client;
                Content : access Content_Source'Class;
                Get     : Boolean := True
             );
   procedure Send_Body
             (  Client  : in out HTTP_Client;
                Content : access Content_Source'Class;
                Length  : Stream_Element_Count;
                Get     : Boolean := True
             );
--
-- Send_Body -- Complete response sending string content
--
--    Client  - The HTTP client connection object
--    Content - That contains data to transfer
--    Get     - In response to GET or HEAD
--
-- This operation sends the header:
--
--    Content-Length: <length>
--
-- followed by the content. The content must fit into the output buffer.
-- Larger  contents  should  be first  accumulated  in the memory  using
-- Accumulate_Body. When Get is false, no message body is sent.
--
   procedure Send_Body
             (  Client  : in out HTTP_Client;
                Content : String;
                Get     : Boolean := True
             );
--
-- Send_Body -- Complete response sending accumulated content
--
--    Client  - The HTTP client connection object
--    Get     - In response to GET or HEAD
--
-- This operation sends the header:
--
--    Content-Length: <length>
--
-- followed by the content. When Get is false, no message body is sent.
--
   procedure Send_Body
             (  Client : in out HTTP_Client;
                Get    : Boolean := True
             );
------------------------------------------------------------------------
-- Receiving bodies:
--
-- Do_Body -- Prepare to start receipt a request's body
--
--    Client - The HTTP client connection object
--
-- This  procedure is  called before  the server starts receiving of the
-- body of  the pending  request.  The implementation  may create a file
-- to write here  using information from the header fields.  The default
-- implementation  does nothing  which has  the effect  that the body is
-- received but ignored. For multipart bodies Do_Body is called for each
-- part.
--
   procedure Do_Body (Client : in out HTTP_Client);
--
-- Body_Error -- Called on body receipt error
--
--    Client           - The HTTP client connection object
--    Stream / Content - Used during transfer
--    Error            - The error occurence
--
-- This procedure is called  on  an error  occurred  during  storing the
-- request's body.  The rest of  the body  is accepted but ignored.  The
-- default implementation raises Data_Error exception.
--
   procedure Body_Error
             (  Client : in out HTTP_Client;
                Stream : in out Root_Stream_Type'Class;
                Error  : Exception_Occurrence
             );
   procedure Body_Error
             (  Client  : in out HTTP_Client;
                Content : in out Content_Destination'Class;
                Error   : Exception_Occurrence
             );
   procedure Body_Error
             (  Client  : in out HTTP_Client;
                Content : in out CGI_Keys.Table'Class;
                Error   : Exception_Occurrence
             );
--
-- Body_Received -- Called on body receipt completion
--
--    Client           - The HTTP client connection object
--    Stream / Content - Used during transfer
--
-- This procedure is called  on the request's  body  receipt completion,
-- when that  was initiated using  a call  to Receive_Body.  The default
-- implmentation does nothing.
--
   procedure Body_Received
             (  Client : in out HTTP_Client;
                Stream : in out Root_Stream_Type'Class
             );
   procedure Body_Received
             (  Client  : in out HTTP_Client;
                Content : in out Content_Destination'Class
             );
   procedure Body_Received
             (  Client  : in out HTTP_Client;
                Content : in out CGI_Keys.Table'Class
             );
--
-- Get_CGI_Key -- CGI key of the body received
--
--    Client - The client connection object
--    Index  - The key's position starting with 1
--
-- The  key/value  pairs  are set  using  a call to a Receive_Body which
-- parses the application/x-www-form-urlencoded bodies.
--
-- Returns :
--
--    The key or empty string
--
-- Exceptions :
--
--    Constraint_Error - Index is not in 1..Get_CGI_Size
--
   function Get_CGI_Key (Client : HTTP_Client; Index : Positive)
      return String;
--
-- Get_CGI_Size -- The number of CGI values
--
--    Client - The client connection object
--
-- Returns :
--
--    The number of CGI key/value pairs
--
   function Get_CGI_Size (Client : HTTP_Client) return Natural;
--
-- Get_CGI_Value -- CGI value of the body received
--
--    Client      - The client connection object
--    Key / Index - The key name or position starting with 1
--
-- The  key/value  pairs are  set using  a call to a Receive_Body  which
-- parses the application/x-www-form-urlencoded bodies.
--
-- Returns :
--
--    The value corresponding to Key or empty string
--
-- Exceptions :
--
--    Constraint_Error - Index is not in 1..Get_CGI_Size
--
   function Get_CGI_Value (Client : HTTP_Client; Key : String)
      return String;
   function Get_CGI_Value (Client : HTTP_Client; Index : Positive)
      return String;
--
-- Get_Multipart_Header -- The header of the current multipart body
--
--    Client - The client connection object
--    Header - Multipart header
--
-- Returns :
--
--    The header value of the current request or empty string
--
   function Get_Multipart_Header
            (  Client : HTTP_Client;
               Header : Multipart_Header
            )  return String;
--
-- Receive_Body -- Start receipt into a CGI key/value pairs
--
--    Client      - The HTTP client connection object
--  [ Content   ] - The mapping of the expected keys (table or text)
--  [ Delimiter ] - Keys delimiter in Content
--
-- This procedure is typically  called  from  Do_Body to start receiving
-- body  of  the  application/x-www-form-urlencoded  content  type.  The
-- parameter  Content specifies  the list of  expected keys.  Not listed
-- keys are ignored.  After  successful body receipt Content has a value
-- set  if the key was present  in the body or  empty  string otherwise.
-- The object  Content shall  exist at least until  CGI_Body_Received is
-- called. When Content is a string it is a list of keys separated using
-- Delimiter character. When Content is omitted any key is accepted  for
-- which the primitive operation Validate_Key returns True, which is the
-- default. Note that this behavior is unsafe.  When the variant of this
-- procedure without the Content parameter is used,  Validate_Key should
-- be overridden.
--
   procedure Receive_Body
             (  Client  : in out HTTP_Client;
                Content : access CGI_Keys.Table'Class
             );
   procedure Receive_Body
             (  Client    : in out HTTP_Client;
                Content   : String;
                Delimiter : Character := ','
             );
   procedure Receive_Body (Client : in out HTTP_Client);
--
-- Receive_Body -- Start receipt into a stream
--
--    Client - The HTTP client connection object
--    Stream - To write
--
-- This procedure  is typcally  called  from  Do_Body to start receiving
-- the request's body into the stream.
--
   procedure Receive_Body
             (  Client : in out HTTP_Client;
                Stream : access Root_Stream_Type'Class
             );
--
-- Receive_Body -- Start receipt into a user-content object
--
--    Client  - The HTTP client connection object
--    Content - The content object to accept the body
--
-- This procedure is typically  called  from  Do_Body to start receiving
-- the request's body by Content object.
--
   procedure Receive_Body
             (  Client  : in out HTTP_Client;
                Content : access Content_Destination'Class
             );
--
-- Receive_Body_Tracing -- Enable tracing
--
--    Client - The HTTP client connection object
--    Enable - Trace receiving bodies
--
   procedure Receive_Body_Tracing
             (  Client : in out HTTP_Client;
                Enable : Boolean
             );
--
-- Receive_Header_Tracing -- Enable tracing
--
--    Client - The HTTP client connection object
--    Enable - Trace receiving bodies
--
   procedure Receive_Header_Tracing
             (  Client : in out HTTP_Client;
                Enable : Boolean
             );
--
-- Validate_Key -- Validate a CGI key
--
--    Client - The HTTP client connection object
--    Key    - The CGI key to validate
--
-- This function is called for each CGI key when Receive_Body is used to
-- receive  CGI key/value pairs without the content parameter.  The keys
-- for which  the  function  returns  false  are  ignored.  The  default
-- implementation accepts any key.
--
-- Returns :
--
--    True if the CGI key is accepted
--
   function Validate_Key
            (  Client : HTTP_Client;
               Key    : String
            )  return Boolean;
------------------------------------------------------------------------
-- WebSocket operations. WebSockets are defined in RFC 6455
--
-- WebSocket_Close -- Initiate socket close
--
--    Client  - The HTTP client connection object
--    Status  - Status code
--    Message - To the client (UTF-8 encoded)
--
-- Usually the socket is closed on client's request. If the server wants
-- to close it, this procedure is called.  Note that a client's response
-- will be awaited.
--
-- Exceptions :
--
--    Constraint_Error - Message is longer than 123 characters
--    End_Error        - The socket is closed
--
   procedure WebSocket_Close
             (  Client  : in out HTTP_Client;
                Status  : WebSocket_Status := WebSocket_Normal_Closure;
                Message : String := ""
             );
--
-- WebSocket_Closed -- Socket closed callback
--
--    Client  - The HTTP client connection object
--    Status  - The status code
--    Message - The close socket message
--
-- This  procedure  is  called  when  the  WebSocket is    closed at the
-- client's request. That is when the server receives a close frame from
-- the client. The Status code is the first two octets of the  message's
-- payload  data.  Message the UTF-8 encoded text from the payload data.
-- The default implementation does nothing.
--
   procedure WebSocket_Closed
             (  Client  : in out HTTP_Client;
                Status  : WebSocket_Status;
                Message : String
             );
--
-- WebSocket_Error -- Socket error callback
--
--    Client - The HTTP client connection object
--    Error  - The exception occurrence
--
-- This   procedure   is   called   on  WebSocket  errors.  The  default
-- implementation does nothing.
--
   procedure WebSocket_Error
             (  Client : in out HTTP_Client;
                Error  : Exception_Occurrence
             );
--
-- WebSocket_Finalize -- Socket finalization
--
--    Client - The HTTP client connection object
--
-- This  is  the  last  procedure  called  on  WebSocket.  The   default
-- implementation does nothing.
--
   procedure WebSocket_Finalize (Client : in out HTTP_Client);
--
-- WebSocket_Initialize -- Socket initialization callback
--
--    Client - The HTTP client connection object
--
-- This  is the first operation called on  the WebSocket called when the
-- socket  becomes  fully  operational.  This  is  an appropriate  place
-- to start a task if duplex I/O is intended. The default implementation
-- does nothing.
--
   procedure WebSocket_Initialize (Client : in out HTTP_Client);
--
-- WebSocket_Open -- Socket connection request callback
--
--    Client - The HTTP client connection object
--
-- The implementation verifies the header fields and the URI in order to
-- decide to accept or reject connection.
--
-- Returns :
--
--    WebSocket_Accept:
--
--    (o)  If   Accepted   discriminant  is  True,  the  server  accepts
--         connection and:
--
--         - Size  determines  the maximal length in octets  of an input
--           WebSocket message (maybe split in several frames);
--
--         - Duplex  is set to  True  when the server will  send to  the
--           WebSocket from an independent task.  In particular  it will
--           enable  receiving  and sending  messages in parallel  (full
--           duplex).  When  Duplex is False  the communication  is half
--           duplex.  The server responds to each incoming message  by a
--           series of messages. While sending these messages the server
--           stops receiving new messages;
--
--         - Chunked  controls  chunked  message  receipt.  When   False
--           messages larger than Size cause error and the connection is
--           dropped.   When   True   incomplete   message  parts  cause
--           WebSocket_Received_Part  called.  The  last  part  of   the
--           message   is   always   delivered   using   a    call    to
--           WebSocket_Received.
--
--         - Protocols  is the list of protocols reported  to the client
--           as supported.  If  this string  is  empty the  contents  of
--           Sec-WebSocket-Protocol is taken.
--
--    (o)  If Accepted is false, the connection is rejected and:
--
--         - Code is the code to report, e.g. 400;
--
--         - Reason is its text, e.g. Bad Request.
--
   type WebSocket_Accept
        (  Accepted : Boolean;
           Length   : Natural
        )  is
   record
      case Accepted is
         when True =>
            Size      : WebSocket_Message_Size;
            Duplex    : Boolean;
            Chunked   : Boolean;
            Protocols : String (1..Length);
         when False =>
            Code      : Positive;
            Reason    : String (1..Length);
      end case;
   end record;
   function WebSocket_Open
            (  Client : access HTTP_Client
            )  return WebSocket_Accept;
--
-- WebSocket_Received -- Socket message callback
--
--    Client  - The HTTP client connection object
--    Message - From the client
--
-- These procedures are called on receipt of a  WebSocket  message.  The
-- variant  with  Stream_Element_Array parameter is called when a binary
-- message has been received. The variant with String is called  when  a
-- text  message  has. The text messages are UTF-8 encoded (according to
-- RFC  6455). The validity of the encoding is not checked, though. Note
-- that  the  maximal  message  length  is limited by the value set upon
-- socket connection. The default implementations do nothing.
--
   procedure WebSocket_Received
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             );
   procedure WebSocket_Received
             (  Client  : in out HTTP_Client;
                Message : String
             );
--
-- WebSocket_Received_Part -- Socket message callback
--
--    Client  - The HTTP client connection object
--    Message - From the client
--
-- These  procedures  are  called  on  receipt  of  a  part of WebSocket
-- message. It is called only when  Chunked  field  of  WebSocket_Accept
-- returned    by    WebSocket_Open    is   True.   The   variant   with
-- Stream_Element_Array parameter is called when a  binary  message  has
-- been  received. The variant with String is called when a text message
-- has. The text messages are UTF-8 encoded (according to RFC 6455). The
-- validity of the encoding  is  not  checked,  though.  Note  that  the
-- maximal message length is  limited  by  the  value  set  upon  socket
-- connection. The default implementations do nothing.
--
   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             );
   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_Client;
                Message : String
             );
--
-- WebSocket_Send -- Send a message over the socket
--
--    Client  - The HTTP client connection object
--    Message - To the client
--
-- These procedures are used to send a binary or a text message over the
-- WebSocket.
--
-- Exceptions :
--
--    End_Error - The socket is closed
--
   procedure WebSocket_Send
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             );
   procedure WebSocket_Send
             (  Client  : in out HTTP_Client;
                Message : String
             );
------------------------------------------------------------------------
--
-- From_Escaped -- Convert RFC-escaped name to plain string
--
--    Name           - To convert
--    Translate_Plus - When True + it replaces with SP
--
-- The RFC 2396 uses %HH escape sequences in the URI.
--
-- Returns :
--
--    Equivalent plain string
--
-- Exceptions :
--
--    Data_Error - Syntax error
--
   function From_Escaped
            (  Name           : String;
               Translate_Plus : Boolean := False
            )  return String;
--
-- To_HTML -- Convert to HTML format
--
--    Text - UTF-8 encoded string to encode
--
-- Returns :
--
--    String representation in HTML format
--
-- Exceptions :
--
--    Data_Error - Invalid UTF-8 sequence
--
   function To_HTML (Text : String) return String;
--
-- To_Escaped -- Convert plain string to RFC-escaped
--
--    Name - To convert
--
-- Returns :
--
--    Equivalent escaped string
--
   function To_Escaped (Name : String) return String;
--
-- To_HTTP -- Convert time to HTTP format
--
--    Date - The time to convert
--
-- Returns :
--
--    String representation of the parameter Date in the format:
--    Sun, 17 Feb 2013 21:02:43 +0100
--
   function To_HTTP (Date : Time) return String
      renames Strings_Edit.Time_Conversions.To_String;
--
-- To_Time -- Time conversion
--
--    Date - To convert
--
-- Supported formats:
--
--    Fri, 31 Dec 1999 23:59:59 GMT
--    Friday, 31-Dec-99 23:59:59 GMT
--    Fri Dec 31 23:59:59 1999
--
-- Exceptions :
--
--    Time_Error - On error
--
   function To_Time (Date : String) return Time
      renames Strings_Edit.Time_Conversions.To_Time;
------------------------------------------------------------------------
   CRLF  : constant String := (Character'Val (13), Character'Val (10));
   Lower : constant Character_Mapping :=
                    To_Mapping
                    (  "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                       "abcdefghijklmnopqrstuvwxyz"
                    );

   type Connection_Flags is mod 2**3;
   Connection_Close      : Connection_Flags := 1; -- "close"
   Connection_Persistent : Connection_Flags := 2; -- "keep-alive"
   Connection_Upgrade    : Connection_Flags := 4; -- "upgrade"
--
-- To_Flags -- Conversion of connection header value into flags
--
--    Value - The header value
--
-- Returns :
--
--    Connection flags
--
   function To_Flags (Value : String) return Connection_Flags;
private
   use GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
   use GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

   type Stream_Ptr is access all Root_Stream_Type'Class;

   type Action is access procedure (Client : in out HTTP_Client'Class);
   procedure Skip (Source : String; Pointer : in out Integer);
--
-- Request_Line_Type -- Types of request lines/processing states
--
   type Request_Line_Type is
        (  Request_Line,          -- Request status line
           Header_Line,           -- Request header field line
           Body_Data,             -- Data of a body
           Chunk_Line,            -- Header of a chunk
           Multipart_Preamble,    -- Multipart preamble
           Multipart_Header_Line, -- Header of multipart body
           Multipart_Body_Data,   -- Data of a multipart body
           Multipart_Body_Tail,   -- The multipart body data tail
           Multipart_Epilogue,    -- Multipart epilogue
           WebSocket_Header,      -- Incoming frame beginning
           WebSocket_Length,      -- Length octet
           WebSocket_Length_Ex,   -- Extended length octets
           WebSocket_Mask,        -- Mask octets
           WebSocket_Payload_Data -- Payload data octets
        );
   type Stream_Element_Array_Ptr is access all Stream_Element_Array;
   type Content_Source_Ptr is access all Content_Source'Class;
   type Content_Destination_Ptr is access all Content_Destination'Class;
   type Data_Pool is new Stack_Storage.Pool with null record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Pool
             );
   for Data_Pool'Write use Write;

   subtype Specific_Header is Request_Header
      range Date_Header..Last_Modified_Header;

   type Status_Line_Ptr is access constant Status_Line;
   type Text_Ptr is access constant String;
   type Request_Header_Array is array (Text_Header) of Text_Ptr;
   type Multipart_Header_Array is array (Multipart_Header) of Text_Ptr;
   type Specific_Header_Array is array (Specific_Header) of Boolean;

   type Content_Item;
   type Content_Item_Ptr is access all Content_Item;
   type Content_Item_Type is
        (  Literal_Value,
           String_Pointer,
           Stream_Elements_Pointer
        );
   type Content_Item
        (  Kind   : Content_Item_Type;
           Length : Stream_Element_Count
        )  is
   record
      Next  : Content_Item_Ptr;
      First : Stream_Element_Offset := 1;
      case Kind is
         when Literal_Value =>
            Data : Stream_Element_Array (1..Length);
         when String_Pointer =>
            Text_Ptr : String_Ptr;
         when Stream_Elements_Pointer =>
            Data_Ptr : Stream_Element_Array_Ptr;
      end case;
   end record;
   type Content_Stream is new Root_Stream_Type with record
      Length : Stream_Element_Count := 0;
      First  : Content_Item_Ptr;
      Last   : Content_Item_Ptr;
   end record;
   function Is_Empty (Stream : Content_Stream) return Boolean;
   procedure Read
             (  Stream : in out Content_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Content_Stream;
                Item   : Stream_Element_Array
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Content_Stream
             );
   for Content_Stream'Write use Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Content_Destination
             );
   for Content_Destination'Write use Write;

   type CGI_Keys_Ptr is access all CGI_Keys.Table'Class;
   type CGI_State is (CGI_Key, CGI_Value);
   type CGI_Content (Client : access HTTP_Client'Class) is
      new Content_Destination with
   record
      State  : CGI_State;
      Offset : Natural := 0;
      Keys   : CGI_Keys_Ptr;
      Map    : aliased CGI_Keys.Table; -- Keys from a string
   end record;
   procedure Commit (Destination : in out CGI_Content);
   procedure Put
             (  Destination : in out CGI_Content;
                Data        : String
             );

   subtype WebSocket_Frame_Type is Stream_Element range 0..15;
   WebSocket_Continunation_Type : constant WebSocket_Frame_Type := 0;
   WebSocket_Text_Type          : constant WebSocket_Frame_Type := 1;
   WebSocket_Binary_Type        : constant WebSocket_Frame_Type := 2;
   WebSocket_Close_Type         : constant WebSocket_Frame_Type := 8;
   WebSocket_Ping_Type          : constant WebSocket_Frame_Type := 9;
   WebSocket_Pong_Type          : constant WebSocket_Frame_Type := 10;

   type WebSocket_Mask_Index is mod 4;
   type WebSocket_Mask_Array is
      array (WebSocket_Mask_Index) of Stream_Element;
   type WebSocket_Length_Count is range 1..8;

   subtype WebSocket_Control_Frame_Data is
      Stream_Element_Array (Stream_Element_Offset range 0..125);

   type WebSocket_Message (Size : WebSocket_Message_Size) is record
      Type_Of : WebSocket_Frame_Type;  -- The frame type
      Length  : Stream_Element_Count;  -- The message length
      Pointer : Stream_Element_Offset; -- To element to write
      Data    : Stream_Element_Array (1..Size);
   end record;
   type WebSocket_Message_Ptr is access all WebSocket_Message;

   type Duplex_Status is
        (  Disabled,
           Closing,
           Idle,
           Task_Sending,
           Server_Sending
        );
--
-- Send_Mutex -- The  object  used  for  interlocking  send  buffer  and
--               signaling its state when going multi-tasking.  The only
-- case when it happens is in a WebSocket full-duplex exchange.
--
   protected type Send_Mutex (Client : access HTTP_Client'Class) is
   --
   -- Failed -- Send failure notification
   --
      procedure Failed (Error : Exception_Occurrence);
   --
   -- Get_Status -- Get current status
   --
   -- Returns :
   --
   --    The current status
   --
      function Get_Status return Duplex_Status;
   --
   -- Grab -- Lock sending for the server
   --
   --    Seized - True if the lock was taken
   --
      procedure Grab (Seized : out Boolean);
   --
   -- Release -- Sending previously locked by Seize or Grab
   --
      procedure Release;
   --
   -- Set -- Change object state
   --
   --    New_State - The new state
   --
      procedure Set (New_State : Duplex_Status);
   --
   -- Seize -- Lock sending from an external task
   --
      entry Seize;

   private
      State : Duplex_Status := Disabled;     -- Socket is closed
   end Send_Mutex;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Send_Mutex
             );
   for Send_Mutex'Write use Write;

   type WebSocket_State is (Open_Socket, Closing_Socket, Closed_Socket);
   type WebSocket_Data is record
      State        : WebSocket_State := Closed_Socket;
      Final        : Boolean := False;        -- Current frame is last
      Pending      : Boolean := False;        -- A message is pending
      Duplex       : Boolean := False;        -- Use mutex
      Chunked      : Boolean := False;        -- Chunked messages
      Frame_Type   : WebSocket_Frame_Type;    -- Current frame type
      Frame_Length : Stream_Element_Count;    -- Current frame length
      Length_Count : WebSocket_Length_Count;  -- Extended length
      Max_Length   : WebSocket_Message_Size;  -- Maximal message length
      Frame        : WebSocket_Message_Ptr;   -- Current frame buffer
      Data         : WebSocket_Message_Ptr;   -- Incoming messages frame
      Context      : Task_ID := Null_Task_Id; -- Calling context
      Mask_Index   : WebSocket_Mask_Index;    -- Mask index
      Mask         : WebSocket_Mask_Array;    -- Mask
      Control      : aliased WebSocket_Message (125);
      pragma Atomic (State);
      pragma Atomic (Duplex);
   end record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : WebSocket_Data
             );
   for WebSocket_Data'Write use Write;

   type HTTP_Client
        (  Listener       : access Connections_Server'Class;
           Request_Length : Positive;
           Input_Size     : Buffer_Length;
           Output_Size    : Buffer_Length
        )  is new State_Machine (Input_Size, Output_Size) with
   record
      Expecting    : Request_Line_Type    := Request_Line;
      Data_Length  : Stream_Element_Count := 0;
      Chunk_Type   : Request_Line_Type;
      Method       : HTTP_Method;
      Stream       : Stream_Ptr;
      Source       : Content_Source_Ptr;
      Destination  : Content_Destination_Ptr;
      Boundary     : String_Ptr;       -- Multipart boundary
      Position     : Integer;          -- Boundary character to match
      Chain        : Action := null;
      Connection   : Connection_Flags := 0;
      Chunked      : Boolean := False; -- Using chunked transfer
      Trace_Body   : Boolean := False;
      Trace_Header : Boolean := False;
      Validate_CGI : Boolean := False;
         -- Time header fields
      Date                : Time := Clock;
      Last_Modified       : Time := Clock;
      If_Modified_Since   : Time := Clock;
      If_Unmodified_Since : Time := Clock;
         -- Options
      Allowed   : HTTP_Allowed := (  HTTP_GET     => True,
                                     HTTP_HEAD    => True,
                                     HTTP_OPTIONS => True,
                                     others       => False
                                  );
      Version   : HTTP_Version;
      Port      : Port_Type;
      Stub      : Content_Item_Ptr; -- First allocated content item
         -- WebSocket data
      WebSocket : WebSocket_Data;
         --
      Ranges    : Content_Ranges.Set;    -- Request ranges
      Suffix    : Stream_Element_Offset := Stream_Element_Offset'First;

         -- Parsed line, CR LF terminated
      Line         : Dynamic_String_Data_Item;
      LF           : Expected_Item (1);

      Status       : Status_Line_Ptr;
      Part_Mark    : Text_Ptr;               -- Mark of part headers
      Headers      : Request_Header_Array;   -- Request headers
      Multipart    : Multipart_Header_Array; -- Part headers
      Specific     : Specific_Header_Array;  -- Specific headers
      Body_Content : aliased Content_Stream;
      CGI          : aliased CGI_Content (HTTP_Client'Unchecked_Access);
      Pool         : Data_Pool (0, 4);
      Mutex        : aliased Send_Mutex (HTTP_Client'Unchecked_Access);
      pragma Atomic (Expecting);
   end record;

   function Check_WebSocket
            (  Client : access HTTP_Client
            )  return Boolean;

   procedure Process_Body_Tail    (Client : in out HTTP_Client'Class);
   procedure Process_Chunk_Line   (Client : in out HTTP_Client'Class);
   procedure Process_Epilogue     (Client : in out HTTP_Client'Class);
   procedure Process_Header_Line  (Client : in out HTTP_Client'Class);
   procedure Process_Part_Header  (Client : in out HTTP_Client'Class);
   procedure Process_Preamble     (Client : in out HTTP_Client'Class);
   procedure Process_Request      (Client : in out HTTP_Client'Class);
   procedure Process_Request_Line (Client : in out HTTP_Client'Class);
--
-- Queue_Content -- Queue data to send later
--
--    Client - The HTTP client connection object
--    Data   - To send
--
-- The procedure adds message the content's queue
--
   procedure Queue_Content
             (  Client : in out HTTP_Client;
                Data   : Stream_Element_Array
             );
   procedure Queue_Content
             (  Client : in out HTTP_Client;
                Data   : String
             );
   procedure Set_Failed
             (  Client : in out HTTP_Client;
                Error  : Exception_Occurrence
             );
   procedure Write
             (  Client  : in out HTTP_Client;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             );

   type Completion is access
      procedure (Client : in out HTTP_Client'Class);
   procedure Receive_Header_Line
             (  Client  : in out HTTP_Client'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Handler : Completion
             );
   procedure Receive_Multipart_Line
             (  Client  : in out HTTP_Client'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Handler : Completion
             );

   procedure Header_Received
             (  Client : in out HTTP_Client;
                Header : Request_Header;
                Value  : String
             );
   procedure Multipart_Header_Received
             (  Client : in out HTTP_Client;
                Header : Request_Header;
                Value  : String
             );

   procedure Cleanup_Body_Part (Client : in out HTTP_Client'Class);
   procedure Continue (Client : in out HTTP_Client; Chain : Action);
   procedure Content_Chunk (Client : in out HTTP_Client'Class);
   procedure Message_Chunk (Client : in out HTTP_Client'Class);
   procedure Sent (Client : in out HTTP_Client);
   procedure Stream_Chunk  (Client : in out HTTP_Client'Class);
--
-- Send_Content -- Send immediately or queue rest
--
--    Client - The HTTP client connection object
--    Data   - To send
--
-- The procedure adds message the content's queue
--
   procedure Send_Content
             (  Client : in out HTTP_Client;
                Data   : Stream_Element_Array
             );
   procedure Send_Content
             (  Client : in out HTTP_Client;
                Data   : String
             );
--
-- Status_Line_Received -- Stauts line receipt
--
--    Client   - The HTTP client connection object
--    Method   - The method being requested
--  [ Path     - The file path
--    Host     - The host specified in the URI
--    Port     - The port specified in the URI
--  [ Query ]] - The query part specified in the URI
--    Version  - The HTTP version requested
--
-- This procedure is called when the status list is successfully parsed.
-- The default implementation stores fields of the status line.
--
   procedure Status_Line_Received
             (  Client  : in out HTTP_Client;
                Method  : HTTP_Method;
                Version : HTTP_Version
             );
   procedure Status_Line_Received
             (  Client  : in out HTTP_Client;
                Method  : HTTP_Method;
                Path    : String;
                Query   : String;
                Version : HTTP_Version
             );
   procedure Status_Line_Received
             (  Client  : in out HTTP_Client;
                Scheme  : Scheme_Type;
                Method  : HTTP_Method;
                Host    : String;
                Port    : Port_Type;
                Path    : String;
                Query   : String;
                Version : HTTP_Version
             );
--
-- WebSocket_Blocking_Send -- Send a piece of message from an alien task
--
--    Client  - The HTTP client connection object
--    Data    - To send
--    First   - True if Data is the first chunk of the message
--    Last    - True if Data is the last chunk of the message
--
-- Exceptions :
--
--    End_Error - WebSocket is closed
--
   procedure WebSocket_Blocking_Send
             (  Client  : in out HTTP_Client'Class;
                Data    : Stream_Element_Array;
                First   : Boolean;
                Last    : Boolean
             );
--
-- WebSocket_Cleanup -- Socket finalization
--
--    Client - The HTTP client connection object
--
   procedure WebSocket_Cleanup (Client : in out HTTP_Client'Class);

   type Multipart_Body (Boundary : access String) is abstract
      new Content_Destination with
   record
      Header : Boolean := True;
   end record;

   package HTTP_Tables_Raw is new Tables (HTTP_Method);
   package HTTP_Tables is new HTTP_Tables_Raw.Names;
   use HTTP_Tables;

   package Request_Header_Tables_Raw is new Tables (Request_Header);
   package Request_Header_Tables is
      new Request_Header_Tables_Raw.Names;
   use Request_Header_Tables;

   package Connection_Tables_Raw is new Tables (Connection_Flags);
   package Connection_Tables is new Connection_Tables_Raw.Names;
   use Connection_Tables;

   package Scheme_Tables_Raw is new Tables (Scheme_Type);
   package Scheme_Tables is new Scheme_Tables_Raw.Names;
   use Scheme_Tables;

   Commands        : HTTP_Tables.Dictionary;
   Request_Headers : Request_Header_Tables.Dictionary;
   Connections     : Connection_Tables.Dictionary;
   Schemes         : Scheme_Tables.Dictionary;

end GNAT.Sockets.Connection_State_Machine.HTTP_Server;

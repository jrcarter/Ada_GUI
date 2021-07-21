--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine       Luebeck            --
--  Interface                                      Winter, 2012       --
--                                                                    --
--                                Last revision :  18:41 01 Aug 2019  --
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
--
--  This package provides an implementation of server's  side connection
--  object that implements a state  machine  to receive packets from the
--  client side.  The structure of a packet is described by the contents
--  of connection object itself.  Fields of the  object  derived  from a
--  special abstract  type (Data_Item) fed with the input received  from
--  the client  in the order  they are declared in the object.  Once all
--  fields are received  a primitive operation is called to process  the
--  packet. After that the cycle repeats.
--     Enumeration of the fields (introspection) is  based on Ada stream
--  attributes. See ARM 13.13.2(9) for the legality of the approach.
--
with Ada.Finalization;         use Ada.Finalization;
with Ada.Streams;              use Ada.Streams;
with GNAT.Sockets.Server;      use GNAT.Sockets.Server;
with System.Storage_Elements;  use System.Storage_Elements;

with Generic_Unbounded_Array;
with Interfaces;
with System.Storage_Pools;

package GNAT.Sockets.Connection_State_Machine is
--
-- State_Machine -- Connection  client driven  by the contents.  This is
--                  the  base type  to derive  from.  The  derived  type
-- should contain a set of fields with the types derived from Data_Item.
-- These objects will be read automatically in their order.
--
   type State_Machine is abstract new Connection with private;
--
-- Connected -- Overrides Connections_Server...
--
-- If the derived type overrides this procedure, it should call this one
-- from new implemetation.
--
   procedure Connected (Client : in out State_Machine);
--
-- Finalize -- Destruction
--
--    Client - The connection client
--
-- This procedure is to be called from implementation when overridden.
--
   procedure Finalize (Client : in out State_Machine);
--
-- Received -- Overrides GNAT.Sockets.Server...
--
   procedure Received
             (  Client  : in out State_Machine;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Enumerate -- Fake stream I/O procedure
--
-- This procedure  is used internally in order to enumerate the contents
-- of the record type.
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : State_Machine
             );
   for State_Machine'Write use Enumerate;
------------------------------------------------------------------------
--
-- Data_Item -- Base type of a data item to read
--
   type Data_Item is abstract
      new Ada.Finalization.Limited_Controlled with null record;
   type Data_Item_Ptr is access all Data_Item'Class;
   type Data_Item_Offset is new Integer;
   subtype Data_Item_Address is
      Data_Item_Offset range 1..Data_Item_Offset'Last;
   type Data_Item_Ptr_Array is
      array (Data_Item_Address range <>) of Data_Item_Ptr;
--
-- Feed -- Incoming data
--
--    Item    - The data item
--    Data    - The array of stream elements containing incoming data
--    Pointer - The first element in the array
--    Client  - The connection client
--    State   - Of the data item processing
--
-- This procedure  is called  when data  become  available  to  get item
-- contents.  The stream  elements  are  Data (Pointer..Data'Last).  The
-- procedure consumes data and advances Pointer beyond consumed elements
-- The parameter  State indicates processing state.  It  is initially 0.
-- When Item contents is read in full State is set to 0.  When State  is
-- not 0 then Pointer must be set to Data'Last + 1, indicating that more
-- data required.  Feed will be  called  again on the item when new data
-- come with the value of State returned from the last call.
--
   procedure Feed
             (  Item    : in out Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is abstract;
--
-- Freed -- Deallocation notification
--
--    Item - The data item
--
-- This procedure is  called  when  the shared  data item  is  about  to
-- deallocate  items allocated  there.  See External_String_Buffer.  The
-- default implementation does nothing.
--
   procedure Freed (Item : in out Data_Item);
--
-- Get_Children -- Get list of immediate children
--
--    Item - The data item
--
-- Returns :
--
--    The list of immediate children (nested) data items
--
-- Exceptions :
--
--    Use_Error - The data item is not initialized
--
   function Get_Children (Item : Data_Item) return Data_Item_Ptr_Array;
--
-- Get_Size -- Size of the data item in data items
--
--    Item - The data item
--
-- Returns :
--
--    The default implementation returns 1
--
   function Get_Size (Item : Data_Item) return Natural;
--
-- End_Of_Subsequence -- Completion of a subsequence
--
--    Item    - The data item
--    Data    - The array of stream elements containing incoming data
--    Pointer - The first element in the array (can be Data'Last + 1)
--    Client  - The connection client
--    State   - Of the data item processing
--
-- This procedure is called  when a subsequence  of data items  has been
-- processed.  Item is  the data item which was active prior to sequence
-- start.  It is called only if the data  item was  not completed,  i.e.
-- State is not 0. When the implementation changes State to 0 processing
-- of the data  item completes  and the next item is fetched.  Note that
-- differently to Feed Pointer may point  beyond Data when all available
-- input has been processed. The default implementation does nothing.
--
   procedure End_Of_Subsequence
             (  Item    : in out Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Enumerate -- Fake stream I/O procedure
--
-- This procedure  is used internally in order to enumerate the contents
-- of the record type, a descendant of Connection.  The elements  of the
-- record  type derived  from Data_Item are  ones which will be fed with
-- data received from the socket.
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Item
             );
   for Data_Item'Write use Enumerate;
   type Shared_Data_Item;
   type Shared_Data_Item_Ptr is access all Shared_Data_Item'Class;
--
-- External_Initialize
--
--    Item   - The data item
--    Shared - The shared items stack to use
--
-- This procedure  is  used  to initialize  an object  when  it  is used
-- outside an instance of State_Machine.
--
   procedure External_Initialize
             (  Item   : Data_Item'Class;
                Shared : Shared_Data_Item_Ptr := null
             );
--
-- Shared_Data_Item -- Base type for shared items
--
   type Shared_Data_Item is abstract new Data_Item with record
      Initialized : Boolean := False;
      Previous    : Shared_Data_Item_Ptr; -- Stacked items
   end record;
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Shared_Data_Item
             );
   for Shared_Data_Item'Write use Enumerate;
--
-- Feed -- Implementation, makes the shared item active
--
   procedure Feed
             (  Item    : in out Shared_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
------------------------------------------------------------------------
-- Data_Block -- Base type  of  a sequence  of data items.  Derived data
--               types contain fields derived from Data_Item.
--
   type Data_Block is abstract new Data_Item with private;
--
-- Feed -- Implementation
--
   procedure Feed
             (  Item    : in out Data_Block;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Block
             );
   for Data_Block'Write use Enumerate;
--
-- Finalize -- Destruction
--
--    Item - Being finalized
--
-- To be called the from derived type's Finalize if overridden
--
   procedure Finalize (Item : in out Data_Block);
--
-- Get_Children -- Overriding
--
   function Get_Children (Item : Data_Block) return Data_Item_Ptr_Array;
--
-- Get_Length -- The number of items contained by the block item
--
--    Item - The block item
--
-- Exceptions :
--
--    Use_Error - The block was not initialized yet
--
   function Get_Length (Item : Data_Block) return Positive;
--
-- Get_Size -- Overriding
--
   function Get_Size (Item : Data_Block) return Natural;
------------------------------------------------------------------------
-- Data_Null -- No data item, used where a data item is required
--
   type Data_Null is new Data_Item with null record;
--
-- Feed -- Implementation
--
   procedure Feed
             (  Item    : in out Data_Null;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
------------------------------------------------------------------------
-- Data_Selector -- Base  type  of  a  list  of  data   items   selected
--                  alternatively.  A  derived  type  has fields derived
-- from Data_Item. One  of the  fields  is used at a time.  So  the type
-- acts as a variant record.  The  field  to select  is set  by  calling
-- Set_Alternative.  Usually  it is done  from  Feed  of some descendant
-- derived from Data_Item,  placed after the field controlling selection
-- of the alternative. When an alternative should enclose several fields
-- a Data_Block descendant is used.
--
   type Data_Selector is new Data_Item with private;
--
-- Feed -- Implementation
--
   procedure Feed
             (  Item    : in out Data_Selector;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Finalize -- Destruction
--
--    Item - Being finalized
--
-- To be called the from derived type's Finalize if overridden
--
   procedure Finalize (Item : in out Data_Selector);
--
-- Get_Alternative -- The currently selected alternative
--
--    Item - Selector item
--
-- Returns :
--
--    The alternative number 1..
--
   function Get_Alternative (Item : Data_Selector) return Positive;
--
-- Get_Alternatives_Number -- The number of alternatives
--
--    Item - Selector item
--
-- Returns :
--
--    The total number of alternatives
--
-- Exceptions :
--
--    Use_Error - The selector was not initialized yet
--
   function Get_Alternatives_Number (Item : Data_Selector)
      return Positive;
--
-- Get_Children -- Overriding
--
   function Get_Children
            (  Item : Data_Selector
            )  return Data_Item_Ptr_Array;
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Selector
             );
   for Data_Selector'Write use Enumerate;
--
-- Set_Alternative -- Select an alternative
--
--    Item        - Selector item
--    Alternative - To select (number 1..Get_Alternatives_Number (Item))
--
-- Exceptions :
--
--    Constraint_Error - Invalid alternative number
--    Use_Error        - The selector was not initialized yet
--
   procedure Set_Alternative
             (  Item        : in out Data_Selector;
                Alternative : Positive
             );
--
-- Get_Size -- Overriding
--
   function Get_Size (Item : Data_Selector) return Natural;
------------------------------------------------------------------------
--
-- External_String_Buffer -- The arena buffer to  keep bodies of strings
--                           and  dynamically allocated  elements.  This
-- buffer can be shared by multiple instances of Data_Item. For example:
--
--    type Alternatives_Record is new Choice_Data_Item with record
--       Text_1 : Implicit_External_String_Data_Item;
--       Text_2 : Implicit_External_String_Data_Item;
--       Text_3 : Implicit_External_String_Data_Item;
--    end record;
--    type Packet is new State_Machine ... with record
--       Buffer : External_String_Buffer (1024); -- Shared buffer
--       Choice : Alternatives_Record;
--    end record;
--
-- When several buffers  appear the  nearest  one before  the  data item
-- object is used.
--
   type External_String_Buffer;
   type External_String_Buffer_Ptr is
      access all External_String_Buffer'Class;
--
-- Arena_Pool -- The pool component  of the  External_String_Buffer that
--               allocates memory as substrings in the buffer.
--
--    Parent - The buffer to take memory from
--
   type Arena_Pool
        (  Parent : access External_String_Buffer'Class
        )  is new System.Storage_Pools.Root_Storage_Pool with
              null record;
--
-- Allocate -- Allocate memory
--
-- The memory is allocated in the string buffer as a string.
--
   procedure Allocate
             (  Pool      : in out Arena_Pool;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
--
-- Deallocate -- Free memory
--
-- This a null operation. The memory is freed by Erase
--
   procedure Deallocate
             (  Pool      : in out Arena_Pool;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   function Storage_Size
            (  Pool : Arena_Pool
            )  return Storage_Count;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Pool   : Arena_Pool
             );
   for Arena_Pool'Write use Enumerate;
--
-- Allocator_Data -- The  allocator's  list.   The  users  of  the  pool
--                   register themselves  in order to  get notifications
-- upon erasing the buffer.  They should finalize  all objects then have
-- allocated in the pool.
--
   type Allocator_Data;
   type Allocator_Data_Ptr is access all Allocator_Data;
   type Allocator_Data (Allocator : access Data_Item'Class) is record
      Previous : Allocator_Data_Ptr; -- List of allocators
   end record;
   type External_String_Buffer (Size : Natural) is
      new Shared_Data_Item with
   record
      Pool       : Arena_Pool (External_String_Buffer'Access);
      Length     : Natural := 0;
      Count      : Natural := 0;       -- Allocatied items
      Allocators : Allocator_Data_Ptr; -- List of objects using the pool
      Buffer     : String (1..Size);
   end record;
--
-- Erase -- Deallocate all elements and all strings
--
--    Buffer - The buffer
--
-- The implementation walks  the list of allocators  to notify them that
-- the allocated  items  will be freed.  The callee  must  finalize  its
-- elements if necessary.
--
   procedure Erase (Buffer : in out External_String_Buffer);
--
-- Feed -- Implementation of the feed operation
--
-- The implementation erases the buffer.
--
   procedure Feed
             (  Item    : in out External_String_Buffer;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Fnalize -- Destruction
--
--    Buffer - The buffer
--
-- The derived type must call this if it overrides it.
--
   procedure Finalize (Buffer : in out External_String_Buffer);
------------------------------------------------------------------------
   generic
      with procedure Put (Text : String) is <>;
      with procedure Put_Line (Line : String) is <>;
   package Generic_Dump is
   --
   -- Put -- Allocator list
   --
   --    Buffer - The external string buffer
   --    Prefix - Of the output lines
   --
      procedure Put
                (  Buffer : in out External_String_Buffer;
                   Prefix : String
                );
   --
   -- Put -- List item structure recursively
   --
   --    Item   - The data item
   --    Prefix - Of the output lines
   --
      procedure Put
                (  Item   : Data_Item'Class;
                   Prefix : String := ""
                );
   --
   -- Put_Call_Stack -- List the call stack of the state machine
   --
   --    Client - The state machine
   --    Prefix - Of the output lines
   --
      procedure Put_Call_Stack
                (  Client : State_Machine'Class;
                   Prefix : String := ""
                );
   --
   -- Put_Stream -- Stream and dump the result list
   --
   --    Item   - The data item
   --    Prefix - Of the output lines
   --
      procedure Put_Stream
                (  Item   : Data_Item'Class;
                   Prefix : String := ""
                );
   end Generic_Dump;
private
   use Interfaces;

   Out_Of_Bounds : constant String := "Pointer is out of bounds";
   No_Room       : constant String := "No room for output";

   package Data_Item_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Data_Item_Address,
             Object_Type       => Data_Item_Ptr,
             Object_Array_Type => Data_Item_Ptr_Array,
             Null_Element      => null
          );
   use Data_Item_Arrays;
   type Data_Item_Ptr_Array_Ptr is access Data_Item_Ptr_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Data_Item_Ptr_Array,
             Data_Item_Ptr_Array_Ptr
          );

   type Sequence;
   type Sequence_Ptr is access all Sequence;
   type Sequence (Length : Data_Item_Address) is record
      Caller  : Sequence_Ptr;
      State   : Stream_Element_Offset := 0; -- Saved caller's state
      Current : Data_Item_Offset      := 1;
      List    : Data_Item_Ptr_Array (1..Length);
   end record;
   procedure Free is
      new Ada.Unchecked_Deallocation (Sequence, Sequence_Ptr);

   type State_Type is (Feeding, Packet_Processing);
   type State_Machine is abstract new Connection with record
      State  : Stream_Element_Offset := 0;
      Start  : Stream_Element_Offset := 0; -- Saved pointer
      Fed    : Unsigned_64 := 0;  -- Running count of octets processed
      Data   : Sequence_Ptr;
   end record;
   procedure Call
             (  Client  : in out State_Machine;
                Pointer : Stream_Element_Offset;
                Data    : Sequence_Ptr;
                State   : Stream_Element_Offset := 0
             );
   type Shared_Data_Item_Ptr_Ptr is access all Shared_Data_Item_Ptr;
   type Initialization_Stream is new Root_Stream_Type with record
      Shared : aliased Shared_Data_Item_Ptr; -- The latest shared item
      Parent : Shared_Data_Item_Ptr_Ptr;
      Count  : Data_Item_Offset := 0;
      Ignore : Data_Item_Offset := 0;
      Data   : Unbounded_Array;
   end record;
   procedure Add
             (  Stream : in out Initialization_Stream;
                Item   : Data_Item'Class;
                Nested : Data_Item_Offset := 0
             );
   procedure Read
             (  Stream : in out Initialization_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );

   procedure Write
             (  Stream : in out Initialization_Stream;
                Item   : Stream_Element_Array
             );
   function Get_Prefix (Stream : Root_Stream_Type'Class) return String;
   procedure Set_Prefix
             (  Stream : Root_Stream_Type'Class;
                Prefix : Natural
             );
   procedure Check_Initialization_Stream
             (  Item   : Data_Item'Class;
                Stream : Root_Stream_Type'Class
             );

   function To_Integer (Value : Unsigned_8 ) return Integer_8;
   function To_Integer (Value : Unsigned_16) return Integer_16;
   function To_Integer (Value : Unsigned_32) return Integer_32;
   function To_Integer (Value : Unsigned_64) return Integer_64;
   pragma Inline (To_Integer);

   function To_Unsigned (Value : Integer_8 ) return Unsigned_8;
   function To_Unsigned (Value : Integer_16) return Unsigned_16;
   function To_Unsigned (Value : Integer_32) return Unsigned_32;
   function To_Unsigned (Value : Integer_64) return Unsigned_64;
   pragma Inline (To_Unsigned);

   function Self (Item : Data_Item'Class) return Data_Item_Ptr;

   type Data_Block_Ptr is access all Data_Block'Class;
   type Data_Block is new Data_Item with record
      Data        : Sequence_Ptr;
      Initialized : Boolean := False;
   end record;
   function Self (Item : Data_Block'Class) return Data_Block_Ptr;

   type Sequence_Array is array (Positive range <>) of Sequence_Ptr;
   type Alternatives_List (Size : Natural) is record
      List : Sequence_Array (1..Size);
   end record;
   type Alternatives_List_Ptr is access Alternatives_List;

   type Data_Selector_Ptr is access all Data_Selector'Class;
   type Data_Selector is new Data_Item with record
      Alternatives : Alternatives_List_Ptr;
      Current      : Positive := 1;
      Initialized  : Boolean  := False;
   end record;
   function Self (Item : Data_Selector'Class) return Data_Selector_Ptr;
   function Self (Item : Shared_Data_Item'Class)
      return Shared_Data_Item_Ptr;

   generic
      with procedure Put_Line (Line : String) is <>;
   procedure Generic_Dump_Stream
             (  Stream : Initialization_Stream'Class;
                Prefix : String := ""
             );

end GNAT.Sockets.Connection_State_Machine;

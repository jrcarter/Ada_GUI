--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine       Luebeck            --
--  Implementation                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  13:13 14 Sep 2019  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Ada.Tags;               use Ada.Tags;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Unchecked_Deallocation;
with Ada.Containers.Ordered_Maps; -- Changed to use standard library by J. Carter 2021
with System.Address_Image;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine is

   procedure Check_Initialization_Stream
             (  Item   : Data_Item'Class;
                Stream : Root_Stream_Type'Class
             )  is
   begin
      if Stream not in Initialization_Stream'Class then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Stream attribute of "
            &  Expanded_Name (Item'Tag)
            &  " used with a non-initialization stream "
            &  Expanded_Name (Stream'Tag)
         )  );
      end if;
   end Check_Initialization_Stream;

   procedure Generic_Dump_Stream
             (  Stream : Initialization_Stream'Class;
                Prefix : String := ""
             )  is
         use Strings_Edit;
         Trailer : constant String (Prefix'Range) := (others => ' ');
   begin
      Put_Line
      (  Prefix
      &  "stream at "
      &  System.Address_Image (Stream'Address)
      &  " items: "
      &  Image (Integer (Stream.Count))
      &  ", to ignore next: "
      &  Image (Integer (Stream.Ignore))
      );
      for Index in 1..Stream.Count loop
         Put_Line
         (  Trailer
         &  Image (Integer (Index))
         &  " "
         &  Ada.Tags.Expanded_Name (Get (Stream.Data, Index).all'Tag)
         &  " at "
         &  System.Address_Image (Get (Stream.Data, Index).all'Address)
         );
      end loop;
   end Generic_Dump_Stream;

   package body Generic_Dump is
      procedure Put is new Generic_Dump_Stream;
      procedure Put
                (  Item   : Data_Item'Class;
                   Prefix : String := ""
                )  is
      begin
         Put_Line
         (  Prefix
         &  Ada.Tags.Expanded_Name (Item'Tag)
         &  " at "
         &  System.Address_Image (Item'Address)
         );
         declare
            Nested : constant Data_Item_Ptr_Array := Get_Children (Item);
         begin
            if Nested'Length > 0 then
               declare
                  Trailer : constant String (Prefix'Range) :=
                                            (others => ' ');
               begin
                  for Index in Nested'Range loop
                     Put
                     (  Nested (Index).all,
                        Trailer & "   " & Image (Integer (Index)) & " "
                     );
                  end loop;
               end;
            end if;
         end;
      end Put;

      procedure Put
                (  Buffer : in out External_String_Buffer;
                   Prefix : String
                )  is
         Trailer : constant String (Prefix'Range) := (others => ' ');
         This    : Allocator_Data_Ptr := Buffer.Allocators;
      begin
         Put_Line
         (  Prefix
         &  "external buffer at "
         &  System.Address_Image (Buffer'Address)
         &  " allocators: "
         );
         for Index in Positive'Range loop
            exit when This = null;
            Put
            (  Trailer
            &  "   "
            &  Image (Index)
            &  " at "
            &  System.Address_Image (This.all'Address)
            );
            Put_Line
            (  " -> "
            &  Ada.Tags.Expanded_Name (This.Allocator.all'Tag)
            &  " at "
            &  System.Address_Image (This.Allocator.all'Address)
            );
            This := This.Previous;
         end loop;
      end Put;

      procedure Put_Stream
                (  Item   : Data_Item'Class;
                   Prefix : String := ""
                )  is
         Buffer : aliased External_String_Buffer (64);
         Stream : aliased Initialization_Stream;
      begin
         Stream.Shared := Buffer'Unchecked_Access;
         Data_Item'Class'Write (Stream'Access, Item);
         Put (Stream, Prefix);
      end Put_Stream;

      procedure Put_Call_Stack
                (  Client : State_Machine'Class;
                   Prefix : String := ""
                )  is
         Depth : Natural := 0;
         This  : Sequence_Ptr := Client.Data;
      begin
         while This /= null loop
            Depth := Depth + 1;
            This  := This.Caller;
         end loop;
         declare
            Stack : Sequence_Array (1..Depth);
            procedure Dump
                      (  Start  : Positive;
                         Prefix : String
                      )  is
               This : Sequence renames Stack (Start).all;
               List : Data_Item_Ptr_Array renames This.List;
            begin
               for Index in List'Range loop
                  if Index = This.Current then
                     Put_Line
                     (  Prefix
                     &  "->"
                     &  Image (Integer (Index))
                     &  " "
                     &  Ada.Tags.Expanded_Name (List (Index).all'Tag)
                     );
                     if Start < Stack'Last then
                        Dump (Start + 1, Prefix & "   ");
                     end if;
                  else
                     Put_Line
                     (  Prefix
                     &  "  "
                     &  Image (Integer (Index))
                     &  " "
                     &  Ada.Tags.Expanded_Name (List (Index).all'Tag)
                     );
                  end if;
               end loop;
            end Dump;
         begin
            This := Client.Data;
            for Index in reverse Stack'Range loop
               Stack (Index) := This;
               This  := This.Caller;
            end loop;
            Put_Line (Prefix & Ada.Tags.Expanded_Name (Client'Tag));
            Dump (1, Prefix & "   ");
         end;
      end Put_Call_Stack;
   end Generic_Dump;

   package Address_Maps Is new Ada.Containers.Ordered_Maps
      (Key_Type => System.Address, Element_Type => Natural, "<" => System."<");
   Prefixes : Address_Maps.Map;

   function Get_Prefix
            (  Stream : Root_Stream_Type'Class
            )  return String is
   begin
      if Prefixes.Contains (Stream'Address) then
         declare
            Result : constant String := (1 .. Prefixes.Element (Stream'Address) => ' ');
         begin
            return Result & System.Address_Image (Stream'Address) & ' ';
         end;
      else
         return "";
      end if;
   end Get_Prefix;

   procedure Set_Prefix
             (  Stream : Root_Stream_Type'Class;
                Prefix : Natural
             )  is
   begin
      Prefixes.Include (Key => Stream'Address, New_Item => Prefix);
   end Set_Prefix;

   procedure Add
             (  Stream : in out Initialization_Stream;
                Item   : Data_Item'Class;
                Nested : Data_Item_Offset := 0
             )  is
   begin
      if Stream.Ignore > 0 then
         Stream.Ignore := Stream.Ignore - 1;
      else
         Stream.Count := Stream.Count + 1;
         Put (Stream.Data, Stream.Count, Self (Item));
      end if;
      Stream.Ignore := Stream.Ignore + Nested;
   end Add;

   procedure Allocate
             (  Pool      : in out Arena_Pool;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
      Size_In_Characters : constant Storage_Count :=
                  (  (Size * System.Storage_Unit + Character'Size - 1)
                  /  Character'Size
                  );
      Length : Natural renames Pool.Parent.Length;
      Buffer : String  renames Pool.Parent.Buffer;
      Last   : Integer;
   begin
Align_To_The_Margin :
      for Index in 1..Alignment loop
         exit when Length >= Buffer'Last;
         if Buffer (Length + 1)'Address mod Alignment = 0 then
            Last := Length + Natural (Size_In_Characters);
            loop
               exit Align_To_The_Margin when Last >= Buffer'Last;
               exit when (  (  Buffer (Last   + 1)'Address
                            -  Buffer (Length + 1)'Address
                            )
                         <= Size
                         );
               Last := Last + 1;
            end loop;
            Address := Buffer (Length + 1)'Address;
            Length  := Last + 1;
            Pool.Parent.Count := Pool.Parent.Count + 1;
            return;
         end if;
         Length := Length + 1;
      end loop Align_To_The_Margin;
      Raise_Exception
      (  Storage_Error'Identity,
         (  "Arena pool exhausted, "
         &  Image (Length)
         &  " used out of "
         &  Image (Buffer'Last)
         &  ", items allocated "
         &  Image (Pool.Parent.Count)
      )  );
   end Allocate;

   procedure Call
             (  Client  : in out State_Machine;
                Pointer : Stream_Element_Offset;
                Data    : Sequence_Ptr;
                State   : Stream_Element_Offset := 0
             )  is
   begin
      Client.Fed   := Client.Fed + Unsigned_64 (Pointer - Client.Start);
      Client.Start := Pointer;
      Data.Caller  := Client.Data;
      Client.Data  := Data;
      Data.Current := 0;
      Data.State   := State;
   end Call;

   procedure Connected (Client : in out State_Machine) is
      Stream : aliased Initialization_Stream;
   begin
      Initialize (Connection (Client));
      State_Machine'Class'Write
      (  Stream'Access,
         State_Machine'Class (Client)
      );
      Free (Client.Data);
      if Stream.Count > 0 then
         Client.Data :=
            new Sequence'
                (  Length  => Stream.Count,
                   Caller  => null,
                   State   => 0,
                   Current => 1,
                   List    => Stream.Data.Vector (1..Stream.Count)
                );
      end if;
   end Connected;

   procedure Deallocate
             (  Pool      : in out Arena_Pool;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
   begin
      null;
   end Deallocate;

   procedure End_Of_Subsequence
             (  Item    : in out Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      null;
   end End_Of_Subsequence;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Pool   : Arena_Pool
             )  is
   begin
      null;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : State_Machine
             )  is
   begin
      null;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Block
             )  is
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         Count : Data_Item_Offset := 0;
         Block : Data_Block'Class renames Self (Item).all;
         This  : Initialization_Stream'Class renames
                 Initialization_Stream'Class (Stream.all);
      begin
         if not Item.Initialized then
            declare
               Counter : aliased Initialization_Stream;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Block.Initialized := True;
               Data_Block'Class'Write (Counter'Access, Block);
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "Block "
                     &  Expanded_Name (Block'Tag)
                     &  " is empty (contains no items)"
                  )  );
               end if;
               Self (Item).Data :=
                  new Sequence'
                      (  Length  => Counter.Count - 1,
                         Caller  => null,
                         State   => 0,
                         Current => 1,
                         List    => Counter.Data.Vector
                                    (  2
                                    .. Counter.Count
                      )             );
               Count := Data_Item_Offset (Get_Size (Item)) - 1;
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Item
             )  is
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         This : Initialization_Stream'Class renames
                Initialization_Stream'Class (Stream.all);
      begin
         Add (This, Item);
      end;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Selector
             )  is
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         Selector : Data_Selector'Class renames
                    Self (Data_Selector'Class (Item)).all;
         Count    : Data_Item_Offset := 0;
         This     : Initialization_Stream'Class renames
                    Initialization_Stream'Class (Stream.all);
      begin
         if not Selector.Initialized then
            Selector.Initialized := True;
            declare
               Counter : aliased Initialization_Stream;
            begin
               Selector.Initialized := True;
               Data_Selector'Class'Write (Counter'Access, Selector);
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "Selector "
                     &  Expanded_Name (Data_Item'Class (Item)'Tag)
                     &  " contains no alternatives"
                  )  );
               end if;
               Selector.Alternatives :=
                  new Alternatives_List
                      (  Natural (Counter.Count)
                      -  1
                      );
               declare
                  This : Data_Item_Ptr;
                  List : Sequence_Array renames
                         Item.Alternatives.List;
               begin
                  for Index in List'Range loop
                     This :=
                        Get
                        (  Counter.Data,
                           Data_Item_Offset (Index) + 1
                        );
                     List (Index) :=
                        new Sequence'
                            (  Length  => 1,
                               Caller  => null,
                               State   => 0,
                               Current => 1,
                               List    => (1 => This)
                            );
                  end loop;
               end;
               Count := Data_Item_Offset (Get_Size (Item)) - 1;
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Shared_Data_Item
             )  is
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         This : Initialization_Stream'Class renames
                Initialization_Stream'Class (Stream.all);
      begin
         if not Item.Initialized then
            declare
               Shared : Shared_Data_Item'Class renames
                        Self (Item).all;
            begin
               Shared.Initialized := True;
               Shared.Previous    := This.Shared;
               This.Shared        := Shared'Unchecked_Access;
            end;
         end if;
         Add (This, Item);
      end;
   end Enumerate;

   procedure Erase (Buffer : in out External_String_Buffer) is
      This : Allocator_Data_Ptr := Buffer.Allocators;
      Next : Allocator_Data_Ptr;
   begin
      while This /= null loop
         Next := This.Previous;
         This.Previous := null;
         Freed (This.Allocator.all);
         This := Next;
      end loop;
      Buffer.Length := 0;
      Buffer.Count  := 0;
      Buffer.Allocators := null;
   end Erase;

   procedure External_Initialize
             (  Item   : Data_Item'Class;
                Shared : Shared_Data_Item_Ptr := null
             )  is
      Stream : aliased Initialization_Stream;
   begin
      Stream.Shared := Shared;
      Data_Item'Class'Write (Stream'Access, Item);
   end External_Initialize;

   procedure Feed
             (  Item    : in out Shared_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      State := 0;
   end Feed;

   procedure Feed
             (  Item    : in out External_String_Buffer;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Erase (Item); -- Erase the buffer
      Feed (Shared_Data_Item (Item), Data, Pointer, Client, State);
   end Feed;

   procedure Feed
             (  Item    : in out Data_Block;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Call (Client, Pointer, Item.Data);
      State := 0;
   end Feed;

   procedure Feed
             (  Item    : in out Data_Null;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      State := 0;
   end Feed;

   procedure Feed
             (  Item    : in out Data_Selector;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Call (Client, Pointer, Item.Alternatives.List (Item.Current));
      State := 0;
   end Feed;

   procedure Finalize (Client : in out State_Machine) is
   begin
      if Client.Data /= null then
         while Client.Data.Caller /= null loop
            Client.Data := Client.Data.Caller;
         end loop;
         Free (Client.Data);
      end if;
      Finalize (Connection (Client));
   end Finalize;

   procedure Finalize (Item : in out Data_Block) is
   begin
      Free (Item.Data);
   end Finalize;

   procedure Finalize (Item : in out Data_Selector) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Alternatives_List,
                Alternatives_List_Ptr
             );
   begin
      if Item.Alternatives /= null then
         declare
            List : Sequence_Array renames Item.Alternatives.List;
         begin
            for Index in List'Range loop
               Free (List (Index));
            end loop;
         end;
         Free (Item.Alternatives);
      end if;
   end Finalize;

   procedure Finalize (Buffer : in out External_String_Buffer) is
   begin
      Erase (Buffer);
      Buffer.Allocators := null;
      Finalize (Shared_Data_Item (Buffer));
   end Finalize;

   procedure Freed (Item : in out Data_Item) is
   begin
      null;
   end Freed;

   package From_Block_Address is
      new System.Address_To_Access_Conversions (Data_Block'Class);

   package From_Item_Address is
      new System.Address_To_Access_Conversions (Data_Item'Class);

   package From_Selector_Address is
      new System.Address_To_Access_Conversions (Data_Selector'Class);

   package From_Shared_Item_Address is
      new System.Address_To_Access_Conversions (Shared_Data_Item'Class);

   function Get_Alternative (Item : Data_Selector) return Positive is
   begin
      return Item.Current;
   end Get_Alternative;

   function Get_Alternatives_Number (Item : Data_Selector)
      return Positive is
   begin
      if not Item.Initialized or else Item.Alternatives = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      return Positive (Item.Alternatives.Size);
   end Get_Alternatives_Number;

   function Get_Children
            (  Item : Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      return (1..0 => null);
   end Get_Children;

   function Get_Children
            (  Item : Data_Block
            )  return Data_Item_Ptr_Array is
      Length : Data_Item_Offset  := 0;
   begin
      if not Item.Initialized or else Item.Data = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Block "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      return Item.Data.List;
   end Get_Children;

   function Get_Children
            (  Item : Data_Selector
            )  return Data_Item_Ptr_Array is
      Length : Data_Item_Offset  := 0;
   begin
      if not Item.Initialized or else Item.Alternatives = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      for Index in Item.Alternatives.List'Range loop
         Length := Length + Item.Alternatives.List (Index).List'Length;
      end loop;
      declare
         Result : Data_Item_Ptr_Array (1..Length);
         No     : Data_Item_Address := 1;
      begin
         for Index in Item.Alternatives.List'Range loop
            declare
               Alternative : Data_Item_Ptr_Array renames
                             Item.Alternatives.List (Index).List;
            begin
               Result (No..No + Alternative'Length - 1) := Alternative;
               No := No + Alternative'Length;
            end;
         end loop;
         return Result;
      end;
   end Get_Children;

   function Get_Length (Item : Data_Block) return Positive is
   begin
      if not Item.Initialized or else Item.Data = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Block "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      return Positive (Item.Data.Length);
   end Get_Length;

   function Get_Size (Item : Data_Block) return Natural is
   begin
      if not Item.Initialized or else Item.Data = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Block "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      declare
         Result : Natural := 1;
         List   : Data_Item_Ptr_Array renames Item.Data.List;
      begin
         for Index in List'Range loop
            Result := Result + Get_Size (List (Index).all);
         end loop;
         return Result;
      end;
   end Get_Size;

   function Get_Size (Item : Data_Item) return Natural is
   begin
      return 1;
   end Get_Size;

   function Get_Size (Item : Data_Selector) return Natural is
   begin
      if not Item.Initialized or else Item.Alternatives = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      declare
         Result   : Natural := 1;
         Sequence : Sequence_Array renames Item.Alternatives.List;
      begin
         for Index in Sequence'Range loop
            declare
               List : Data_Item_Ptr_Array renames Sequence (Index).List;
            begin
               for Index in List'Range loop
                  Result := Result + Get_Size (List (Index).all);
               end loop;
            end;
         end loop;
         return Result;
      end;
   end Get_Size;

   procedure Read
             (  Stream : in out Initialization_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Last := Item'Last;
   end Read;

   procedure Received
             (  Client  : in out State_Machine;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Pointer := Data'First;
      while Pointer <= Data'Last loop
         Client.Start := Pointer;
         Feed
         (  Client.Data.List (Client.Data.Current).all,
            Data,
            Pointer,
            Client,
            Client.State
         );
         Client.Fed :=
            Client.Fed + Unsigned_64 (Pointer - Client.Start);
         if Client.State = 0 then -- Done with this item
            Client.Data.Current := Client.Data.Current + 1;
            while Client.Data.Current > Client.Data.Length loop
               if Client.Data.Caller = null then
                  Client.Data.Current := 1;
                  Process_Packet (State_Machine'Class (Client));
                  exit;
               end if;
               Client.State := Client.Data.State; -- Restore state
               Client.Data  := Client.Data.Caller;
               if Client.State /= 0 then
                  End_Of_Subsequence
                  (  Client.Data.List (Client.Data.Current).all,
                     Data,
                     Pointer,
                     Client,
                     Client.State
                  );
                  exit when Client.State /= 0;
               end if;
               Client.Data.Current := Client.Data.Current + 1;
            end loop;
         else
            exit when Pointer > Data'Last; -- All data consumed
            Raise_Exception
            (  Status_Error'Identity,
               (  "Unprocessed data left when after return from "
               &  Expanded_Name
                  (  Client.Data.List
                     (  Client.Data.Current
                     ) .all'Tag
            )  )  );
         end if;
      end loop;
   end Received;

   function Self (Item : Data_Block'Class) return Data_Block_Ptr is
      use From_Block_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self (Item : Data_Item'Class) return Data_Item_Ptr is
      use From_Item_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self (Item : Data_Selector'Class)
      return Data_Selector_Ptr is
      use From_Selector_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self (Item : Shared_Data_Item'Class)
      return Shared_Data_Item_Ptr is
      use From_Shared_Item_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Alternative
             (  Item        : in out Data_Selector;
                Alternative : Positive
             )  is
   begin
      if Alternative > Get_Alternatives_Number (Item) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Invalid alternative number of selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
         )  );
      end if;
      Item.Current := Alternative;
   end Set_Alternative;

   function Storage_Size
            (  Pool : Arena_Pool
            )  return Storage_Count is
   begin
      return Pool.Parent.Buffer'Size / System.Storage_Unit;
   end Storage_Size;

   function To_Integer (Value : Unsigned_8) return Integer_8 is
   begin
      if Value < 2**7 then
         return Integer_8 (Value);
      else
         return Integer_8 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Integer (Value : Unsigned_16) return Integer_16 is
   begin
      if Value < 2**15 then
         return Integer_16 (Value);
      else
         return Integer_16 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Integer (Value : Unsigned_32) return Integer_32 is
   begin
      if Value < 2**31 then
         return Integer_32 (Value);
      else
         return Integer_32 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Integer (Value : Unsigned_64) return Integer_64 is
   begin
      if Value < 2**63 then
         return Integer_64 (Value);
      else
         return Integer_64 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Unsigned (Value : Integer_8) return Unsigned_8 is
   begin
      if Value >= 0 then
         return Unsigned_8 (Value);
      else
         return not Unsigned_8 (-(Value + 1));
      end if;
   end To_Unsigned;

   function To_Unsigned (Value : Integer_16) return Unsigned_16 is
   begin
      if Value >= 0 then
         return Unsigned_16 (Value);
      else
         return not Unsigned_16 (-(Value + 1));
      end if;
   end To_Unsigned;

   function To_Unsigned (Value : Integer_32) return Unsigned_32 is
   begin
      if Value >= 0 then
         return Unsigned_32 (Value);
      else
         return not Unsigned_32 (-(Value + 1));
      end if;
   end To_Unsigned;

   function To_Unsigned (Value : Integer_64) return Unsigned_64 is
   begin
      if Value >= 0 then
         return Unsigned_64 (Value);
      else
         return not Unsigned_64 (-(Value + 1));
      end if;
   end To_Unsigned;

   procedure Write
             (  Stream : in out Initialization_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      null;
   end Write;

end GNAT.Sockets.Connection_State_Machine;

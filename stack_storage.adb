--                                                                    --
--  package Stack_Storage           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  09:21 06 Oct 2019  --
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

with Ada.Unchecked_Deallocation;

package body Stack_Storage is
   use Block_Arrays;
   use Object_Ptr_Array;

   procedure Free is new Ada.Unchecked_Deallocation (Block, Block_Ptr);

   procedure New_Block
             (  Stack : in out Pool;
                Size : Storage_Count
             )  is
      Ptr : Block_Ptr;
   begin
      if Stack.Block_Size < Size * 2 then
         Stack.Block_Size := Size * Storage_Count (Stack.Items_Number);
      end if;
      Ptr := new Block (Stack.Block_Size);
      Put (Stack.Blocks, Stack.Current + 1, Ptr);
      Stack.Current    := Stack.Current + 1;
      Stack.Last       := Stack.Last    + 1;
      Stack.Total_Size := Stack.Total_Size + Stack.Block_Size;
   exception
      when others =>
         Free (Ptr);
         raise;
   end New_Block;

   procedure Allocate
             (  Stack     : in out Pool;
                Place     : out Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
   begin
      if Stack.Blocks.Vector = null then
         New_Block (Stack, Size);
      end if;
      loop
         declare
            This  : Block renames Get (Stack.Blocks, Stack.Current).all;
            Index : Storage_Offset := This.Free;
         begin
            if This.Free <= This.Memory'Last then
               declare
                  Shift : constant Storage_Offset :=
                          This.Memory (This.Free)'Address mod Alignment;
               begin
                  if Shift /= 0 then
                     Index := Index + Alignment - Shift;
                  end if;
                  if Index + Size <= This.Memory'Last + 1 then
                     Place := This.Memory (Index)'Address;
                     This.Free := Index + Size;
                     return;
                  end if;
               end;
            end if;
         end;
         if (  Stack.Current >= Stack.Blocks.Vector'Last
            or else
               Stack.Blocks.Vector (Stack.Current + 1) = null
            )
         then
            New_Block (Stack, Size);
         else
            Stack.Current := Stack.Current + 1;
         end if;
      end loop;
   exception
      when Error : others =>
         Raise_Exception
         (  Storage_Error'Identity,
            Exception_Message (Error)
         );
   end Allocate;

   procedure Deallocate
             (  Stack     : in out Pool;
                Place     : Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
   begin
      loop
         declare
            This : Block renames Get (Stack.Blocks, Stack.Current).all;
         begin
            if (  Place >= This.Memory (This.Memory'First)'Address
               and then
                  Place <= This.Memory (This.Memory'Last)'Address
               )
            then
               --
               -- Within the current segment. Free everything up to  the
               -- specified address in this segment and return.
               --
               This.Free :=
                  Place - This.Memory (This.Memory'First)'Address + 1;
               return;
            else
               --
               -- Outside  the  current  segment. Free everything in the
               -- segment. If the segment is smaller  than  the  current
               -- estimated segment size, it is deleted.
               --
               if This.Size < Stack.Block_Size then
                  Stack.Total_Size := Stack.Total_Size - This.Size;
                  if Stack.Last = Stack.Current then
                     Put (Stack.Blocks, Stack.Current, null);
                  else
                     Put
                     (  Stack.Blocks,
                        Stack.Current,
                        Stack.Blocks.Vector (Stack.Last)
                     );
                     Stack.Blocks.Vector (Stack.Last) := null;
                  end if;
                  Stack.Last := Stack.Last - 1;
               else
                  This.Free := This.Memory'First;
               end if;
               if Stack.Current > 1 then
                  Stack.Current := Stack.Current - 1;
               else
                  raise Storage_Error;
               end if;
            end if;
         end;
      end loop;
   exception
      when others =>
         raise Storage_Error;
   end Deallocate;

   procedure Deallocate_All (Stack : in out Pool) is
   begin
      if Stack.Current > 0 then
         loop
            Get (Stack.Blocks, Stack.Current).Free := 1;
            exit when Stack.Current = 1;
            Stack.Current := Stack.Current - 1;
         end loop;
      end if;
      Stack.Total_Size := 0;
   end Deallocate_All;

   function Get_Last_Segment (Stack : Pool) return Natural is
   begin
      return Natural (Stack.Current);
   end Get_Last_Segment;

   procedure Get_Segment_Data
             (  Stack : Pool;
                Index : Positive;
                Size  : out Storage_Count;
                Used  : out Storage_Count;
                Start : out Address
             )  is
      This : Block renames Get (Stack.Blocks, Block_Index (Index)).all;
   begin
      Size  := This.Size;
      Used  := This.Size - This.Free + 1;
      Start := This.Memory (This.Memory'First)'Address;
   end Get_Segment_Data;

   function Get_Segments_Number (Stack : Pool) return Natural is
   begin
      return Natural (Stack.Last);
   end Get_Segments_Number;

   function Storage_Size (Stack : Pool) return Storage_Count is
   begin
      return Stack.Total_Size;
   end Storage_Size;

end Stack_Storage;

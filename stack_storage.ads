--                                                                    --
--  package Stack_Storage           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  13:09 10 Mar 2013  --
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
--  This package implements a dynaically allocated stack which acts as a
--  storage pool. The type Stack is a descendant  of  Root_Storage_Pool.
--  Note that pool objects have to be allocated and deallocated in LIFO.
--  Note also that the  pool  should  be  protected  from  a  concurrent
--  access.
--
with Generic_Unbounded_Ptr_Array;

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

package Stack_Storage is
--
-- Pool -- The stack pool
--
--    Initial_Size - Of the stack segments
--    Items_Number - The number of items in a segment
--
-- A stack pool consists of contiguous segments allocated dynamically as
-- necessary. The discriminants control the stack  segments  allocation.
-- Initial_Size determines the initial default size of a newly allocated
-- segment.  If  this  size  is  less  than the size of the object being
-- allocated  the  default  size is set to the object size multiplied to
-- Items_Number. This value will  then be used  as the default size  for
-- all further  segments.  The segments allocated  earlier having lesser
-- size  will be freed when possible.  Otherwise, they remain  allocated
-- until pool  destruction. Upon stack destruction,  all  stack segments
-- are deallocated. No checks made whether some objects remain allocated
-- on the stack.
--
   type Pool
        (  Initial_Size : Storage_Count;
           Items_Number : Positive
        )  is new Root_Storage_Pool with private;
--
-- Allocate -- Overrides System.Storage_Pools...
--
   procedure Allocate
             (  Stack     : in out Pool;
                Place     : out Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
--
-- Deallocate -- Overrides System.Storage_Pools...
--
-- The object has to be deallocated in the order they were allocated. No
-- checks made with this respect. It is also  not  checked  whether  the
-- address of the object being deallocated is correct.
--
   procedure Deallocate
             (  Stack     : in out Pool;
                Place     : Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
--
-- Deallocate_All -- Erase pool contents
--
--    Stack - The storage pool
--
-- This procedure  deallocates everything allocated in the pool,  should
-- be used with great care.
--
   procedure Deallocate_All (Stack : in out Pool);
--
-- Get_Last_Segment -- The number of the last segment in use
--
--    Stack - The storage pool
--
-- Returns :
--
--    The last segment holding some allocated data
--
   function Get_Last_Segment (Stack : Pool) return Natural;
--
-- Get_Segments_Number -- Get total number segments in the pool
--
--    Stack - The storage pool
--
-- Returns :
--
--    Number of allocated segments, including unused ones
--
   function Get_Segments_Number (Stack : Pool) return Natural;
--
-- Get_Segment_Data -- Get description of a segment
--
--    Stack - The storage pool
--    Index - The segment number 1..Get_Segments_Number
--    Size  - The total size of the segment
--    Used  - The used space in the segment
--    Start - The first address in the segment
--
-- The first free address is Start + Used.  Free space in the segment is
-- Size - Used.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Get_Segment_Data
             (  Stack : Pool;
                Index : Positive;
                Size  : out Storage_Count;
                Used  : out Storage_Count;
                Start : out Address
             );
--
-- Storage_Size -- Overrides System.Storage_Pools...
--
   function Storage_Size (Stack : Pool) return Storage_Count;

private
   type Block_Index is new Integer;
--
-- Block -- A contiguous segment of a stack pool
--
--    Size - The segment size
--
-- The  field Free specifies the first free address in the segment. When
-- a segment is allocated it is set to point to the first element of the
-- field  Memory.  As  the  memory  is  consumed  Free  moves to further
-- elements.
--
   type Block (Size : Storage_Count) is record
      Free   : Storage_Offset := 1;
      Memory : aliased Storage_Array (1..Size);
   end record;
   type Block_Ptr is access Block;
   type Block_Ptr_Array is array (Block_Index range <>) of Block_Ptr;
--
-- Block_Arrays -- A package providing unbounded arrays of segments
--
   package Block_Arrays is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Block_Index,
             Object_Type           => Block,
             Object_Ptr_Type       => Block_Ptr,
             Object_Ptr_Array_Type => Block_Ptr_Array
          );

   type Pool
        (  Initial_Size : Storage_Count;
           Items_Number : Positive
        )  is new Root_Storage_Pool with
   record
      Total_Size : Storage_Count := 0;
      Block_Size : Storage_Count := Initial_Size;
      Current    : Block_Index   := 0;
      Last       : Block_Index   := 0;
      Blocks     : Block_Arrays.Unbounded_Ptr_Array;
   end record;

end Stack_Storage;

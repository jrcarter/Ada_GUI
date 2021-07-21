--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Handle.Generic_Unbounded_Array       Luebeck            --
--  Interface                                      Spring, 2003       --
--                                                                    --
--                                Last revision :  20:41 21 Jul 2017  --
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
--  This  package defines a generic type Unbounded_Array. An instance of
--  Unbounded_Array is a dynamically  expanded  vector  of  pointers  to
--  automatically  collected  objects  (see  the  package  Object).  The
--  package has the following generic parameters:
--
--     Index_Type   - The array index type
--     Handle_Type  - A handle type derived from Handle
--     Minimal_Size - Minimal additionally allocated size
--     Increment    - By which the vector is enlarged
--
with Generic_Unbounded_Array;

generic
   type Index_Type is (<>);
   type Handle_Type is new Handle with private;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Object.Handle.Generic_Unbounded_Array is
   type Unbounded_Array is
      new Ada.Finalization.Controlled with private;
--
-- Adjust -- Assignment (part of)
--
--    Container - The array
--
-- The  assignment  does  not copy the array. It only increments the use
-- count  of  the array body. Only destructive operations like Put cause
-- array replication.
--
   procedure Adjust (Container : in out Unbounded_Array);
--
-- Erase -- Delete all array items
--
--    Container - The array
--
-- This procedure makes Container empty.
--
   procedure Erase (Container : in out Unbounded_Array);
--
-- Finalize -- Destructor
--
--    Container - The array
--
   procedure Finalize (Container : in out Unbounded_Array);
--
-- First -- The lower array bound
--
--    Container - The array
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--    Constraint_Error - The array is presently empty
--
   function First (Container : Unbounded_Array) return Index_Type;
--
-- Get -- Get an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- Returns :
--
--    The element or null if none
--
   function Get
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Object_Type_Ptr;
--
-- Last -- The upper array bound
--
--    Container - The array
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--    Constraint_Error - The array is presently empty
--
   function Last (Container : Unbounded_Array) return Index_Type;
--
-- Put -- Replace an array element by its index
--
--    Container - The array
--    Index     - Of the element
--    Element   - The new element (a pointer/handle to)
--
-- The array is expanded as necessary.
--
   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Index_Type;
                Element   : Object_Type_Ptr
             );
   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Index_Type;
                Element   : Handle_Type
             );
--
-- Ref -- Get a handle to an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- Returns :
--
--    A handle to the element
--
-- Exceptions :
--
--    Constraint_Error - No such element
--
   function Ref
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Handle_Type;

private
   pragma Inline (First);
   pragma Inline (Get);
   pragma Inline (Last);
   pragma Inline (Ref);

   type Object_Ptr_Array_Type is
      array (Index_Type range <>) of Object_Type_Ptr;

   package Object_Ptr_Array is
      new Standard.Generic_Unbounded_Array
          (  Index_Type        => Index_Type,
             Object_Type       => Object_Type_Ptr,
             Object_Array_Type => Object_Ptr_Array_Type,
             Null_Element      => null,
             Minimal_Size      => Minimal_Size,
             Increment         => Increment
        );
   type Unbounded_Array_Body is new Object.Entity with record
      Data : Object_Ptr_Array.Unbounded_Array;
   end record;
   type Unbounded_Array_Body_Ptr is access Unbounded_Array_Body;

   type Unbounded_Array is new Ada.Finalization.Controlled with record
      Ptr : Unbounded_Array_Body_Ptr;
   end record;

end Object.Handle.Generic_Unbounded_Array;

--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Unbounded_Ptr_Array                 Luebeck            --
--  Interface                                      Spring, 2002       --
--                                                                    --
--                                Last revision :  21:24 10 Nov 2009  --
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
--  This package defines a generic type Unbounded_Ptr_Array. An instance
--  of Unbounded_Ptr_Array is a dynamically expanded vector of  pointers
--  to  elements. Upon destruction objects pointed by array elements are
--  destroyed. Same happens when  an  element  is  replaced.  Thus  once
--  pointer  to an allocated object is put into an array, its is no more
--  the user responsibility to care on its destruction. A consequence of
--  this  is  that  pointers  to  statically  or stack-allocated objects
--  cannot  be  put  there.  The  package  has  the  following   generic
--  parameters:
--
--     Index_Type            - The array index type
--     Object_Type           - The object type
--     Object_Ptr_Type       - The object pointer type
--     Object_Ptr_Array_Type - The array of pointers type
--     Minimal_Size          - Minimal additionally allocated size
--     Increment             - By which the vector is enlarged
--
with Generic_Unbounded_Array;

generic
   type Index_Type is (<>);
   type Object_Type (<>) is limited private;
   type Object_Ptr_Type is access Object_Type;
   type Object_Ptr_Array_Type is
      array (Index_Type range <>) of Object_Ptr_Type;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Generic_Unbounded_Ptr_Array is
   package Object_Ptr_Array is
      new Generic_Unbounded_Array
          (  Index_Type,
             Object_Ptr_Type,
             Object_Ptr_Array_Type,
             null,
             Minimal_Size,
             Increment
          );
   type Unbounded_Ptr_Array is
      new Object_Ptr_Array.Unbounded_Array with null record;
--
-- Erase -- Delete all array items
--
--    Container - The array
--
-- This procedure makes Container empty.
--
   procedure Erase (Container : in out Unbounded_Ptr_Array);
--
-- Finalize -- Destructor
--
--    Container - The array
--
   procedure Finalize (Container : in out Unbounded_Ptr_Array);
--
-- Get -- Get an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- This an equivalent to Container.Vector (Index). However it  does  not
-- fail if no vector allocated or index is not in the vector  range.  It
-- returns null instead.
--
-- Returns :
--
--    The element or null if none
--
   function Get
            (  Container : Unbounded_Ptr_Array;
               Index     : Index_Type
            )  return Object_Ptr_Type;
--
-- Put -- Replace an array element by its index
--
--    Container - The array
--    Index     - Of the element
--    Element   - Pointer to the new element
--
-- The  array is expanded as necessary. If the replaced array element is
-- not null then the object it points is destroyed. Note that the object
-- pointed by Element is not copied. Thus it is not a responsibility  of
-- the caller to destroy the object. It will be automatically  destroyed
-- upon array destruction or replacing the element in the array.
--
   procedure Put
             (  Container : in out Unbounded_Ptr_Array;
                Index     : Index_Type;
                Element   : Object_Ptr_Type
             );

private
   pragma Inline (Get);
end Generic_Unbounded_Ptr_Array;

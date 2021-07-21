--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Unbounded_Array                     Luebeck            --
--  Interface                                      Spring, 2002       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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
--  Unbounded_Array is a dynamically expanded vector  of  elements.  The
--  implementation  keeps  vector  contiguous,  so  it  might  be   very
--  inefficient  to  put complex data structures into the array. In many
--  cases it is better to put  pointers  to  elements  there.  See  also
--  Generic_Unbounded_Ptr_Array  which  instantiates Unbounded_Array for
--  this purpose. The type wraps the component Vector which is a pointer
--  to an array of elements. One can use Vector to access array elements
--  and query its present bounds, which are rather arbitrary. The unused
--  elements  of  the  array  vector  are  padded  using a distinguished
--  null-element value. The package generic parameters are:
--
--     Index_Type        - The array index type
--     Object_Type       - The array element type
--     Object_Array_Type - The array type
--     Null_Element      - To pad unused array elements
--     Minimal_Size      - Minimal additionally allocated size
--     Increment         - By which the vector is enlarged if necessary
--
--  The parameter Increment controls  array  vector  size  growth.  When
--  there  is  no free space in the vector then it is enlarged by Size *
--  Increment / 100. Here Size is the current vector size. The allocated
--  amount  of  elements  cannot be less than the parameter Minimal_Size
--  specifies. So it will be the initial vector  size  after  the  first
--  element is put in.
--
with Ada.Finalization;

generic
   type Index_Type is (<>);
   type Object_Type is private;
   type Object_Array_Type is array (Index_Type range <>) of Object_Type;
   Null_Element : Object_Type;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Generic_Unbounded_Array is
   type Object_Array_Ptr is access Object_Array_Type;
   type Unbounded_Array is
      new Ada.Finalization.Limited_Controlled with
   record
      Vector : Object_Array_Ptr := null;
   end record;
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
-- Fetch -- Get an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- This  function  returns  the  element  corresponding to Index. If the
-- container does not have it, the result is Null_Element.
--
-- Returns :
--
--    The element
--
   function Fetch
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Object_Type;
--
-- Get -- Get an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- This  an  equivalent  to Container.Vector (Index). However, subscript
-- checks cannot be suppressed for Get.
--
-- Returns :
--
--    The element
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Object_Type;
--
-- Put -- Replace an array element by its index
--
--    Container - The array
--    Index     - Of the element
--    Element   - To put in
--
-- The array is expanded as necessary.
--
   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Index_Type;
                Element   : Object_Type
             );

private
   pragma Inline (Fetch);
   pragma Inline (Get);
end Generic_Unbounded_Array;

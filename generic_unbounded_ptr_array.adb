--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Unbounded_Ptr_Array                 Luebeck            --
--  Implementation                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

with Ada.Unchecked_Deallocation;

package body Generic_Unbounded_Ptr_Array is
   use type Object_Ptr_Array.Object_Array_Ptr;

   procedure Delete is
      new Ada.Unchecked_Deallocation (Object_Type, Object_Ptr_Type);

   procedure Erase (Container : in out Unbounded_Ptr_Array)
      renames Finalize;

   procedure Finalize (Container : in out Unbounded_Ptr_Array) is
   begin
      if Container.Vector /= null then
         for Index in Container.Vector'range loop
            Delete (Container.Vector (Index));
         end loop;
      end if;
   end Finalize;

   function Get
            (  Container : Unbounded_Ptr_Array;
               Index     : Index_Type
            )  return Object_Ptr_Type is
   begin
      if (  Container.Vector = null
         or else
            Index not in Container.Vector'range
         )
      then
         return null;
      else
         return Container.Vector (Index);
      end if;
   end Get;

   procedure Put
             (  Container : in out Unbounded_Ptr_Array;
                Index     : Index_Type;
                Element   : Object_Ptr_Type
             )  is
   begin
      if (  Container.Vector /= null
         and then
            Index in Container.Vector'Range
         )
      then
         if Container.Vector (Index) /= Element then
            Delete (Container.Vector (Index));
            Container.Vector (Index) := Element;
         end if;
      else
         Object_Ptr_Array.Put
         (  Object_Ptr_Array.Unbounded_Array (Container),
            Index,
            Element
         );
      end if;
   end Put;

end Generic_Unbounded_Ptr_Array;

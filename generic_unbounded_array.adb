--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Unbounded_Array                     Luebeck            --
--  Implementation                                 Spring, 2002       --
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

with Ada.Unchecked_Deallocation;

package body Generic_Unbounded_Array is

   procedure Delete is
      new Ada.Unchecked_Deallocation
          (  Object_Array_Type,
             Object_Array_Ptr
          );

   procedure Erase (Container : in out Unbounded_Array)
      renames Finalize;

   procedure Finalize (Container : in out Unbounded_Array) is
   begin
      Delete (Container.Vector);
   end Finalize;

   function Fetch
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Object_Type is
   begin
      if (  Container.Vector = null
         or else
            Index not in Container.Vector'Range
         )
      then
         return Null_Element;
      else
         return Container.Vector (Index);
      end if;
   end Fetch;

   function Get
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Object_Type is
   begin
      if (  Container.Vector = null
         or else
            Index not in Container.Vector'Range
         )
      then
         raise Constraint_Error;
      else
         return Container.Vector (Index);
      end if;
   end Get;

   function "+" (Left : Index_Type; Right : Natural)
      return Index_Type is
      pragma Inline ("+");
   begin
      if (  Index_Type'Pos (Left) + Natural'Pos (Right)
         >= Index_Type'Pos (Index_Type'Last)
         )
      then
         return Index_Type'Last;
      else
         return
            Index_Type'Val
            (  Index_Type'Pos (Left)
            +  Natural'Pos (Right)
            );
      end if;
   end "+";

   function "-" (Left : Index_Type; Right : Natural)
      return Index_Type is
      pragma Inline ("-");
   begin
      if (  Index_Type'Pos (Left)
         <= Index_Type'Pos (Index_Type'First) + Natural'Pos (Right)
         )
      then
         return Index_Type'First;
      else
         return Index_Type'Val (Index_Type'Pos (Left) - Right);
      end if;
   end "-";

   function Max (Left, Right : Index_Type) return Index_Type is
      pragma Inline (Max);
   begin
      if Index_Type'Pos (Left) > Index_Type'Pos (Right) then
         return Left;
      else
         return Right;
      end if;
   end Max;

   function Min (Left, Right : Index_Type) return Index_Type is
      pragma Inline (Min);
   begin
      if Index_Type'Pos (Left) < Index_Type'Pos (Right) then
         return Left;
      else
         return Right;
      end if;
   end Min;

   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Index_Type;
                Element   : Object_Type
             )  is
   begin
      if Container.Vector = null then
         Container.Vector :=
            new Object_Array_Type'
                (Index..Index + Minimal_Size => Null_Element);
      elsif Index not in Container.Vector'range then
         declare
            New_Vector : Object_Array_Ptr;
            Inc : constant Natural :=
                    Natural'Max
                    (  Minimal_Size,
                       (Container.Vector'Length * Increment) / 100
                    );
         begin
            if (  Index_Type'Pos (Index)
               <  Index_Type'Pos (Container.Vector'First)
               )
            then
               New_Vector :=
                  new Object_Array_Type'
                      (  Min (Index, Container.Vector'Last - Inc)
                      .. Container.Vector'Last
                      => Null_Element
                      );
            else
               New_Vector :=
                  new Object_Array_Type'
                      (  Container.Vector'First
                      .. Max (Index, Container.Vector'First + Inc)
                      => Null_Element
                      );
            end if;
            New_Vector (Container.Vector'range) :=
               Container.Vector.all;
            Delete (Container.Vector);
            Container.Vector := New_Vector;
         end;
      end if;
      Container.Vector (Index) := Element;
   end Put;

end Generic_Unbounded_Array;

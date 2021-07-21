--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Handle.Unbounded_Array               Luebeck            --
--  Implementation                                 Spring, 2003       --
--                                                                    --
--                                Last revision :  10:33 11 May 2019  --
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

package body Object.Handle.Generic_Unbounded_Array is
   use type Object_Ptr_Array.Object_Array_Ptr;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Unbounded_Array_Body,
             Unbounded_Array_Body_Ptr
          );

   procedure Release (Ptr : in out Unbounded_Array_Body_Ptr) is
   begin
      if Ptr /= null then
         declare
            Object : Unbounded_Array_Body renames Ptr.all;
         begin
            case Object.Use_Count is
               when 0 =>
                  raise Program_Error;
               when 1 =>
                  Object.Use_Count := 0;
	          if Ptr.Data.Vector /= null then
                     declare
                        Vector : Object_Ptr_Array_Type renames
                                 Ptr.Data.Vector.all;
                     begin
                        for Index in Vector'Range loop
                           Release (Vector (Index));
                        end loop;
                     end;
                  end if;
                  Free (Ptr);
               when others =>
                  Object.Use_Count := Object.Use_Count - 1;
            end case;
         end;
      end if;
   end Release;

   procedure Adjust (Container : in out Unbounded_Array) is
   begin
      if Container.Ptr /= null then
         Container.Ptr.Use_Count := Container.Ptr.Use_Count + 1;
      end if;
   end Adjust;

   procedure Erase (Container : in out Unbounded_Array)
      renames Finalize;

   procedure Finalize (Container : in out Unbounded_Array) is
   begin
      Release (Container.Ptr);
   end Finalize;

   function First (Container : Unbounded_Array)
      return Index_Type is
   begin
      return Container.Ptr.Data.Vector'First;
   end First;

   function Get
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Object_Type_Ptr is
   begin
      if (  Container.Ptr = null
         or else
            Container.Ptr.Data.Vector = null
         or else
            Index not in Container.Ptr.Data.Vector'range
         )
      then
         return null;
      else
         return Container.Ptr.Data.Vector (Index);
      end if;
   end Get;

   function Last (Container : Unbounded_Array)
      return Index_Type is
   begin
      return Container.Ptr.Data.Vector'Last;
   end Last;

   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Index_Type;
                Element   : Object_Type_Ptr
             )  is
   begin
      --
      -- Check if the body is used elsewhere
      --
      if Container.Ptr = null then
         --
         -- There is no body. Create an empty one
         --
         Container.Ptr := new Unbounded_Array_Body;
         Container.Ptr.Use_Count := 1;
      else
         if Container.Ptr.Use_Count /= 1 then
            Container.Ptr.Use_Count :=
               Container.Ptr.Use_Count - 1;
            if Container.Ptr.Data.Vector /= null then
               --
               -- It is not an empty body and it is used somewhere else.
               -- So let's clone it.
               --
               declare
                  Source : Object_Ptr_Array_Type
		      renames Container.Ptr.Data.Vector.all;
               begin
                  Container.Ptr := new Unbounded_Array_Body;
                  Container.Ptr.Data.Vector :=
		         new Object_Ptr_Array_Type'(Source);
                  for Index in Source'Range loop
                     if Source (Index) /= null then
                        Increment_Count (Source (Index).all);
                     end if;
                  end loop;
               end;
            else
               Container.Ptr := new Unbounded_Array_Body;
            end if;
            Container.Ptr.Use_Count := 1;
         end if;
      end if;
      --
      -- Do the operation. Here a body is always present
      --
      declare
         Data : Object_Ptr_Array.Unbounded_Array
                   renames Container.Ptr.Data;
      begin
         if Data.Vector /= null and then Index in Data.Vector'Range then
            if Element = null then
               Release (Data.Vector (Index));
               Data.Vector (Index) := null;
            else
               if (  Data.Vector (Index) /= null
                  and then
                     Equal
                     (  Data.Vector (Index).all,
                        Element.all
                  )  )
               then
                  return;
               end if;
               Increment_Count (Element.all);
               Release (Data.Vector (Index));
               Data.Vector (Index) := Element;
            end if;
         else
            if Element /= null then
               Object_Ptr_Array.Put (Data, Index, Element);
               Increment_Count (Element.all);
            end if;
         end if;
      end;
   end Put;

   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Index_Type;
                Element   : Handle_Type
             )  is
   begin
      Put (Container, Index, Element.Ptr);
   end Put;

   function Ref
            (  Container : Unbounded_Array;
               Index     : Index_Type
            )  return Handle_Type is
      Ptr : constant Object_Type_Ptr := Get (Container, Index);
   begin
      if Ptr = null then
         raise Constraint_Error;
      else
         return Ref (Ptr);
      end if;
   end Ref;

end Object.Handle.Generic_Unbounded_Array;

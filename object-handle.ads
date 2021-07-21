--                                                                    --
--  package Object.Handle           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
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
-- The generic package Object.Handle provides the the type  Handle  used
-- to  reference  decendants of the type Entity (declared in the package
-- Object). An object is not destroyed as long as at least one handle of
-- a object exists. The object is destoyed upon finalization of the last
-- handle to it. Handles can be copied.
--
--    Object_Type     - A type derived from Object.Entity
--    Object_Type_Ptr - A class-wide access type for Object_Type'Class
--
generic
   type Object_Type (<>) is abstract new Entity with private;
   type Object_Type_Ptr is access Object_Type'Class;
package Object.Handle is
   type Handle is new Ada.Finalization.Controlled with private;
   Null_Handle : constant Handle;
--
-- Adjust -- Assignment
--
--    Reference - The handle
--
-- This procedure increases the reference count the object.
--
   procedure Adjust (Reference : in out Handle);
--
-- Finalize -- Destructor
--
--    Reference - The handle
--
-- When  the  last  handle  to  the  object  is finalized, the object is
-- destroyed.
--
   procedure Finalize (Reference : in out Handle);
--
-- Invalidate -- Detach handle from the object
--
--    Reference - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the later is destroyed.
--
   procedure Invalidate (Reference : in out Handle);
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Reference - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Reference : Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by the handle
--
--    Reference - The handle
--
-- Returns :
--
--    The referenced object or null if Reference is invalid
--
   function Ptr (Reference : Handle) return Object_Type_Ptr;
--
-- Ref -- Get handle to an object
--
--    Thing - A pointer to the object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Thing : Object_Type_Ptr) return Handle;
--
-- Set -- Handle to an object
--
--    Reference - A handle to set
--    Thing     - A pointer to the object
--
   procedure Set (Reference : in out Handle; Thing : Object_Type_Ptr);
--
-- <, <=, =, >=, > -- Comparisons
--
--    Left  - The first argument
--    Right - The second argument
--
-- Only  valid  handles  are comparable. However it is exception-safe to
-- compare Null_Handle for equality. In all other cases Constraint_Error
-- is propagated.
--
-- Returns :
--
--    The result of comparison of the objects
--
-- Exceptions :
--
--    Constraint_Error - One of arguments is Null_Handle
--
   function "<"  (Left, Right : Handle) return Boolean;
   function "<=" (Left, Right : Handle) return Boolean;
   function "="  (Left, Right : Handle) return Boolean;
   function ">=" (Left, Right : Handle) return Boolean;
   function ">"  (Left, Right : Handle) return Boolean;
--
-- = -- Equality (handle and object)
--
--    Left  - The first argument
--    Right - The second argument
--
-- Returns :
--
--    The result of comparison
--
   function "="  (Left : Handle; Right : access Object_Type'Class)
      return Boolean;
   function "="  (Left : access Object_Type'Class; Right : Handle)
      return Boolean;

private
   pragma Inline (Adjust);
   pragma Inline (Finalize);
   pragma Inline (Invalidate);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (Set);
   pragma Inline ("<=", "<", "=", ">", ">=");
   pragma Inline ("=");

   type Handle is new Ada.Finalization.Controlled with record
      Ptr : Object_Type_Ptr := null;
   end record;
--
-- Release -- Decrement object's use count
--
--    Ptr - To the object
--
-- The object pointed by Ptr is deleted if its use count in 1. Otherwise
-- the use count is decremented. Nothing happens if Ptr is null.
--
-- Exceptions :
--
--    Program_Error - Use count is already zero
--
   procedure Release (Ptr : in out Object_Type_Ptr);
   pragma Inline (Release);

   Null_Handle : constant Handle :=
                    (Ada.Finalization.Controlled with null);
end Object.Handle;

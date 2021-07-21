--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Terminated_Strings                          Winter, 2012       --
--  Interface                                                         --
--                                Last revision :  08:20 11 Jan 2015  --
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

package GNAT.Sockets.Connection_State_Machine.Terminated_Strings is
--
-- String_Data_Item -- Terminated string
--
--    Size       - Maximum string length
--    Terminator - String terminator, e.g. NUL
--
-- The actual value of the item is Item.Value (1..Item.Last)
--
   type String_Data_Item
        (  Size       : Positive;
           Terminator : Character
        )  is new Data_Item with
   record
      Last  : Natural := 0;
      Value : String (1..Size);
   end record;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_Value -- Get current value
--
--    Item - The data item
--
-- Returns :
--
--    Current value of the data item
--
   function Get_Value (Item : String_Data_Item) return String;
------------------------------------------------------------------------
--
-- Dynamic_String_Data_Item -- Terminated string
--
   type String_Ptr is access String;
   type Dynamic_String_Data_Item is new Data_Item with record
      Last       : Natural   := 0;
      Terminator : Character := Character'Val (0);
      Value      : String_Ptr;
   end record;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Dynamic_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Finalize -- Overrides Data_Item...
--
   procedure Finalize (Item : in out Dynamic_String_Data_Item);
--
-- Get_Maximum_Size -- The maximum string length without terminator
--
--    Item - The data item
--
-- Returns :
--
--    The maximum length of the string
--
   function Get_Maximum_Size
            (  Item : Dynamic_String_Data_Item
            )  return Natural;
--
-- Get_Value -- Get current value
--
--    Item - The data item
--
-- Returns :
--
--    Current value of the data item
--
   function Get_Value
            (  Item : Dynamic_String_Data_Item
            )  return String;
--
-- Set_Maximum_Size -- The maximum string length without terminator
--
--    Item - The data item
--    Size - The maximum length
--
-- This procedure expands internal buffer if necessary.
--
   procedure Set_Maximum_Size
             (  Item : in out Dynamic_String_Data_Item;
                Size : Positive
             );
-----------------------------------------------------------------------
--
-- Get -- String from stream element array
--
--    Data       - The stream element array
--    Pointer    - The first element to read
--    Terminator - The string terminator
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    End_Error    - Not enough data (no terminator)
--    Layout_Error - Pointer is outside bounds
--
   function Get
            (  Data       : Stream_Element_Array;
               Pointer    : access Stream_Element_Offset;
               Terminator : Character
            )  return String;
--
-- Put -- String into stream element array
--
--    Data       - The stream element array
--    Pointer    - The first element to write
--    Value      - The value to encode
--    Terminator - The string terminator
--
-- The parameter Pointer is advanced beyond the value output
--
-- Exceptions :
--
--    Data_Error   - The string contains the terminator
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data       : in out Stream_Element_Array;
                Pointer    : in out Stream_Element_Offset;
                Value      : String;
                Terminator : Character
             );
private
   pragma Assert (Stream_Element'Size = 8);

end GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

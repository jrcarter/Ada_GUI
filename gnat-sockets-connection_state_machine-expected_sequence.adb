--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Expected_Sequence                           Winter, 2013       --
--  Implementation                                                    --
--                                Last revision :  19:56 08 Aug 2015  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Connection_State_Machine.Expected_Sequence is

   procedure Feed
             (  Item    : in out Expected_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State := Item.Size;
      end if;
      while Pointer <= Data'Last and then State > 0 loop
         if Item.Value (Item.Size - State + 1) /= Data (Pointer) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Wrong input octet"
               &  Stream_Element'Image (Data (Pointer))
               &  ", expected"
               &  Stream_Element'Image
                  (  Item.Value (Item.Size - State + 1)
            )  )  );
         end if;
         State   := State - 1;
         Pointer := Pointer + 1;
      end loop;
   end Feed;

end GNAT.Sockets.Connection_State_Machine.Expected_Sequence;

--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Time_Conversions               Luebeck            --
--  Interface                                      Summer, 2016       --
--                                                                    --
--                                Last revision :  12:47 19 Jun 2016  --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;

with Tables.Names;

package Strings_Edit.Time_Conversions is
--
-- To_String -- Convert time to text format
--
--    Date - The time to convert
--
-- Returns :
--
--    String representation of the parameter Date in the format:
--    Sun, 17 Feb 2013 21:02:43 +0100
--
   function To_String (Date : Time) return String;
--
-- To_Time -- Time conversion
--
--    Date - To convert
--
-- Supported formats:
--
--    Fri, 31 Dec 1999 23:59:59 GMT
--    Friday, 31-Dec-99 23:59:59 GMT
--    Fri Dec 31 23:59:59 1999
--
-- Exceptions :
--
--    Time_Error - On error
--
   function To_Time (Date : String) return Time;
--
-- Check_Spelling -- Of a month, day, zone name
--
   procedure Check_Spelling (Name : String);
--
-- Check_Matched -- End of a month, day, zone name
--
   function Check_Matched
            (  Source  : String;
               Pointer : Integer
            )  return Boolean;
private
   package Day_Tables_Raw is new Tables (Day_Name);
   package Day_Tables is new Day_Tables_Raw.Names;
   use Day_Tables;

   package Month_Tables_Raw is new Tables (Month_Number);
   package Month_Tables is new Month_Tables_Raw.Names;
   use Month_Tables;

   package Zone_Tables_Raw is new Tables (Time_Zones.Time_Offset);
   package Zone_Tables is new Zone_Tables_Raw.Names;
   use Zone_Tables;

   Months    : Month_Tables.Dictionary;
   Week_Days : Day_Tables.Dictionary;
   Zones     : Zone_Tables.Dictionary;

end Strings_Edit.Time_Conversions;

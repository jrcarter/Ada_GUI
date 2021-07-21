--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Time_Conversions               Luebeck            --
--  Implementation                                 Summer, 2016       --
--                                                                    --
--                                Last revision :  13:13 14 Sep 2019  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

package body Strings_Edit.Time_Conversions is

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched
            (  Source  : String;
               Pointer : Integer
            )  return Boolean is
   begin
      case Source (Pointer) is
         when '0'..'9' | 'A'..'Z' | 'a'..'z' =>
            return False;
         when others =>
            return True;
      end case;
   end Check_Matched;

   function To_String (Date : Time) return String is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;

      function Day_Of_Week return String is
      begin
         case Day_Of_Week (Date) is
            when Monday    => return "Mon";
            when Tuesday   => return "Tue";
            when Wednesday => return "Wed";
            when Thursday  => return "Thu";
            when Friday    => return "Fri";
            when Saturday  => return "Sat";
            when Sunday    => return "Sun";
         end case;
      end Day_Of_Week;

      function Month_Name return String is
      begin
         case Month is
            when  1 => return "Jan";
            when  2 => return "Feb";
            when  3 => return "Mar";
            when  4 => return "Apr";
            when  5 => return "May";
            when  6 => return "Jun";
            when  7 => return "Jul";
            when  8 => return "Aug";
            when  9 => return "Sep";
            when 10 => return "Oct";
            when 11 => return "Nov";
            when 12 => return "Dec";
         end case;
      end Month_Name;

      function Zone return String is
      begin
         declare
            Offset  : Time_Offset := UTC_Time_Offset (Date);
            Pointer : Integer := 2;
            Text    : String (1..5);
         begin
            if Offset >= 0 then
               Text (1) := '+';
            else
               Offset   := -Offset;
               Text (1) := '-';
            end if;
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Integer (Offset / 60),
               Field       => 2,
               Justify     => Right,
               Fill        => '0'
            );
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Integer (Offset rem 60),
               Field       => 2,
               Justify     => Right,
               Fill        => '0'
            );
            return Text;
         end;
      exception
         when Unknown_Zone_Error =>
            return "GMT";
      end Zone;

   begin
      Split (Date, Year, Month, Day, Seconds);
      return
      (  Day_Of_Week
      &  ", "
      &  Image (Integer (Day))
      &  ' '
      &  Month_Name
      &  ' '
      &  Image (Integer (Year))
      &  ' '
      &  Image (Duration (Seconds))
      &  ' '
      &  Zone
      );
   end To_String;

   function To_Time (Date : String) return Time is
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Week_Day : Day_Name;
      Hour     : Hour_Number;
      Minute   : Minute_Number;
      Second   : Second_Number;
      Zone     : Time_Zones.Time_Offset;
      Got_It   : Boolean;
      Pointer  : Integer := Date'First;

      procedure Get_Year (Expand : Boolean) is
         Value : Integer;
      begin
         Get (Date, Pointer, Value);
         if Expand then
            if Value < 50 then
               Year := Year_Number (Value + 2000);
            elsif Value < 100 then
               Year := Year_Number (Value + 1900);
            end if;
         else
            Year := Year_Number (Value);
         end if;
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing year number"
            );
      end Get_Year;

      procedure Get_Day is
      begin
         Get (Date, Pointer, Integer (Day));
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing day number"
            );
      end Get_Day;

      procedure Get_Month is
      begin
         Get (Date, Pointer, Months, Month, Got_It);
         if not Got_It then
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing month name"
            );
         end if;
      end Get_Month;

      procedure Get_Time is
      begin
         begin
            Get (Date, Pointer, Integer (Hour));
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong or missing hour number"
               );
         end;
         if Date (Pointer) = ':' then
            Pointer := Pointer + 1;
         else
            Raise_Exception (Data_Error'Identity, "Colon is expected");
         end if;
         begin
            Get (Date, Pointer, Integer (Minute));
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong or missing minute number"
               );
         end;
         if Date (Pointer) = ':' then
            Pointer := Pointer + 1;
         else
            Raise_Exception (Data_Error'Identity, "Colon is expected");
         end if;
         begin
            Get (Date, Pointer, Integer (Second));
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong or missing second number"
               );
         end;
      end Get_Time;

      procedure Get_Zone is
         procedure Get_Offset is
         begin
            Zone :=
               Time_Offset
               (  Integer'(Value (Date (Pointer - 3..Pointer - 3)))
               +  Integer'(Value (Date (Pointer - 2..Pointer - 1)))
               );
            if Date (Pointer - 4) = '-' then
               Zone := -Zone;
            end if;
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong time zone offset"
               );
         end Get_Offset;
      begin
         Get (Date, Pointer, Zones, Zone, Got_It);
         if not Got_It then
            if Pointer + 4 < Date'Last then
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong time zone offset"
               );
            end if;
            if Date (Pointer) = '+' or else Date (Pointer) = '-' then
               Pointer := Pointer + 5;
               Get_Offset;
            else
               Raise_Exception
               (  Data_Error'Identity,
                  "Missing sign of time zone offset"
               );
            end if;
         end if;
      end Get_Zone;

   begin
      Get (Date, Pointer, Week_Days, Week_Day, Got_It);
      if not Got_It then
         Raise_Exception
         (  Data_Error'Identity,
            "Wrong or missing week day: " & Date (Pointer..Date'Last)
         );
      end if;
      if Date (Pointer) = ',' then
         Pointer := Pointer + 1;
      end if;
      Get (Date, Pointer);
      Get (Date, Pointer, Months, Month, Got_It); -- Dec 31 23:59:59
      if Got_It then
         Get (Date, Pointer);
         Get_Day;
      else -- No month
         Get_Day;
         if Date (Pointer) = '-' then -- 31-Dec-99 23:59:59 GMT
            Pointer := Pointer + 1;
            Get_Month;
            if Date (Pointer) = '-' then
               Pointer := Pointer + 1;
            else
               Raise_Exception
               (  Data_Error'Identity,
                  "Hyphen is expected after the month name"
               );
            end if;
            Get_Year (True);
         else                         -- 31 Dec 1999 23:59:59 GMT
            Get (Date, Pointer);
            Get_Month;
            Get (Date, Pointer);
            Get_Year (False);
         end if;
         Get (Date, Pointer);
         Get_Time;
         Get (Date, Pointer);
         Get_Zone;
      end if;
      return
         Time_Of
         (  Year      => Year,
            Month     => Month,
            Day       => Day,
            Hour      => Hour,
            Minute    => Minute,
            Second    => Second,
            Time_Zone => Zone
         );
   exception
      when Time_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Illegal date specification"
         );
   end To_Time;

begin
   Add (Week_Days, "Fri",       Friday);
   Add (Week_Days, "Friday",    Friday);
   Add (Week_Days, "Mon",       Monday);
   Add (Week_Days, "Monday",    Monday);
   Add (Week_Days, "Sat",       Saturday);
   Add (Week_Days, "Saturday",  Saturday);
   Add (Week_Days, "Sun",       Sunday);
   Add (Week_Days, "Sunday",    Sunday);
   Add (Week_Days, "Thu",       Thursday);
   Add (Week_Days, "Thursday",  Thursday);
   Add (Week_Days, "Tue",       Tuesday);
   Add (Week_Days, "Tuesday",   Tuesday);
   Add (Week_Days, "Wed",       Wednesday);
   Add (Week_Days, "Wednesday", Wednesday);

   Add (Months, "Apr",       4);
   Add (Months, "April",     4);
   Add (Months, "Aug",       8);
   Add (Months, "August",    8);
   Add (Months, "Dec",      12);
   Add (Months, "December", 12);
   Add (Months, "Feb",       2);
   Add (Months, "February",  2);
   Add (Months, "Jan",       1);
   Add (Months, "January",   1);
   Add (Months, "Jul",       7);
   Add (Months, "July",      7);
   Add (Months, "Jun",       6);
   Add (Months, "June",      6);
   Add (Months, "Mar",       3);
   Add (Months, "March",     3);
   Add (Months, "May",       5);
   Add (Months, "Nov",      11);
   Add (Months, "November", 11);
   Add (Months, "Oct",      10);
   Add (Months, "October",  10);
   Add (Months, "Sep",       9);
   Add (Months, "September", 9);

   Add (Zones, "UT",   0);
   Add (Zones, "GMT",  0);
   Add (Zones, "EDT", -4*60);
   Add (Zones, "EST", -5*60);
   Add (Zones, "CDT", -5*60);
   Add (Zones, "CST", -6*60);
   Add (Zones, "MDT", -6*60);
   Add (Zones, "MST", -7*60);
   Add (Zones, "PDT", -7*60);
   Add (Zones, "PST", -8*60);
   Add (Zones, "Z",    0);
   Add (Zones, "A",   -1*60);
   Add (Zones, "M",  -12*60);
   Add (Zones, "N",    1*60);
   Add (Zones, "Y",   12*60);

end Strings_Edit.Time_Conversions;

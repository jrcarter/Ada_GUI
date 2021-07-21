-- Ada_GUI version of Random_Int
--
-- Copyright (C) 2021 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause
--
-- The (application-specific) user interface

package Random_Int.UI is
   type Event_ID is (Generate, Quit);

   function Ended return Boolean;
   -- Returns True when Next_Event has returned Quit; False otherwise

   function Next_Event return Event_ID with Pre => not Ended;
   -- Blocks until the next event ia available and returns it
   -- If the result is Quit, further calls to operations of this package will raise Program_Error

   function Min_Text return String with Pre => not Ended;
   function Max_Text return String with Pre => not Ended;
   -- Returns the text in the Min and Max input fields

   procedure Set_Min (Value : in Integer) with Pre => not Ended;
   procedure Set_Max (Value : in Integer) with Pre => not Ended;
   -- Sets the text in the Min and Max input fields to Value

   procedure Min_Error with Pre => not Ended;
   procedure Max_Error with Pre => not Ended;
   -- Displays an error message in the Min and Max input fields

   procedure Show_Result (Value : in Integer) with Pre => not Ended;
   -- Puts a value in the result display field
end Random_Int.UI;

-- An Ada-oriented GUI library
-- Implementation derived from Gnoga
--
-- Copyright (C) 2023 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Numerics;

separate (Ada_GUI)
package body Plotting is
   function New_Plot (ID : in Widget_ID; X_Min : in Float; X_Max : in Float; Y_Min : in Float; Y_Max : in Float)
   return Plot_Info is
      Widget : constant Widget_Info := Widget_List (ID.Value);

      Result : Plot_Info;
   begin -- New_Plot
      Result.ID := ID;
      Result.X_Min := X_Min;
      Result.X_Max := X_Max;
      Result.Y_Min := Y_Min;
      Result.Y_Max := Y_Max;
      Result.X_Scale := Float (Widget.Width) / (X_Max - X_Min);
      Result.Y_Scale := Float (Widget.Height) / (Y_Max - Y_Min);
      ID.Draw_Rectangle (From_X => 0, From_Y => 0, To_X => Widget.Width - 1, To_Y => Widget.Height - 1);

      return Result;
   end New_Plot;

   function Scale_X (Plot : Plot_Info; X : Float) return Integer is (Integer (Plot.X_Scale * (X - Plot.X_Min) ) );

   function Scale_Y (Plot : Plot_Info; Y : Float) return Integer is
      Widget : constant Widget_Info := Widget_List (Plot.ID.Value);
   begin -- Scale_Y
      return Widget.Height - Integer (Plot.Y_Scale * (Y - Plot.Y_Min) );
   end Scale_Y;

   procedure Draw_Point (Plot : in Plot_Info; X : in Float; Y : in Float; Color : in Color_Info := To_Color (Black) ) is
      -- Empty
   begin -- Draw_Point
      Plot.ID.Draw_Arc (X          => Scale_X (Plot, X),
                        Y          => Scale_Y (Plot, Y),
                        Radius     => 2,
                        Start      => 0.0,
                        Stop       => 2.0 * Ada.Numerics.Pi,
                        Line_Color => (None => True),
                        Fill_Color => (None => False, Color => Color) );
   end Draw_Point;

   procedure Draw_Line (Plot   : in Plot_Info;
                        From_X : in Float;
                        From_Y : in Float;
                        To_X   : in Float;
                        To_Y   : in Float;
                        Color  : in Color_Info := To_Color (Black) )
   is
      -- Empty
   begin -- Draw_Line
      Plot.ID.Draw_Line (From_X => Scale_X (Plot, From_X),
                         From_Y => Scale_Y (Plot, From_Y),
                         To_X   => Scale_X (Plot, To_X),
                         To_Y   => Scale_Y (Plot, To_Y),
                         Color  => Color);
   end Draw_Line;

   procedure Draw_X_Axis
      (Plot : in Plot_Info; Interval : in Positive_Float; Length : in Positive; Color : in Color_Info := To_Color (Black) )
   is
      procedure Label_Tick (X : in Integer; Value : in Float);
      -- If Value is an integer, draws its image next to its tick at horizontal pixel X

      Widget : constant Widget_Info := Widget_List (Plot.ID.Value);
      Y      : constant Integer     := Scale_Y (Plot, 0.0);

      procedure Label_Tick (X : in Integer; Value : in Float) is
         -- Empty
      begin -- Label_Tick
         if abs (Float (Integer (Value) ) - Value) > 0.001 then -- Not an integer
            return;
         end if;

         Plot.ID.Draw_Text (X          => X,
                            Y          => (if Y <= Widget.Height / 2 then Y + Length + 2 else Y - Length - 2),
                            Text       => Gnoga.Left_Trim (Integer'Image (Integer (Value) ) ),
                            Fill_Color => (None => False, Color => Color) );
      end Label_Tick;

      X     : Integer;
      Point : Float;
   begin -- Draw_X_Axis
      if Y not in 0 .. Widget.Height - 1 then -- Axis not visible
         return;
      end if;

      Draw_Line (Plot => Plot, From_X => Plot.X_Min, From_Y => 0.0, To_X => Plot.X_Max, To_Y => 0.0, Color => Color);
      Point := Interval;

      Plus : loop
         exit Plus when Point > Plot.X_Max;

         X := Scale_X (Plot, Point);
         Plot.ID.Draw_Line (From_X => X, From_Y => Y - Length, To_X => X, To_Y => Y + Length, Color => Color);
         Label_Tick (X => X, Value => Point);
         Point := Point + Interval;
      end loop Plus;

      Point := -Interval;

      Minus : loop
         exit Minus when Point < Plot.X_Min;

         X := Scale_X (Plot, Point);
         Plot.ID.Draw_Line (From_X => X, From_Y => Y - Length, To_X => X, To_Y => Y + Length, Color => Color);
         Label_Tick (X => X, Value => Point);
         Point := Point - Interval;
      end loop Minus;
   end Draw_X_Axis;

   procedure Draw_Y_Axis
      (Plot : in Plot_Info; Interval : in Positive_Float; Length : in Positive; Color : in Color_Info := To_Color (Black) )
   is
      procedure Label_Tick (Y : in Integer; Value : in Float);
      -- If Value is an integer, draws its image next to its tick at vertical pixel Y

      Widget : constant Widget_Info := Widget_List (Plot.ID.Value);
      X      : constant Integer     := Scale_X (Plot, 0.0);

      procedure Label_Tick (Y : in Integer; Value : in Float) is
         -- Empty
      begin -- Label_Tick
         if abs (Float (Integer (Value) ) - Value) > 0.001 then -- Not an integer
            return;
         end if;

         Plot.ID.Draw_Text (X          => (if X <= Widget.Width / 2 then X + Length +2 else X - Length - 2),
                            Y          => Y,
                            Text       => Gnoga.Left_Trim (Integer'Image (Integer (Value) ) ),
                            Fill_Color => (None => False, Color => Color) );
      end Label_Tick;

      Y     : Integer;
      Point : Float;
   begin -- Draw_Y_Axis
      if X not in 0 .. Widget.Width - 1 then -- Axis not visible
         return;
      end if;

      Draw_Line (Plot => Plot, From_X => 0.0, From_Y => Plot.Y_Min, To_X => 0.0, To_Y => Plot.Y_Max, Color => Color);
      Point := Interval;

      Plus : loop
         exit Plus when Point > Plot.Y_Max;

         Y := Scale_Y (Plot, Point);
         Plot.ID.Draw_Line (From_X => X - Length, From_Y => Y, To_X => X + Length, To_Y => Y, Color => Color);
         Label_Tick (Y => Y, Value => Point);
         Point := Point + Interval;
      end loop Plus;

      Point := -Interval;

      Minus : loop
         exit Minus when Point < Plot.Y_Min;

         Y := Scale_Y (Plot, Point);
         Plot.ID.Draw_Line (From_X => X - Length, From_Y => Y, To_X => X + Length, To_Y => Y, Color => Color);
         Label_Tick (Y => Y, Value => Point);
         Point := Point - Interval;
      end loop Minus;
   end Draw_Y_Axis;

   procedure Draw_Axes
      (Plot : in Plot_Info; Interval : in Positive_Float; Length : in Positive; Color : in Color_Info := To_Color (Black) )
   is
      -- Empty
   begin -- Draw_Axes
      Draw_X_Axis (Plot => Plot, Interval => Interval, Length => Length, Color => Color);
      Draw_Y_Axis (Plot => Plot, Interval => Interval, Length => Length, Color => Color);
   end Draw_Axes;
end Plotting;

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
      Widget : constant Widget_Info := Widget_List.Element (ID.Value);

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
      Widget : constant Widget_Info := Widget_List.Element (Plot.ID.Value);
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
                        Color  : in Color_Info    := To_Color (Black);
                        Style  : in Line_Style_ID := Normal)
   is
      -- Empty
   begin -- Draw_Line
      Plot.ID.Draw_Line (From_X => Scale_X (Plot, From_X),
                         From_Y => Scale_Y (Plot, From_Y),
                         To_X   => Scale_X (Plot, To_X),
                         To_Y   => Scale_Y (Plot, To_Y),
                         Color  => Color,
                         Style  => Style);
   end Draw_Line;

   function Width (Plot : in Plot_Info; Text : in String) return Natural with Pre => Plot.ID.Kind = Graphic_Area;
   -- Returns the width of Text in pixels if drawn in Plot
   -- Doesn't seem to be very accurate

   procedure Draw_X_Axis (Plot     : in Plot_Info;
                          Interval : in Positive_Float;
                          Length   : in Positive;
                          Label    : in String     := "";
                          Color    : in Color_Info := To_Color (Black) )
   is
      procedure Label_Axis;
      -- Draws Label near the center of the axis

      procedure Label_Tick (X : in Integer; Value : in Float);
      -- If Value is an integer, draws its image next to its tick at horizontal pixel X

      Widget : constant Widget_Info := Widget_List.Element (Plot.ID.Value);
      Axis_Y : constant Float       := (if 0.0 < Plot.Y_Min then Plot.Y_Min elsif 0.0 > Plot.Y_Max then Plot.Y_Max else 0.0);
      Y      : constant Integer     := Scale_Y (Plot, Axis_Y);

      procedure Label_Axis is
         X : constant Integer := (Widget.Width - Width (Plot, Label) ) / 2;
      begin -- Label_Axis
         if Y <= Widget.Height / 2 then -- Axis in upper half of plot with tick labels below
            Plot.ID.Draw_Text (X          => X,
                               Y          => (if Y >= 22 + Length then Y - Length - 22 else Y + Length + 42),
                               Text       => Label,
                               Fill_Color => (None => False, Color => Color) );
         else -- Axis in lower half with tick labels above
            Plot.ID.Draw_Text (X          => X,
                               Y          => (if Y <= Widget.Height - 22 - Length then Y + Length + 22 else Y - Length - 42),
                               Text       => Label,
                               Fill_Color => (None => False, Color => Color) );
         end if;
      end Label_Axis;

      procedure Label_Tick (X : in Integer; Value : in Float) is
         -- Empty
      begin -- Label_Tick
         if abs (Float (Integer (Value) ) - Value) > 0.001 then -- Not an integer
            return;
         end if;

         Plot.ID.Draw_Text (X          => X,
                            Y          => (if Y <= Widget.Height / 2 then Y + Length + 22 else Y - Length - 2),
                            Text       => Gnoga.Left_Trim (Integer'Image (Integer (Value) ) ),
                            Fill_Color => (None => False, Color => Color) );
      end Label_Tick;

      X     : Integer;
      Point : Float;
   begin -- Draw_X_Axis
      Plot.Draw_Line (From_X => Plot.X_Min, From_Y => Axis_Y, To_X => Plot.X_Max, To_Y => Axis_Y, Color => Color);
      Label_Axis;
      Point := Interval;

      Plus : loop
         exit Plus when Point > Plot.X_Max;

         X := Plot.Scale_X (Point);
         Plot.ID.Draw_Line (From_X => X, From_Y => Y - Length, To_X => X, To_Y => Y + Length, Color => Color);
         Label_Tick (X => X, Value => Point);
         Point := Point + Interval;
      end loop Plus;

      Point := -Interval;

      Minus : loop
         exit Minus when Point < Plot.X_Min;

         X := Plot.Scale_X (Point);
         Plot.ID.Draw_Line (From_X => X, From_Y => Y - Length, To_X => X, To_Y => Y + Length, Color => Color);
         Label_Tick (X => X, Value => Point);
         Point := Point - Interval;
      end loop Minus;
   end Draw_X_Axis;

   procedure Draw_Y_Axis (Plot     : in Plot_Info;
                          Interval : in Positive_Float;
                          Length   : in Positive;
                          Label    : in String     := "";
                          Color    : in Color_Info := To_Color (Black) )
   is
      procedure Label_Axis;
      -- Draws Label vertically near the center of the axis

      procedure Label_Tick (Y : in Integer; Value : in Float);
      -- If Value is an integer, draws its image next to its tick at vertical pixel Y

      Widget : constant Widget_Info := Widget_List.Element (Plot.ID.Value);
      Axis_X : constant Float       := (if 0.0 < Plot.X_Min then Plot.X_Min elsif 0.0 > Plot.X_Max then Plot.X_Max else 0.0);
      X      : constant Integer     := Scale_X (Plot, Axis_X);

      Max_Label : Integer := -1; -- Max width of a tick label

      procedure Label_Axis is
         procedure Draw_Label (X : in Integer);
         -- Draws Label vertically at X

         Y : constant Integer := Widget.Height / 2 - 11 * Label'Length + 20;

         procedure Draw_Label (X : in Integer) is
            Next_Y : Integer := Y;
         begin -- Draw_Label
            All_Chars : for C of Label loop
               Plot.ID.Draw_Text (X => X, Y => Next_Y, Text => C & "", Fill_Color => (None => False, Color => Color) );
               Next_Y := Next_Y + 22;
            end loop All_Chars;
         end Draw_Label;
      begin -- Label_Axis
         if X <= Widget.Width / 2 then -- Axis in left half of plot with labels to the right
            Draw_Label (X => (if X >= 22 + Length then X - Length - 22 else X + Length + Max_Label + 2) );
         else
            Draw_Label (X => (if X <= Widget.Width - Length - 22 then X + Length + 2 else X - Length - Max_Label - 2) );
         end if;
      end Label_Axis;

      procedure Label_Tick (Y : in Integer; Value : in Float) is
         Image : constant String  := Gnoga.Left_Trim (Integer'Image (Integer (Value) ) );
         Size  : constant Natural := Width (Plot, Image);
      begin -- Label_Tick
         if abs (Float (Integer (Value) ) - Value) > 0.001 then -- Not an integer
            return;
         end if;

         Plot.ID.Draw_Text (X          => (if X <= Widget.Width / 2 then X + Length + 2 else X - Length - Size - 2),
                            Y          => Y,
                            Text       => Image,
                            Fill_Color => (None => False, Color => Color) );
         Max_Label := Integer'Max (Size, Max_Label);
      end Label_Tick;

      Y     : Integer;
      Point : Float;
   begin -- Draw_Y_Axis
      Plot.Draw_Line (From_X => Axis_X, From_Y => Plot.Y_Min, To_X => Axis_X, To_Y => Plot.Y_Max, Color => Color);
      Point := Interval;

      Plus : loop
         exit Plus when Point > Plot.Y_Max;

         Y := Plot.Scale_Y (Point);
         Plot.ID.Draw_Line (From_X => X - Length, From_Y => Y, To_X => X + Length, To_Y => Y, Color => Color);
         Label_Tick (Y => Y, Value => Point);
         Point := Point + Interval;
      end loop Plus;

      Y := Plot.Scale_Y (0.0);
      Plot.ID.Draw_Line (From_X => X - Length, From_Y => Y, To_X => X + Length, To_Y => Y, Color => Color);
      Label_Tick (Y => Y, Value => 0.0);
      Point := -Interval;

      Minus : loop
         exit Minus when Point < Plot.Y_Min;

         Y := Plot.Scale_Y (Point);
         Plot.ID.Draw_Line (From_X => X - Length, From_Y => Y, To_X => X + Length, To_Y => Y, Color => Color);
         Label_Tick (Y => Y, Value => Point);
         Point := Point - Interval;
      end loop Minus;

      Label_Axis;
   end Draw_Y_Axis;

   procedure Draw_Axes
      (Plot : in Plot_Info; Interval : in Positive_Float; Length : in Positive; Color : in Color_Info := To_Color (Black) )
   is
      -- Empty
   begin -- Draw_Axes
      Draw_X_Axis (Plot => Plot, Interval => Interval, Length => Length, Color => Color);
      Draw_Y_Axis (Plot => Plot, Interval => Interval, Length => Length, Color => Color);
   end Draw_Axes;

   function Width (Plot : in Plot_Info; Text : in String) return Natural is
      Widget : constant Widget_Info := Widget_List.Element (Plot.ID.Value);
   begin -- Width
      return Integer (Float'Ceiling (Widget.Context.Measure_Text_Width (Text) ) );
   end Width;
end Plotting;

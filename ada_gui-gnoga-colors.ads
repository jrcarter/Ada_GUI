-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . T Y P E S . C O L O R S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2015 Pascal Pignard                    --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

package Ada_GUI.Gnoga.Colors is
   type Color_Enumeration is
     (Alice_Blue,
      Antique_White,
      Aqua,
      Aquamarine,
      Azure,
      Beige,
      Bisque,
      Black,
      Blanched_Almond,
      Blue,
      Blue_Violet,
      Brown,
      Burly_Wood,
      Cadet_Blue,
      Chartreuse,
      Chocolate,
      Coral,
      Cornflower_Blue,
      Cornsilk,
      Crimson,
      Cyan,
      Dark_Blue,
      Dark_Cyan,
      Dark_Golden_Rod,
      Dark_Gray,
      Dark_Green,
      Dark_Grey,
      Dark_Khaki,
      Dark_Magenta,
      Dark_Olive_Green,
      Dark_Orange,
      Dark_Orchid,
      Dark_Red,
      Dark_Salmon,
      Dark_Sea_Green,
      Dark_Slate_Blue,
      Dark_Slate_Gray,
      Dark_Slate_Grey,
      Dark_Turquoise,
      Dark_Violet,
      DeepPink,
      Deep_Sky_Blue,
      Dim_Gray,
      Dim_Grey,
      Dodger_Blue,
      Fire_Brick,
      Floral_White,
      Forest_Green,
      Fuchsia,
      Gainsboro,
      Ghost_White,
      Gold_Deep_Sky_Blue,
      Golden_Rod,
      Gray,
      Green,
      Green_Yellow,
      Grey,
      Honey_Dew,
      Hot_Pink,
      Indian_Red,
      Indigo,
      Ivory,
      Khaki,
      Lavender,
      Lavender_Blush,
      Lawn_Green,
      Lemon_Chiffon,
      Light_Blue,
      Light_Coral,
      Light_Cyan,
      Light_Golden_Rod_Yellow,
      Light_Gray,
      Light_Green,
      Light_Grey,
      Light_Pink,
      Light_Salmon,
      Light_Sea_Green,
      Light_Sky_Blue,
      Light_Slate_Gray,
      Light_Slate_Grey,
      Light_Steel_Blue,
      Light_Yellow,
      Lime,
      Lime_Green,
      Linen,
      Magenta,
      Maroon,
      Medium_Aqua_Marine,
      Medium_Blue,
      Medium_Orchid,
      Medium_Purple,
      Medium_Sea_Green,
      Medium_Slate_Blue,
      Medium_Spring_Green,
      Medium_Turquoise,
      Medium_Violet_Red,
      Midnight_Blue,
      Mint_Cream,
      Misty_Rose,
      Moccasin,
      Navajo_White,
      Navy,
      Old_Lace,
      Olive,
      Olive_Drab,
      Orange,
      Orange_Red,
      Orchid,
      Pale_Golden_Rod,
      Pale_Green,
      Pale_Turquoise,
      Pale_Violet_Red,
      Papaya_Whip,
      Peach_Puff,
      Peru,
      Pink,
      Plum,
      Powder_Blue,
      Purple,
      Red,
      Rosy_Brown,
      Royal_Blue,
      Saddle_Brown,
      Salmon,
      Sandy_Brown,
      Sea_Green,
      Sea_Shell,
      Sienna,
      Silver,
      Sky_Blue,
      Slate_Blue,
      Slate_Gray,
      Slate_Grey,
      Snow,
      Spring_Green,
      Steel_Blue,
      Tan,
      Teal,
      Thistle,
      Tomato,
      Turquoise,
      Violet,
      Wheat,
      White,
      White_Smoke,
      Yellow,
      Yellow_Green);

   Color_Error : exception;

   function To_String (Value : Color_Enumeration) return String;
   --  Returns color name

   function To_RGBA (Value : Color_Enumeration) return RGBA_Type;
   --  Returns color RGBA_Type

   function To_Color_Enumeration (Value : RGBA_Type) return Color_Enumeration;
   --  Returns Color_Enumeration if it exists else raises Color_Error exception

   function To_Color_Enumeration (Value : String) return Color_Enumeration;
   --  Returns Color_Enumeration if it exists else raises Color_Error exception

end Ada_GUI.Gnoga.Colors;

-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . T Y P E S . C O L O R S                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Exceptions;

package body Ada_GUI.Gnoga.Colors is

   --  Based on CSS extended color keywords
   --  http://dev.w3.org/csswg/css-color-3/ §4.3

   type CSS_Color_Enumeration is
     (aliceblue,
      antiquewhite,
      aqua,
      aquamarine,
      azure,
      beige,
      bisque,
      black,
      blanchedalmond,
      blue,
      blueviolet,
      brown,
      burlywood,
      cadetblue,
      chartreuse,
      chocolate,
      coral,
      cornflowerblue,
      cornsilk,
      crimson,
      cyan,
      darkblue,
      darkcyan,
      darkgoldenrod,
      darkgray,
      darkgreen,
      darkgrey,
      darkkhaki,
      darkmagenta,
      darkolivegreen,
      darkorange,
      darkorchid,
      darkred,
      darksalmon,
      darkseagreen,
      darkslateblue,
      darkslategray,
      darkslategrey,
      darkturquoise,
      darkviolet,
      deeppink,
      deepskyblue,
      dimgray,
      dimgrey,
      dodgerblue,
      firebrick,
      floralwhite,
      forestgreen,
      fuchsia,
      gainsboro,
      ghostwhite,
      gold,
      goldenrod,
      gray,
      green,
      greenyellow,
      grey,
      honeydew,
      hotpink,
      indianred,
      indigo,
      ivory,
      khaki,
      lavender,
      lavenderblush,
      lawngreen,
      lemonchiffon,
      lightblue,
      lightcoral,
      lightcyan,
      lightgoldenrodyellow,
      lightgray,
      lightgreen,
      lightgrey,
      lightpink,
      lightsalmon,
      lightseagreen,
      lightskyblue,
      lightslategray,
      lightslategrey,
      lightsteelblue,
      lightyellow,
      lime,
      limegreen,
      linen,
      magenta,
      maroon,
      mediumaquamarine,
      mediumblue,
      mediumorchid,
      mediumpurple,
      mediumseagreen,
      mediumslateblue,
      mediumspringgreen,
      mediumturquoise,
      mediumvioletred,
      midnightblue,
      mintcream,
      mistyrose,
      moccasin,
      navajowhite,
      navy,
      oldlace,
      olive,
      olivedrab,
      orange,
      orangered,
      orchid,
      palegoldenrod,
      palegreen,
      paleturquoise,
      palevioletred,
      papayawhip,
      peachpuff,
      peru,
      pink,
      plum,
      powderblue,
      purple,
      red,
      rosybrown,
      royalblue,
      saddlebrown,
      salmon,
      sandybrown,
      seagreen,
      seashell,
      sienna,
      silver,
      skyblue,
      slateblue,
      slategray,
      slategrey,
      snow,
      springgreen,
      steelblue,
      tan,
      teal,
      thistle,
      tomato,
      turquoise,
      violet,
      wheat,
      white,
      whitesmoke,
      yellow,
      yellowgreen);

   type Color_Array_Type is array (Color_Enumeration) of Gnoga.RGBA_Type;

   RGBA_Colors : constant Color_Array_Type :=
     ((240, 248, 255, 1.0),           --  Alice_Blue
      (250, 235, 215, 1.0),           --  Antique_White
      (0, 255, 255, 1.0),             --  Aqua
      (127, 255, 212, 1.0),           --  Aquamarine
      (240, 255, 255, 1.0),           --  Azure
      (245, 245, 220, 1.0),           --  Beige
      (255, 228, 196, 1.0),           --  Bisque
      (0, 0, 0, 1.0),                 --  Black
      (255, 235, 205, 1.0),           --  Blanched_Almond
      (0, 0, 255, 1.0),               --  Blue
      (138, 43, 226, 1.0),            --  Blue_Violet
      (165, 42, 42, 1.0),             --  Brown
      (222, 184, 135, 1.0),           --  Burly_Wood
      (95, 158, 160, 1.0),            --  Cadet_Blue
      (127, 255, 0, 1.0),             --  Chartreuse
      (210, 105, 30, 1.0),            --  Chocolate
      (255, 127, 80, 1.0),            --  Coral
      (100, 149, 237, 1.0),           --  Cornflower_Blue
      (255, 248, 220, 1.0),           --  Cornsilk
      (220, 20, 60, 1.0),             --  Crimson
      (0, 255, 255, 1.0),             --  Cyan
      (0, 0, 139, 1.0),               --  Dark_Blue
      (0, 139, 139, 1.0),             --  Dark_Cyan
      (184, 134, 11, 1.0),            --  Dark_Golden_Rod
      (169, 169, 169, 1.0),           --  Dark_Gray
      (0, 100, 0, 1.0),               --  Dark_Green
      (169, 169, 169, 1.0),           --  Dark_Grey
      (189, 183, 107, 1.0),           --  Dark_Khaki
      (139, 0, 139, 1.0),             --  Dark_Magenta
      (85, 107, 47, 1.0),             --  Dark_Olive_Green
      (255, 140, 0, 1.0),             --  Dark_Orange
      (153, 50, 204, 1.0),            --  Dark_Orchid
      (139, 0, 0, 1.0),               --  Dark_Red
      (233, 150, 122, 1.0),           --  Dark_Salmon
      (143, 188, 143, 1.0),           --  Dark_Sea_Green
      (72, 61, 139, 1.0),             --  Dark_Slate_Blue
      (47, 79, 79, 1.0),              --  Dark_Slate_Gray
      (47, 79, 79, 1.0),              --  Dark_Slate_Grey
      (0, 206, 209, 1.0),             --  Dark_Turquoise
      (148, 0, 211, 1.0),             --  Dark_Violet
      (255, 20, 147, 1.0),            --  DeepPink
      (0, 191, 255, 1.0),             --  Deep_Sky_Blue
      (105, 105, 105, 1.0),           --  Dim_Gray
      (105, 105, 105, 1.0),           --  Dim_Grey
      (30, 144, 255, 1.0),            --  Dodger_Blue
      (178, 34, 34, 1.0),             --  Fire_Brick
      (255, 250, 240, 1.0),           --  Floral_White
      (34, 139, 34, 1.0),             --  Forest_Green
      (255, 0, 255, 1.0),             --  Fuchsia
      (220, 220, 220, 1.0),           --  Gainsboro
      (248, 248, 255, 1.0),           --  Ghost_White
      (255, 215, 0, 1.0),             --  Gold_Deep_Sky_Blue
      (218, 165, 32, 1.0),            --  Golden_Rod
      (128, 128, 128, 1.0),           --  Gray
      (0, 128, 0, 1.0),               --  Green
      (173, 255, 47, 1.0),            --  Green_Yellow
      (128, 128, 128, 1.0),           --  Grey
      (240, 255, 240, 1.0),           --  Honey_Dew
      (255, 105, 180, 1.0),           --  Hot_Pink
      (205, 92, 92, 1.0),             --  Indian_Red
      (75, 0, 130, 1.0),              --  Indigo
      (255, 255, 240, 1.0),           --  Ivory
      (240, 230, 140, 1.0),           --  Khaki
      (230, 230, 250, 1.0),           --  Lavender
      (255, 240, 245, 1.0),           --  Lavender_Blush
      (124, 252, 0, 1.0),             --  Lawn_Green
      (255, 250, 205, 1.0),           --  Lemon_Chiffon
      (173, 216, 230, 1.0),           --  Light_Blue
      (240, 128, 128, 1.0),           --  Light_Coral
      (224, 255, 255, 1.0),           --  Light_Cyan
      (250, 250, 210, 1.0),           --  Light_Golden_Rod_Yellow
      (211, 211, 211, 1.0),           --  Light_Gray
      (144, 238, 144, 1.0),           --  Light_Green
      (211, 211, 211, 1.0),           --  Light_Grey
      (255, 182, 193, 1.0),           --  Light_Pink
      (255, 160, 122, 1.0),           --  Light_Salmon
      (32, 178, 170, 1.0),            --  Light_Sea_Green
      (135, 206, 250, 1.0),           --  Light_Sky_Blue
      (119, 136, 153, 1.0),           --  Light_Slate_Gray
      (119, 136, 153, 1.0),           --  Light_Slate_Grey
      (176, 196, 222, 1.0),           --  Light_Steel_Blue
      (255, 255, 224, 1.0),           --  Light_Yellow
      (0, 255, 0, 1.0),               --  Lime
      (50, 205, 50, 1.0),             --  Lime_Green
      (250, 240, 230, 1.0),           --  Linen
      (255, 0, 255, 1.0),             --  Magenta
      (128, 0, 0, 1.0),               --  Maroon
      (102, 205, 170, 1.0),           --  Medium_Aqua_Marine
      (0, 0, 205, 1.0),               --  Medium_Blue
      (186, 85, 211, 1.0),            --  Medium_Orchid
      (147, 112, 219, 1.0),           --  Medium_Purple
      (60, 179, 113, 1.0),            --  Medium_Sea_Green
      (123, 104, 238, 1.0),           --  Medium_Slate_Blue
      (0, 250, 154, 1.0),             --  Medium_Spring_Green
      (72, 209, 204, 1.0),            --  Medium_Turquoise
      (199, 21, 133, 1.0),            --  Medium_Violet_Red
      (25, 25, 112, 1.0),             --  Midnight_Blue
      (245, 255, 250, 1.0),           --  Mint_Cream
      (255, 228, 225, 1.0),           --  Misty_Rose
      (255, 228, 181, 1.0),           --  Moccasin
      (255, 222, 173, 1.0),           --  Navajo_White
      (0, 0, 128, 1.0),               --  Navy
      (253, 245, 230, 1.0),           --  Old_Lace
      (128, 128, 0, 1.0),             --  Olive
      (107, 142, 35, 1.0),            --  Olive_Drab
      (255, 165, 0, 1.0),             --  Orange
      (255, 69, 0, 1.0),              --  Orange_Red
      (218, 112, 214, 1.0),           --  Orchid
      (238, 232, 170, 1.0),           --  Pale_Golden_Rod
      (152, 251, 152, 1.0),           --  Pale_Green
      (175, 238, 238, 1.0),           --  Pale_Turquoise
      (219, 112, 147, 1.0),           --  Pale_Violet_Red
      (255, 239, 213, 1.0),           --  Papaya_Whip
      (255, 218, 185, 1.0),           --  Peach_Puff
      (205, 133, 63, 1.0),            --  Peru
      (255, 192, 203, 1.0),           --  Pink
      (221, 160, 221, 1.0),           --  Plum
      (176, 224, 230, 1.0),           --  Powder_Blue
      (128, 0, 128, 1.0),             --  Purple
      (255, 0, 0, 1.0),               --  Red
      (188, 143, 143, 1.0),           --  Rosy_Brown
      (65, 105, 225, 1.0),            --  Royal_Blue
      (139, 69, 19, 1.0),             --  Saddle_Brown
      (250, 128, 114, 1.0),           --  Salmon
      (244, 164, 96, 1.0),            --  Sandy_Brown
      (46, 139, 87, 1.0),             --  Sea_Green
      (255, 245, 238, 1.0),           --  Sea_Shell
      (160, 82, 45, 1.0),             --  Sienna
      (192, 192, 192, 1.0),           --  Silver
      (135, 206, 235, 1.0),           --  Sky_Blue
      (106, 90, 205, 1.0),            --  Slate_Blue
      (112, 128, 144, 1.0),           --  Slate_Gray
      (112, 128, 144, 1.0),           --  Slate_Grey
      (255, 250, 250, 1.0),           --  Snow
      (0, 255, 127, 1.0),             --  Spring_Green
      (70, 130, 180, 1.0),            --  Steel_Blue
      (210, 180, 140, 1.0),           --  Tan
      (0, 128, 128, 1.0),             --  Teal
      (216, 191, 216, 1.0),           --  Thistle
      (255, 99, 71, 1.0),             --  Tomato
      (64, 224, 208, 1.0),            --  Turquoise
      (238, 130, 238, 1.0),           --  Violet
      (245, 222, 179, 1.0),           --  Wheat
      (255, 255, 255, 1.0),           --  White
      (245, 245, 245, 1.0),           --  White_Smoke
      (255, 255, 0, 1.0),             --  Yellow
      (154, 205, 50, 1.0));           --  Yellow_Green

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Color_Enumeration) return String is
   begin
      return CSS_Color_Enumeration'Image
          (CSS_Color_Enumeration'Val (Color_Enumeration'Pos (Value)));
   end To_String;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA (Value : Color_Enumeration) return Gnoga.RGBA_Type is
   begin
      return RGBA_Colors (Value);
   end To_RGBA;

   --------------------------
   -- To_Color_Enumeration --
   --------------------------

   function To_Color_Enumeration
     (Value : Gnoga.RGBA_Type) return Color_Enumeration
   is
   begin
      for C in RGBA_Colors'Range loop
         if Value = RGBA_Colors (C) then
            return C;
         end if;
      end loop;
      raise Color_Error;
   end To_Color_Enumeration;

   --------------------------
   -- To_Color_Enumeration --
   --------------------------

   function To_Color_Enumeration (Value : String) return Color_Enumeration is
   begin
      return Color_Enumeration'Val (CSS_Color_Enumeration'Pos
                                    (CSS_Color_Enumeration'Value (Value)));
   exception
      when E : Constraint_Error =>
         Log ("Error converting to Color_Enumeration from " & Value);
         Log (Ada.Exceptions.Exception_Information (E));
         raise Color_Error;
   end To_Color_Enumeration;

end Ada_GUI.Gnoga.Colors;

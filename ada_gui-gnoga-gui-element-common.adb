-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . G U I . E L E M E N T . C O M M O N            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
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

package body Ada_GUI.Gnoga.Gui.Element.Common is

   -------------------------------------------------------------------------
   --  A_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (A       : in out A_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      Link    : in     String := "";
      Content : in     String := "";
      Target  : in     String := "_self";
      ID      : in     String := "")
   is
   begin
      A.Create_From_HTML (Parent, Escape_Quotes ("<a target='" & Target &
                            "' href='" & Escape_Inner_Quotes (Link) & "'>" &
                            Content & "</a>"), ID);
   end Create;

   ----------
   -- Link --
   ----------

   procedure Link (A : in out A_Type; Value : String) is
   begin
      A.Property ("href", Value);
   end Link;

   function Link (A : A_Type) return String is
   begin
      return A.Property ("href");
   end Link;

   ------------
   -- Target --
   ------------

   procedure Target (A : in out A_Type; Value : String) is
   begin
      A.Attribute ("target", Value);
   end Target;

   function Target (A : A_Type) return String is
   begin
      return A.Attribute ("target");
   end Target;

   -------------------------------------------------------------------------
   --  Button_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Button  : in out Button_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      Button.Create_From_HTML
        (Parent, Escape_Quotes ("<button type='button'>" & Content &
           "</button>"), ID);
   end Create;

   --------------
   -- Disabled --
   --------------

   procedure Disabled (Button : in out Button_Type;
                       Value  : in     Boolean := True)
   is
   begin
      Button.Property ("disabled", Value);
   end Disabled;

   function Disabled (Button : Button_Type) return Boolean is
   begin
      return Button.Property ("disabled");
   end Disabled;

   -------------------------------------------------------------------------
   --  DIV_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (DIV     : in out DIV_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      DIV.Create_From_HTML (Parent, "<div>" & Escape_Quotes (Content) &
                              "</div>", ID);
   end Create;

   -------------------------------------------------------------------------
   --  P_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (P       : in out P_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      P.Create_From_HTML (Parent, "<p>" & Escape_Quotes (Content) &
                              "</p>", ID);
   end Create;

   -------------------------------------------------------------------------
   --  IMG_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (IMG              : in out IMG_Type;
      Parent           : in out Gnoga.Gui.Base_Type'Class;
      URL_Source       : in     String := "";
      Alternative_Text : in     String := "";
      ID               : in     String := "")
   is
   begin
      IMG.Create_From_HTML (Parent,
                            Escape_Quotes ("<img src='" & Escape_Inner_Quotes (URL_Source) &
                              "' Alt='" & Escape_Inner_Quotes (Alternative_Text) &
                              "'>"), ID);
   end Create;

   -----------------
   --  URL_Source --
   -----------------

   procedure URL_Source (IMG : in out IMG_Type; Value : in String) is
   begin
      IMG.Attribute ("src", Value);
   end URL_Source;

   -------------------------------------------------------------------------
   --  HR_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (HR     : in out HR_Type;
      Parent : in out Gnoga.Gui.Base_Type'Class;
      ID     : in     String := "")
   is
   begin
      HR.Create_From_HTML (Parent, "<hr />", ID);
   end Create;

   -------------------------------------------------------------------------
   --  BR_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (BR     : in out BR_Type;
      Parent : in out Gnoga.Gui.Base_Type'Class;
      ID     : in     String := "")
   is
   begin
      BR.Create_From_HTML (Parent, "<br />", ID);
   end Create;

   -------------------------------------------------------------------------
   --  Meter_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Meter   : in out Meter_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Value   : in     Integer := 0;
                     High    : in     Integer := 100;
                     Low     : in     Integer := 0;
                     Maximum : in     Integer := 100;
                     Minimum : in     Integer := 0;
                     Optimum : in     Integer := 50;
                     ID      : in     String := "")
   is
      pragma Unreferenced (ID);
   begin
      Meter.Create_From_HTML (Parent,
                              "<meter high=" & High'Img &
                                " low=" & Low'Img &
                                " max=" & Maximum'Img &
                                " min=" & Minimum'Img &
                                " optimum=" & Optimum'Img &
                                " value=" & Value'Img &
                                " />");
   end Create;

   procedure Value (Meter : in out Meter_Type; Value : in Integer) is
   begin
      Meter.Property ("value", Value);
   end Value;

   function Value (Meter : Meter_Type) return Integer is
   begin
      return Meter.Property ("value");
   end Value;

   procedure High (Meter : in out Meter_Type; Value : in Integer) is
   begin
      Meter.Property ("high", Value);
   end High;

   function High (Meter : Meter_Type) return Integer is
   begin
      return Meter.Property ("high");
   end High;

   procedure Low (Meter : in out Meter_Type; Value : in Integer) is
   begin
      Meter.Property ("low", Value);
   end Low;

   function Low (Meter : Meter_Type) return Integer is
   begin
      return Meter.Property ("low");
   end Low;

   procedure Maximum (Meter : in out Meter_Type; Value : in Integer) is
   begin
      Meter.Property ("max", Value);
   end Maximum;

   function Maximum (Meter : Meter_Type) return Integer is
   begin
      return Meter.Property ("max");
   end Maximum;

   procedure Minimum (Meter : in out Meter_Type; Value : in Integer) is
   begin
      Meter.Property ("min", Value);
   end Minimum;

   function Minimum (Meter : Meter_Type) return Integer is
   begin
      return Meter.Property ("min");
   end Minimum;

   procedure Optimum (Meter : in out Meter_Type; Value : in Integer) is
   begin
      Meter.Property ("optimum", Value);
   end Optimum;

   function Optimum (Meter : Meter_Type) return Integer is
   begin
      return Meter.Property ("optimum");
   end Optimum;

   -------------------------------------------------------------------------
   --  Progress_Bar_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Progress_Bar : in out Progress_Bar_Type;
                     Parent       : in out Gnoga.Gui.Base_Type'Class;
                     Value        : in     Integer := 0;
                     Maximum      : in     Integer := 100;
                     ID           : in     String := "")
   is
      pragma Unreferenced (ID);
   begin
      Progress_Bar.Create_From_HTML (Parent,
                              "<progress" &
                                " max=" & Maximum'Img &
                                " value=" & Value'Img &
                                " />");

   end Create;

   procedure Value (Progress_Bar : in out Progress_Bar_Type;
                    Value        : in     Integer)
   is
   begin
      Progress_Bar.Property ("value", Value);
   end Value;

   function Value (Progress_Bar : Progress_Bar_Type) return Integer is
   begin
      return Progress_Bar.Property ("value");
   end Value;

   procedure Maximum (Progress_Bar : in out Progress_Bar_Type;
                      Value        : in     Integer)
   is
   begin
      Progress_Bar.Property ("max", Value);
   end Maximum;

   function Maximum (Progress_Bar : Progress_Bar_Type) return Integer is
   begin
      return Progress_Bar.Property ("max");
   end Maximum;

   -------------------------------------------------------------------------
   --  Span_Types
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Span    : in out Span_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      Span.Create_From_HTML (Parent, "<span>" & Escape_Quotes (Content) &
                              "</span>", ID);
   end Create;
end Ada_GUI.Gnoga.Gui.Element.Common;

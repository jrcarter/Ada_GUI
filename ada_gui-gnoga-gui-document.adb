-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . G U I . D O C U M E N T                   --
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

package body Ada_GUI.Gnoga.Gui.Document is

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Document      : in out Document_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String                     := "window";
      ID_Type       : in     Gnoga.ID_Enumeration := Gnoga.Script)
   is
   begin
      if ID_Type = Gnoga.DOM_ID then
         raise Invalid_ID_Type;
      end if;

      Attach (Object        => Base_Type (Document),
              Connection_ID => Connection_ID,
              ID            => Script_Accessor (ID, ID_Type) & ".document",
              ID_Type       => Gnoga.Script);

      Document.DOM_HTML.Attach (Connection_ID => Connection_ID,
                                ID            => Document.Script_Accessor &
                                  ".documentElement",
                                ID_Type       => Gnoga.Script);

      Document.DOM_Head.Attach (Connection_ID => Connection_ID,
                                ID            => Document.Script_Accessor &
                                  ".head",
                                ID_Type       => Gnoga.Script);

      Document.DOM_Body.Attach (Connection_ID => Connection_ID,
                                ID            => Document.Script_Accessor &
                                  ".body",
                                ID_Type       => Gnoga.Script);
   end Attach;

   ------------
   -- Domain --
   ------------

   function Domain (Document : Document_Type) return String is
   begin
      return Document.Property ("domain");
   end Domain;

   --------------------
   -- Input_Encoding --
   --------------------

   function Input_Encoding (Document : Document_Type) return String is
   begin
      return Document.Property ("inputEncoding");
   end Input_Encoding;

   -------------------
   -- Last_Modified --
   -------------------

   function Last_Modified (Document : Document_Type) return String is
   begin
      return Document.Property ("lastModified");
   end Last_Modified;

   -------------
   -- Referer --
   -------------

   function Referrer (Document : Document_Type) return String is
   begin
      return Document.Property ("referrer");
   end Referrer;

   -----------
   -- Title --
   -----------

   procedure Title (Document : in out Document_Type; Value : String) is
   begin
      Document.Property ("title", Value);
   end Title;

   function Title (Document : Document_Type) return String is
   begin
      return Document.Property ("title");
   end Title;

   ---------
   -- URL --
   ---------

   function URL (Document : Document_Type) return String is
   begin
      return Document.Property ("URL");
   end URL;

   ----------------------
   -- Document_Element --
   ----------------------

   function Document_Element
     (Document : Document_Type)
      return Gnoga.Gui.Element.Element_Access
   is
   begin
      return Document.DOM_HTML'Unrestricted_Access;
   end Document_Element;

   ------------------
   -- Head_Element --
   ------------------

   function Head_Element
     (Document : Document_Type)
      return Gnoga.Gui.Element.Element_Access
   is
   begin
      return Document.DOM_Head'Unrestricted_Access;
   end Head_Element;

   ------------------
   -- Body_Element --
   ------------------

   function Body_Element
     (Document : Document_Type)
      return Gnoga.Gui.Element.Element_Access
   is
   begin
      return Document.DOM_Body'Unrestricted_Access;
   end Body_Element;

   -----------------
   -- Ready_State --
   -----------------

   function Ready_State (Document : Document_Type)
                         return Ready_State_Type
   is
   begin
      return Ready_State_Type'Value (Document.Property ("readyState"));
   end Ready_State;

   --------------
   -- Load_CSS --
   --------------

   procedure Load_CSS (Document : in out Document_Type; URL : String) is
   begin
      Document.Head_Element.jQuery_Execute
        ("append('" & Escape_Quotes ("<link rel='stylesheet' " &
           "href='" & Escape_Inner_Quotes (URL) & "' type='text/css'>'") & "')");
   end Load_CSS;

   -----------
   -- Write --
   -----------

   procedure Write (Document : in out Document_Type; Value : String) is
   begin
      Document.Execute ("write('" & Escape_Quotes (Value) & "');");
   end Write;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (Document : in out Document_Type; Value : String) is
   begin
      Document.Execute ("writeln('" & Escape_Quotes (Value) & "');");
   end Write_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Document : in out Document_Type; Value : String) is
   begin
      Document.Write_Line (Value & "<br />");
   end Put_Line;

end Ada_GUI.Gnoga.Gui.Document;

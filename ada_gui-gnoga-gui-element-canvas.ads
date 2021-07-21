-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--              G N O G A . G U I . E L E M E N T . C A N V A S             --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Finalization;

package Ada_GUI.Gnoga.Gui.Element.Canvas is
   -------------------------------------------------------------------------
   --  Canvas_Types
   -------------------------------------------------------------------------

   type Canvas_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Canvas_Access is access all Canvas_Type;
   type Pointer_To_Canvas_Class is access all Canvas_Type'Class;

   -------------------------------------------------------------------------
   --  Canvas_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Canvas  : in out Canvas_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     ID      : in     String := "");
   --  Create a Canvas container

   -------------------------------------------------------------------------
   --  Context_Types
   -------------------------------------------------------------------------

   type Context_Type is
     new Ada.Finalization.Limited_Controlled with private;
   type Context_Access is access all Context_Type;
   type Pointer_To_Context_Class is access all Context_Type'Class;

   overriding procedure Finalize (Object : in out Context_Type);
   --  Clear browser reference to created context

   -------------------------------------------------------------------------
   --  Context_Type - Properties
   -------------------------------------------------------------------------

   procedure Property (Context : in out Context_Type;
                       Name    : in     String;
                       Value   : in     String);
   procedure Property (Context : in out Context_Type;
                       Name    : in     String;
                       Value   : in     Integer);
   procedure Property (Context : in out Context_Type;
                       Name    : in     String;
                       Value   : in     Boolean);
   procedure Property (Context : in out Context_Type;
                       Name    : in     String;
                       Value   : in     Float);
   function Property (Context : Context_Type; Name : String) return String;
   function Property (Context : Context_Type; Name : String) return Integer;
   function Property (Context : Context_Type; Name : String) return Boolean;
   function Property (Context : Context_Type; Name : String) return Float;

   -------------------------------------------------------------------------
   --  Context_Type - Methods
   -------------------------------------------------------------------------

   procedure Execute (Context : in out Context_Type; Method : String);
   function Execute (Context : Context_Type; Method : String)
                     return String;

   --  Internal Methods --

   function ID (Context : Context_Type) return String;

   function Connection_ID (Context : Context_Type)
                           return Gnoga.Connection_ID;

private
   type Canvas_Type is new Gnoga.Gui.Element.Element_Type with null record;

   type Context_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         Connection_ID : Gnoga.Connection_ID :=
                           Gnoga.No_Connection;
         Context_ID    : Gnoga.Web_ID;
      end record;
end Ada_GUI.Gnoga.Gui.Element.Canvas;

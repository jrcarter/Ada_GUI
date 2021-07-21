--                                                                    --
--  package Tables.Names            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2003       --
--                                                                    --
--                                Last revision :  13:11 14 Sep 2019  --
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
--
--  This package defines type Dictionary derived from Table. It has same
--  functionality as the base type, but intended for keeping only  valid
--  case-insensitive names. The case  is  ignored  when  matched  (Find,
--  Get), but kept by the table. Additionally, any  non-empty  chain  of
--  characters  from  the  set  Blanks  is  considered  equivalent  when
--  matched. The procedure Check_Spelling is used to check spelling of a
--  name before placing it into the table. It may raise Constraint_Error
--  to indicate a wrong spelling. The procedure Check_Matched is used to
--  check whether the matched keyword is a proper  name.  It  is  called
--  from  Get  with  Pointer  set  to  the first character following the
--  matched  name.  It  returns  True  if  the  name  fits.  Usually  an
--  implementation  checks  whether Source (Pointer) is neither a letter
--  nor  a  digit.  Check_Matched  is  never called with Pointer outside
--  Source'Range.
--
with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

generic
   with procedure Check_Spelling (Name : String) is <>;
   with function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is <>;
   Blanks : Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set (' ' & Ada.Characters.Latin_1.HT);
package Tables.Names is
   type Dictionary is new Table with private;
--
-- Add -- Overrides Tables...
--
-- Constraint_Error is propagated if Name is spelled incorrectly.
--
   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             );
   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             );
--
-- Delete -- Overrides Tables...
--
   procedure Delete (Folder : in out Dictionary; Name : String);
--
-- Find -- Overrides Tables...
--
   function Find (Folder : Dictionary; Name : String) return Tag;
--
-- Get -- Overrides Tables...
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Data    : out Tag
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Data    : out Tag;
                Got_It  : out Boolean
             );
--
-- IsIn -- Overrides Tables...
--
   function IsIn (Folder : Dictionary; Name : String) return Boolean;
--
-- Locate -- Overrides Tables...
--
   function Locate (Folder : Dictionary; Name : String) return Natural;
   procedure Locate
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Offset  : out Natural
             );
--
-- Replace -- Overrides Tables...
--
-- Constraint_Error is propagated if Name is spelled incorrectly.
--
   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             );
   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             );
private
   pragma Inline (Find);
   pragma Inline (IsIn);

   type Dictionary is new Table with null record;
--
-- Search -- Overrides Tables...
--
   function Search (Folder : Dictionary; Name : String)
      return Integer;

end Tables.Names;

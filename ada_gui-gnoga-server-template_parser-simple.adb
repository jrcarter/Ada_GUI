-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . S E R V E R . T E M P L A T E _ P A R S E R . S I M P L E  --
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

with Ada.Text_IO;

package body Ada_GUI.Gnoga.Server.Template_Parser.Simple is
   ---------------
   -- Load_View --
   ---------------

   function Load_View (Name : String) return String is
      Empty_Data : View_Data;
   begin
      return Load_View (Name, Empty_Data);
   end Load_View;

   function Load_View (Name     : String;
                       Data_Map : Gnoga.Data_Map_Type;
                       Var_Name : String := "data")
                       return String
   is
      Data : View_Data;
   begin
      Data.Insert_Map (Data_Map);
      Data.Variable_Name (Var_Name);

      return Load_View (Name, Data);
   end Load_View;

   function Load_View (Name : String; Data : View_Data)
                       return String
   is
   begin
      return Load_View (Name, Data_List => (1 => Data));
   end Load_View;

   function Load_View (Name      : String;
                       Data_List : View_Data_Array)
                       return String
   is
      use Ada.Strings.Unbounded;

      Error_Queue_Data : View_Data;
      Info_Queue_Data  : View_Data;

      Parsed_File : Ada.Strings.Unbounded.Unbounded_String;

      procedure Load_File;
      procedure Parse_Data;
      procedure Replace_Values (D : View_Data);
      procedure Replace_In_String (S, M : String);

      procedure Load_File is
         use Ada.Text_IO;

         F : File_Type;
      begin
         Open (File => F,
               Mode => In_File,
               Name => Parse_Name (Name),
               Form => "shared=no");

         while not End_Of_File (F) loop
            if Length (Parsed_File) > 0 then
               Parsed_File :=
                 Parsed_File & (Character'Val (10) & Get_Line (F));
            else
               Parsed_File := To_Unbounded_String (Get_Line (F));
            end if;
         end loop;

         Close (F);
      end Load_File;

      procedure Parse_Data is
      begin
         for i in Data_List'First .. Data_List'Last loop
            Replace_Values (Data_List (i));
         end loop;

         Error_Queue_Data.Insert_Array (Error_Queue);
         Error_Queue_Data.Variable_Name ("gnoga_errors");
         Replace_Values (Error_Queue_Data);

         Info_Queue_Data.Insert_Array (Info_Queue);
         Info_Queue_Data.Variable_Name ("gnoga_infos");
         Replace_Values (Info_Queue_Data);
      end Parse_Data;

      procedure Replace_Values (D : View_Data) is
         use Gnoga.Data_Maps;
         use Gnoga.Maps_of_Data_Maps;

         Var_Name : constant String := To_String (D.Name);
      begin
         for C in D.String_Values.Iterate loop
            Replace_In_String ("@@" & Var_Name & "." & Key (C) & "@@",
                               Element (C));
         end loop;

         for M in D.Map_Values.Iterate loop
            for C in Element (M).Iterate loop
               Replace_In_String
                 ("@@" & Var_Name & "." & Key (M) & "." & Key (C) & "@@",
                  Element (C));
            end loop;
         end loop;
      end Replace_Values;

      procedure Replace_In_String (S, M : String) is
         N : Natural := 0;
      begin
         loop
            N := Index (Parsed_File, S);

            if N > 0 then
               Replace_Slice (Parsed_File, N, N + S'Length - 1, M);
            end if;

            exit when N = 0;
         end loop;
      end Replace_In_String;
   begin
      Load_File;
      Parse_Data;
      return To_String (Parsed_File);
   end Load_View;

end Ada_GUI.Gnoga.Server.Template_Parser.Simple;

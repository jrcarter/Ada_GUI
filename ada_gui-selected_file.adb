-- An Ada-oriented GUI library
-- Implementation derived from Gnoga
--
-- Copyright (C) 2022 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;

separate (Ada_GUI)
function Selected_File (Initial_Directory : in String := ".") return File_Result_Info is
   Directory_Tag : constant String := " (directory)";

   procedure Fill_List (Directory : in String; List : in out Gnoga.Gui.Element.Form.Selection_Type);
      -- Clears List, then adds the files in Directory to it, directories first, in alphabetical order

   procedure File_Selected;
      -- User clicked on a file in the file list

   procedure Clean_Up (Dialog_Exists : in Boolean);
      -- Clean-up actions before returning; removes dialog if Dialog_Exists; signals window closed if not Dialog_Exists

   package Name_Lists is new Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

   use Ada.Strings.Unbounded;

   Old_View : constant Gnoga.Gui.Pointer_To_Base_Class := Window.Get_View;

   use type Gnoga.Gui.Pointer_To_Base_Class;

   procedure Fill_List (Directory : in String; List : in out Gnoga.Gui.Element.Form.Selection_Type) is
      procedure Add_Dir (Position : in Name_Lists.Cursor);
      -- Adds the Name at Position to List with Directory_Tag added to the end

      procedure Add_File (Position : in Name_Lists.Cursor);
      -- Adds the name at Position to List

      procedure Add_Dir (Position : in Name_Lists.Cursor) is
         Name : constant String := Name_Lists.Element (Position) & Directory_Tag;
      begin -- Add_Dir
         List.Add_Option (Value => Name, Text => Name);
      end Add_Dir;

      procedure Add_File (Position : in Name_Lists.Cursor) is
         Name : constant String := Name_Lists.Element (Position);
      begin -- Add_File
         List.Add_Option (Value => Name, Text => Name);
      end Add_File;

      Search_Info : Ada.Directories.Search_Type;
      File_Info   : Ada.Directories.Directory_Entry_Type;
      Dir_List    : Name_Lists.Set;
      File_List   : Name_Lists.Set;
      Index       : Natural;
   begin -- Fill_List
      Ada.Directories.Start_Search (Search    => Search_Info,
                                    Directory => Directory,
                                    Pattern   => "*",
                                    Filter    => (Ada.Directories.Special_File => False, others => True) );

      All_Entries : loop
         exit All_Entries when not Ada.Directories.More_Entries (Search_Info);

         Ada.Directories.Get_Next_Entry (Search => Search_Info, Directory_Entry => File_Info);

         case Ada.Directories.Kind (File_Info) is
         when Ada.Directories.Directory =>
            Get_Name : declare
               Name : constant String := Ada.Directories.Simple_Name (File_Info);
            begin -- Get_Name
               if Name /= "." and Name /= ".." then
                  Dir_List.Insert  (New_Item => Name);
               end if;
            end Get_Name;
         when Ada.Directories.Ordinary_File =>
            File_List.Insert  (New_Item => Ada.Directories.Simple_Name (File_Info) );
         when Ada.Directories.Special_File =>
            null;
         end case;
      end loop All_Entries;

      Index := List.Selected_Index;
      List.Visible (Value => False);
      List.Empty_Options;
      Dir_List.Iterate (Process => Add_Dir'Access);
      File_List.Iterate (Process => Add_File'Access);

      if Index /= 0 then
         List.Selected (Index => Index);
      end if;

      List.Visible (Value => True);
   end Fill_List;

   Result      : File_Result_Info;
   Current_Dir : Unbounded_String := To_Unbounded_String (Ada.Directories.Full_Name (Initial_Directory) );
   File_List   : Gnoga.Gui.Element.Form.Selection_Type;
   File_Input  : Gnoga.Gui.Element.Form.Text_Type;

   procedure File_Selected is
      Index : constant Natural := File_List.Selected_Index;
   begin -- File_Selected
      if Index = 0 then
         return;
      end if;

      Get_Name : declare
         Name : constant String := File_List.Value (Index);
      begin -- Get_Name
         if Name'Length <= Directory_Tag'Length or else Name (Name'Last - Directory_Tag'Length + 1 .. Name'Last) /= Directory_Tag
         then -- Normal file
            File_Input.Value (Value => Name);
         else -- Directory
            Current_Dir := To_Unbounded_String (Ada.Directories.Compose (To_String (Current_Dir),
                                                Name (Name'First .. Name'Last - Directory_Tag'Length) ) );
            File_Input.Value (Value => "");
         end if;
      end Get_Name;
   end File_Selected;

   Modal_Background : Gnoga.Gui.View.View_Type;
   Modal_Frame      : Gnoga.Gui.View.View_Type;

   procedure Clean_Up (Dialog_Exists : in Boolean) is
      Event : Gnoga.Gui.Event_Info;

      use type Ada.Containers.Count_Type;
   begin -- Clean_Up
      Empty : loop
         exit Empty when Gnoga.Gui.Event_Queue.Current_Use = 0;

         Gnoga.Gui.Event_Queue.Dequeue (Element => Event);
      end loop Empty;

      File_Selection_Control.Set_Selecting (Value => False);

      if Dialog_Exists then
         Modal_Background.Remove;
      else
         Gnoga.Gui.Event_Queue.Enqueue (New_Item => (Event => To_Unbounded_String (Closed_Text), others => <>) );
      end if;
   exception -- Clean_Up
   when E : others =>
      Gnoga.Log (Message => "Selected_File.Clean_Up: " & Ada.Exceptions.Exception_Information (E) );
   end Clean_Up;

   View     : Gnoga.Gui.View.View_Type;
   Form     : Gnoga.Gui.Element.Form.Form_Type;
   Dir_Line : Gnoga.Gui.Element.Form.Text_Type;
   Up       : Gnoga.Gui.Element.Common.Button_Type;
   Cancel   : Gnoga.Gui.Element.Common.Button_Type;
   OK       : Gnoga.Gui.Element.Common.Button_Type;
   Event    : Gnoga.Gui.Event_Info;
   Exists   : Boolean := False;
begin -- Selected_File
   if File_Selection_Control.Selecting then
      return (Picked => False);
   end if;

   File_Selection_Control.Set_Selecting (Value => True);

      --  Create the Dialog Background view
   Modal_Background.Create (Parent => Window);
   Exists := True;

      --  Creating a view using Window as the parent sets the view as Window's main view.  This sets it back to the original.
   if Old_View /= null then
      Window.Set_View (Object => Old_View.all);
   end if;

      --  Configure the Modal Background
   Modal_Background.Fill_Parent;
   Modal_Background.Background_Color (Value => "Grey");
   Modal_Background.Opacity (Alpha => 1.0);
   Modal_Background.Z_Index (Value => Integer'Last);

      --  Create the containing view of the dialog
   Modal_Frame.Create (Parent => Modal_Background);
   Modal_Frame.Position (Value => Gnoga.Gui.Element.Fixed);
   View.Create (Parent => Modal_Frame);
   View.Background_Color (Value => "White");
   Form.Create (Parent => View);
   Dir_Line.Create (Form => Form, Size => 100);
   Dir_Line.Read_Only;
   Up.Create (Parent => Form, Content => "Up");
   Form.New_Line;
   File_List.Create (Form => Form, Visible_Lines => 20);
   Form.New_Line;
   File_Input.Create (Form => Form, Size => 50);
   Cancel.Create (Parent => Form, Content => "Cancel");
   OK.Create (Parent => Form, Content => "OK");

   Center : declare
      Top_Offset  : Integer := Integer'Max (Modal_Background.Height / 2 - Modal_Frame.Height / 2, 0);
      Left_Offset : Integer := Integer'Max (Modal_Background.Width / 2 - Modal_Frame.Width / 2, 0);
   begin -- Center
      Modal_Frame.Top (Value => Modal_Background.Offset_From_Top  + Top_Offset);
      Modal_Frame.Left (Value => Modal_Background.Offset_From_Left + Left_Offset);
   end Center;

   All_Events : loop
      Dir_Line.Value (Value => To_String (Current_Dir) );
      Fill_List (Directory => To_String (Current_Dir), List => File_List);
      Gnoga.Gui.Event_Queue.Dequeue (Element => Event);

      if Event.Event = Closed_Text then
         Result := (Picked => False);
         Exists := False;

         exit All_Events;
      elsif Event.Event = "click" then
         if Event.Object.Unique_ID = Up.Unique_ID then
            Current_Dir := To_Unbounded_String (Ada.Directories.Containing_Directory (To_String (Current_Dir) ) );
         elsif Event.Object.Unique_ID = File_List.Unique_ID then
            File_Selected;
         elsif Event.Object.Unique_ID = Cancel.Unique_ID then
            Result := (Picked => False);

            exit All_Events;
         elsif Event.Object.Unique_ID = OK.Unique_ID then
            Get_Name : declare
               Name : constant String := File_Input.Value;
            begin -- Get_Name
               if Name /= "" then
                  Result :=
                     (Picked => True, Value => To_Unbounded_String (Ada.Directories.Compose (To_String (Current_Dir), Name) ) );

                  exit All_Events;
               end if;
            end Get_Name;
         else
            null;
         end if;
      else
         null;
      end if;
   end loop All_Events;

   Clean_Up (Dialog_Exists => Exists);

   return Result;
exception -- Selected_File
when E : others =>
   Gnoga.Log (Message => "Selected_File: " & Ada.Exceptions.Exception_Information (E) );
   Clean_Up (Dialog_Exists => Exists);

   return (Picked => False);
end Selected_File;

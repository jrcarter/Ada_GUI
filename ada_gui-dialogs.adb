-- An Ada-oriented GUI library
-- Implementation derived from Gnoga
--
-- Copyright (C) 2022 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;

separate (Ada_GUI)
package body Dialogs is
   procedure Set_Up
      (Background : out Gnoga.Gui.View.View_Type; Frame : out Gnoga.Gui.View.View_Type; View : out Gnoga.Gui.View.View_Type);
   -- Sets up Background, Frame, and View for a dialog
   -- Background should be passed to Clean_Up before the dialog returns
   -- View is where the dialog sets up its widgets

   procedure Center (Background : in out Gnoga.Gui.View.View_Type; Frame : in out Gnoga.Gui.View.View_Type);
   -- After adding widgets to the View from Set_Up, pass the Background and Frame to center the frame in the window

   procedure Clean_Up (Background : in out Gnoga.Gui.View.View_Type; Dialog_Exists : in Boolean);
   -- Cleans up a dialog before returning
   -- Background should have been set up as the Background parameter to Set_Up
   -- Dialog_Exists should be False if the user has closed the window; True otherwise
   -- Empties the event queue
   -- Allows other dialogs or Next_Event to proceed
   -- If Dialog_Exists, removes Background, else adds a window-closed event to the queue

   function Selected_File (Initial_Directory : in String := ".") return File_Result_Info is
      Directory_Tag : constant String := " (directory)";

      procedure Fill_List (Directory : in String; List : in out Gnoga.Gui.Element.Form.Selection_Type);
      -- Clears List, then adds the files in Directory to it, directories first, in alphabetical order

      procedure File_Selected;
      -- User clicked on a file in the file list

      package Name_Lists is new Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

      use Ada.Strings.Unbounded;

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
               Current_Dir :=
                  To_Unbounded_String (Ada.Directories.Compose (To_String (Current_Dir),
                                                                Name (Name'First .. Name'Last - Directory_Tag'Length) ) );
               File_Input.Value (Value => "");
            end if;
         end Get_Name;
      end File_Selected;

      Background : Gnoga.Gui.View.View_Type;
      Frame      : Gnoga.Gui.View.View_Type;
      View       : Gnoga.Gui.View.View_Type;
      Form       : Gnoga.Gui.Element.Form.Form_Type;
      Dir_Line   : Gnoga.Gui.Element.Form.Text_Type;
      Up         : Gnoga.Gui.Element.Common.Button_Type;
      Cancel     : Gnoga.Gui.Element.Common.Button_Type;
      OK         : Gnoga.Gui.Element.Common.Button_Type;
      Event      : Gnoga.Gui.Event_Info;
      Exists     : Boolean := False;
   begin -- Selected_File
      if Dialog_Control.Ongoing then
         return (Picked => False);
      end if;

      Dialog_Control.Set_Ongoing (Value => True);
      Set_Up (Background => Background, Frame => Frame, View => View);
      Exists := True;
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
      Center (Background => Background, Frame => Frame);

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

      Clean_Up (Background => Background, Dialog_Exists => Exists);

      return Result;
   exception -- Selected_File
   when E : others =>
      Gnoga.Log (Message => "Dialogs.Selected_File: " & Ada.Exceptions.Exception_Information (E) );
      Clean_Up (Background => Background, Dialog_Exists => Exists);

      return (Picked => False);
   end Selected_File;

   function Selected_Button (Title : in String; Text : in String; Button : in Text_List) return String is
      type Button_List is array (Button'Range) of Gnoga.Gui.Element.Common.Button_Type;

      Background  : Gnoga.Gui.View.View_Type;
      Frame       : Gnoga.Gui.View.View_Type;
      View        : Gnoga.Gui.View.View_Type;
      Title_View  : Gnoga.Gui.View.View_Type;
      Text_View   : Gnoga.Gui.View.View_Type;
      Button_View : Gnoga.Gui.View.View_Type;
      Control     : Button_List;
      Exists      : Boolean := False;
      Event       : Gnoga.Gui.Event_Info;
      Result      : Unbounded_String;
   begin -- Selected_Button
      if Dialog_Control.Ongoing then
         return "";
      end if;

      Dialog_Control.Set_Ongoing (Value => True);
      Set_Up (Background => Background, Frame => Frame, View => View);
      Exists := True;
      Title_View.Create (Parent => View);
      Title_View.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      Title_View.Background_Color (Enum => Gnoga.Colors.Light_Blue);
      Title_View.Put (Message => Title);
      View.New_Line;
      Text_View.Create (Parent => View);
      Text_View.Text_Alignment (Value => Gnoga.Gui.Element.Left);
      Text_View.Put (Message => Text);
      View.New_Line;
      Button_View.Create (Parent => View);
      Button_View.Text_Alignment (Value => Gnoga.Gui.Element.Right);
      Button_View.Background_Color (Enum => Gnoga.Colors.Light_Blue);

      Create_Buttons : for I in Control'Range loop
         Control (I).Create (Parent => Button_View, Content => To_String (Button (I) ) );
      end loop Create_Buttons;

      Center (Background => Background, Frame => Frame);

      All_Events : loop
         Gnoga.Gui.Event_Queue.Dequeue (Element => Event);

         if Event.Event = Closed_Text then
            Result := Null_Unbounded_String;
            Exists := False;

            exit All_Events;
         elsif Event.Event = "click" then
            Find_Button : for I in Control'Range loop
               if Event.Object.Unique_ID = Control (I).Unique_ID then
                  Result := Button (I);

                  exit All_Events;
               end if;
            end loop Find_Button;
         else
            null;
         end if;
      end loop All_Events;

      Clean_Up (Background => Background, Dialog_Exists => Exists);

      return To_String (Result);
   exception -- Selected_Button
   when E : others =>
      Gnoga.Log (Message => "Dialogs.Selected_Button: " & Ada.Exceptions.Exception_Information (E) );
      Clean_Up (Background => Background, Dialog_Exists => Exists);

      return "";
   end Selected_Button;

   procedure Set_Up
      (Background : out Gnoga.Gui.View.View_Type; Frame : out Gnoga.Gui.View.View_Type; View : out Gnoga.Gui.View.View_Type)
   is
      Old_View : constant Gnoga.Gui.Pointer_To_Base_Class := Window.Get_View;

      use type Gnoga.Gui.Pointer_To_Base_Class;
   begin -- Set_Up
      --  Create the Dialog Background view
      Background.Create (Parent => Window);

      --  Creating a view using Window as the parent sets the view as Window's main view.  This sets it back to the original.
      if Old_View /= null then
         Window.Set_View (Object => Old_View.all);
      end if;

      --  Configure the Modal Background
      Background.Fill_Parent;
      Background.Background_Color (Value => "Grey");
      Background.Opacity (Alpha => 1.0);
      Background.Z_Index (Value => Integer'Last);

      --  Create the containing view of the dialog
      Frame.Create (Parent => Background);
      Frame.Position (Value => Gnoga.Gui.Element.Fixed);
      View.Create (Parent => Frame);
      View.Background_Color (Value => "White");
   exception -- Set_Up
   when E : others =>
      Gnoga.Log (Message => "Dialogs.Set_Up: " & Ada.Exceptions.Exception_Information (E) );
   end Set_Up;

   procedure Center (Background : in out Gnoga.Gui.View.View_Type; Frame : in out Gnoga.Gui.View.View_Type) is
      Top_Offset  : Integer := Integer'Max (Background.Height / 2 - Frame.Height / 2, 0);
      Left_Offset : Integer := Integer'Max (Background.Width / 2 - Frame.Width / 2, 0);
   begin -- Center
      Frame.Top (Value => Background.Offset_From_Top  + Top_Offset);
      Frame.Left (Value => Background.Offset_From_Left + Left_Offset);
   exception -- Center
   when E : others =>
      Gnoga.Log (Message => "Dialogs.Center: " & Ada.Exceptions.Exception_Information (E) );
   end Center;

   procedure Clean_Up (Background : in out Gnoga.Gui.View.View_Type; Dialog_Exists : in Boolean) is
      Event : Gnoga.Gui.Event_Info;

      use type Ada.Containers.Count_Type;
   begin -- Clean_Up
      Empty : loop
         exit Empty when Gnoga.Gui.Event_Queue.Current_Use = 0;

         Gnoga.Gui.Event_Queue.Dequeue (Element => Event);
      end loop Empty;

      Dialog_Control.Set_Ongoing (Value => False);

      if Dialog_Exists then
         Background.Remove;
      else
         Gnoga.Gui.Event_Queue.Enqueue (New_Item => (Event => To_Unbounded_String (Closed_Text), others => <>) );
      end if;
   exception -- Clean_Up
   when E : others =>
      Gnoga.Log (Message => "Dialogs.Clean_Up: " & Ada.Exceptions.Exception_Information (E) );
   end Clean_Up;
end Dialogs;

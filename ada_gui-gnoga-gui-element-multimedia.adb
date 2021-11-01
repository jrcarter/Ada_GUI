-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . G U I . E L E M E N T . M U L T I M E D I A          --
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
package body Ada_GUI.Gnoga.Gui.Element.Multimedia is
   ----------
   -- Play --
   ----------

   procedure Play (Media : in out Multimedia_Type) is
   begin
      Media.Execute ("play()");
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause (Media : in out Multimedia_Type) is
   begin
      Media.Execute ("pause()");
   end Pause;

   ----------
   -- Load --
   ----------

   procedure Load (Media : in out Multimedia_Type) is
   begin
      Media.Execute ("load()");
   end Load;

   function Can_Play (Media : Multimedia_Type; Media_Type : String)
                      return Boolean
   is
   begin
      return Media.Execute ("canPlayType ('" & Media_Type & "')") /= "";
   end Can_Play;

   --------------------
   -- Media_Duration --
   --------------------

   function Media_Duration (Media : Multimedia_Type) return Float is
   begin
      return Media.Property ("duration");
   end Media_Duration;

   ------------------
   -- Media_Source --
   ------------------

   procedure Media_Source (Media : in out Multimedia_Type; Source : in String)
   is
   begin
      Media.Property ("src", Source);
   end Media_Source;

   function Media_Source (Media : Multimedia_Type) return String is
   begin
      return Media.Property ("src");
   end Media_Source;

   --------------------
   -- Media_Position --
   --------------------

   procedure Media_Position (Media   : in out Multimedia_Type;
                             Seconds : in     Float)
   is
   begin
      Media.Property ("currentTime", Seconds);
   end Media_Position;

   function Media_Position (Media : Multimedia_Type) return Float is
   begin
      return Media.Property ("currentTime");
   end Media_Position;

   --------------------
   -- Playback_Ended --
   --------------------

   function Playback_Ended (Media : Multimedia_Type) return Boolean is
   begin
      return Media.Property ("ended");
   end Playback_Ended;

   ----------------
   -- Loop_Media --
   ----------------

   procedure Loop_Media (Media : in out Multimedia_Type; Value : Boolean) is
   begin
      Media.Property ("loop", Value);
   end Loop_Media;

   function Loop_Media (Media : Multimedia_Type) return Boolean is
   begin
      return Media.Property ("loop");
   end Loop_Media;

   -----------
   -- Muted --
   -----------

   procedure Muted (Media : in out Multimedia_Type; Value : Boolean) is
   begin
      Media.Property ("muted", Value);
   end Muted;

   function Muted (Media : Multimedia_Type) return Boolean is
   begin
      return Media.Property ("muted");
   end Muted;

   ------------
   -- Paused --
   ------------

   function Paused (Media : Multimedia_Type) return Boolean is
   begin
      return Media.Property ("paused");
   end Paused;

   -------------------
   -- Playback_Rate --
   -------------------

   procedure Playback_Rate (Media : in out Multimedia_Type; Value : in Float)
   is
   begin
      Media.Property ("playbackRate", Value);
   end Playback_Rate;

   function Playback_Rate (Media : Multimedia_Type) return Float is
   begin
      return Media.Property ("playbackRate");
   end Playback_Rate;

   -------------------
   -- Ready_To_Play --
   -------------------

   function Ready_To_Play (Media : Multimedia_Type) return Boolean is
   begin
      return Media.Property ("readyState") /= 0;
   end Ready_To_Play;

   -------------
   -- Seeking --
   -------------

   function Seeking (Media : Multimedia_Type) return Boolean is
   begin
      return Media.Property ("seeking");
   end Seeking;

   procedure Volume (Media : in out Multimedia_Type; Value : Volume_Range) is
   begin
      Media.Property ("volume", Value);
   end Volume;

   function Volume (Media : Multimedia_Type) return Volume_Range is
   begin
      return Media.Property ("volume");
   end Volume;

   --------------------
   -- On_Media_Abort --
   --------------------

   procedure On_Media_Abort_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Media_Abort_Event /= null then
         Media.Unbind_Event ("abort");
      end if;

      Media.On_Media_Abort_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "abort",
                           Message => "");
      end if;
   end On_Media_Abort_Handler;

   procedure Fire_On_Media_Abort (Media : in out Multimedia_Type)
   is
   begin
      if Media.On_Media_Abort_Event /= null then
         Media.On_Media_Abort_Event (Media);
      end if;
   end Fire_On_Media_Abort;

   --------------------
   -- On_Media_Error --
   --------------------

   procedure On_Media_Error_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Media_Error_Event /= null then
         Media.Unbind_Event ("error");
      end if;

      Media.On_Media_Error_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "error",
                           Message => "");
      end if;
   end On_Media_Error_Handler;

   procedure Fire_On_Media_Error (Media : in out Multimedia_Type) is
   begin
      if Media.On_Media_Error_Event /= null then
         Media.On_Media_Error_Event (Media);
      end if;
   end Fire_On_Media_Error;

   -----------------
   -- On_Can_Play --
   -----------------

   procedure On_Can_Play_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Can_Play_Event /= null then
         Media.Unbind_Event ("canplay");
      end if;

      Media.On_Can_Play_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "canplay",
                           Message => "");
      end if;
   end On_Can_Play_Handler;

   procedure Fire_On_Can_Play (Media : in out Multimedia_Type) is
   begin
      if Media.On_Can_Play_Event /= null then
         Media.On_Can_Play_Event (Media);
      end if;
   end Fire_On_Can_Play;

   -------------------------
   -- On_Can_Play_Through --
   -------------------------

   procedure On_Can_Play_Through_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Can_Play_Through_Event /= null then
         Media.Unbind_Event ("canplaythrough");
      end if;

      Media.On_Can_Play_Through_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "canplaythrough",
                           Message => "");
      end if;
   end On_Can_Play_Through_Handler;

   procedure Fire_On_Can_Play_Through (Media : in out Multimedia_Type) is
   begin
      if Media.On_Can_Play_Through_Event /= null then
         Media.On_Can_Play_Through_Event (Media);
      end if;
   end Fire_On_Can_Play_Through;

   --------------------------------
   -- On_Duration_Change_Handler --
   --------------------------------

   procedure On_Duration_Change_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Duration_Change_Event /= null then
         Media.Unbind_Event ("durationchange");
      end if;

      Media.On_Duration_Change_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "durationchange",
                           Message => "");
      end if;
   end On_Duration_Change_Handler;

   procedure Fire_On_Duration_Change (Media : in out Multimedia_Type) is
   begin
      if Media.On_Duration_Change_Event /= null then
         Media.On_Duration_Change_Event (Media);
      end if;
   end Fire_On_Duration_Change;

   ------------------------
   -- On_Emptied_Handler --
   ------------------------

   procedure On_Emptied_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Emptied_Event /= null then
         Media.Unbind_Event ("emptied");
      end if;

      Media.On_Emptied_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "emptied",
                           Message => "");
      end if;
   end On_Emptied_Handler;

   procedure Fire_On_Emptied (Media : in out Multimedia_Type) is
   begin
      if Media.On_Emptied_Event /= null then
         Media.On_Emptied_Event (Media);
      end if;
   end Fire_On_Emptied;

   ------------------------
   -- On_Ended_Handler --
   ------------------------

   procedure On_Ended_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Ended_Event /= null then
         Media.Unbind_Event ("ended");
      end if;

      Media.On_Ended_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "ended",
                           Message => "");
      end if;
   end On_Ended_Handler;

   procedure Fire_On_Ended (Media : in out Multimedia_Type) is
   begin
      if Media.On_Ended_Event /= null then
         Media.On_Ended_Event (Media);
      end if;
   end Fire_On_Ended;

   ----------------------------
   -- On_Loaded_Data_Handler --
   ----------------------------

   procedure On_Loaded_Data_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Loaded_Data_Event /= null then
         Media.Unbind_Event ("loadeddata");
      end if;

      Media.On_Loaded_Data_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "loadeddata",
                           Message => "");
      end if;
   end On_Loaded_Data_Handler;

   procedure Fire_On_Loaded_Data (Media : in out Multimedia_Type) is
   begin
      if Media.On_Loaded_Data_Event /= null then
         Media.On_Loaded_Data_Event (Media);
      end if;
   end Fire_On_Loaded_Data;

   ---------------------------------
   -- On_Loaded_Meta_Data_Handler --
   ---------------------------------

   procedure On_Loaded_Meta_Data_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Loaded_Meta_Data_Event /= null then
         Media.Unbind_Event ("loadedmetadata");
      end if;

      Media.On_Loaded_Meta_Data_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "loadedmetadata",
                           Message => "");
      end if;
   end On_Loaded_Meta_Data_Handler;

   procedure Fire_On_Loaded_Meta_Data (Media : in out Multimedia_Type) is
   begin
      if Media.On_Loaded_Meta_Data_Event /= null then
         Media.On_Loaded_Meta_Data_Event (Media);
      end if;
   end Fire_On_Loaded_Meta_Data;

   ---------------------------
   -- On_Load_Start_Handler --
   ---------------------------

   procedure On_Load_Start_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Load_Start_Event /= null then
         Media.Unbind_Event ("loadstart");
      end if;

      Media.On_Load_Start_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "loadstart",
                           Message => "");
      end if;
   end On_Load_Start_Handler;

   procedure Fire_On_Load_Start (Media : in out Multimedia_Type) is
   begin
      if Media.On_Load_Start_Event /= null then
         Media.On_Load_Start_Event (Media);
      end if;
   end Fire_On_Load_Start;

   ---------------------
   -- On_Play_Handler --
   ---------------------

   procedure On_Play_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Play_Event /= null then
         Media.Unbind_Event ("play");
      end if;

      Media.On_Play_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "play",
                           Message => "");
      end if;
   end On_Play_Handler;

   procedure Fire_On_Play (Media : in out Multimedia_Type) is
   begin
      if Media.On_Play_Event /= null then
         Media.On_Play_Event (Media);
      end if;
   end Fire_On_Play;

   ------------------------
   -- On_Pause_Handler --
   ------------------------

   procedure On_Pause_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Pause_Event /= null then
         Media.Unbind_Event ("pause");
      end if;

      Media.On_Pause_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "pause",
                           Message => "");
      end if;
   end On_Pause_Handler;

   procedure Fire_On_Pause (Media : in out Multimedia_Type) is
   begin
      if Media.On_Pause_Event /= null then
         Media.On_Pause_Event (Media);
      end if;
   end Fire_On_Pause;

   ------------------------
   -- On_Playing_Handler --
   ------------------------

   procedure On_Playing_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Playing_Event /= null then
         Media.Unbind_Event ("playing");
      end if;

      Media.On_Playing_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "playing",
                           Message => "");
      end if;
   end On_Playing_Handler;

   procedure Fire_On_Playing (Media : in out Multimedia_Type) is
   begin
      if Media.On_Playing_Event /= null then
         Media.On_Playing_Event (Media);
      end if;
   end Fire_On_Playing;

   ------------------------
   -- On_Progress_Handler --
   ------------------------

   procedure On_Progress_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Progress_Event /= null then
         Media.Unbind_Event ("progress");
      end if;

      Media.On_Progress_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "progress",
                           Message => "");
      end if;
   end On_Progress_Handler;

   procedure Fire_On_Progress (Media : in out Multimedia_Type) is
   begin
      if Media.On_Progress_Event /= null then
         Media.On_Progress_Event (Media);
      end if;
   end Fire_On_Progress;

   ----------------------------
   -- On_Rate_Change_Handler --
   ----------------------------

   procedure On_Rate_Change_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Rate_Change_Event /= null then
         Media.Unbind_Event ("ratechange");
      end if;

      Media.On_Rate_Change_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "ratechange",
                           Message => "");
      end if;
   end On_Rate_Change_Handler;

   procedure Fire_On_Rate_Change (Media : in out Multimedia_Type) is
   begin
      if Media.On_Rate_Change_Event /= null then
         Media.On_Rate_Change_Event (Media);
      end if;
   end Fire_On_Rate_Change;

   ------------------------
   -- On_Seeked_Handler --
   ------------------------

   procedure On_Seeked_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Seeked_Event /= null then
         Media.Unbind_Event ("seeked");
      end if;

      Media.On_Seeked_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "seeked",
                           Message => "");
      end if;
   end On_Seeked_Handler;

   procedure Fire_On_Seeked (Media : in out Multimedia_Type) is
   begin
      if Media.On_Seeked_Event /= null then
         Media.On_Seeked_Event (Media);
      end if;
   end Fire_On_Seeked;

   ------------------------
   -- On_Seeking_Handler --
   ------------------------

   procedure On_Seeking_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Seeking_Event /= null then
         Media.Unbind_Event ("seeking");
      end if;

      Media.On_Seeking_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "seeking",
                           Message => "");
      end if;
   end On_Seeking_Handler;

   procedure Fire_On_Seeking (Media : in out Multimedia_Type) is
   begin
      if Media.On_Seeking_Event /= null then
         Media.On_Seeking_Event (Media);
      end if;
   end Fire_On_Seeking;

   ------------------------
   -- On_Stalled_Handler --
   ------------------------

   procedure On_Stalled_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Stalled_Event /= null then
         Media.Unbind_Event ("stalled");
      end if;

      Media.On_Stalled_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "stalled",
                           Message => "");
      end if;
   end On_Stalled_Handler;

   procedure Fire_On_Stalled (Media : in out Multimedia_Type) is
   begin
      if Media.On_Stalled_Event /= null then
         Media.On_Stalled_Event (Media);
      end if;
   end Fire_On_Stalled;

   ------------------------
   -- On_Suspend_Handler --
   ------------------------

   procedure On_Suspend_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Suspend_Event /= null then
         Media.Unbind_Event ("suspend");
      end if;

      Media.On_Suspend_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "suspend",
                           Message => "");
      end if;
   end On_Suspend_Handler;

   procedure Fire_On_Suspend (Media : in out Multimedia_Type) is
   begin
      if Media.On_Suspend_Event /= null then
         Media.On_Suspend_Event (Media);
      end if;
   end Fire_On_Suspend;

   ------------------------
   -- On_Time_Update_Handler --
   ------------------------

   procedure On_Time_Update_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Time_Update_Event /= null then
         Media.Unbind_Event ("timeupdate");
      end if;

      Media.On_Time_Update_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "timeupdate",
                           Message => "");
      end if;
   end On_Time_Update_Handler;

   procedure Fire_On_Time_Update (Media : in out Multimedia_Type) is
   begin
      if Media.On_Time_Update_Event /= null then
         Media.On_Time_Update_Event (Media);
      end if;
   end Fire_On_Time_Update;

   ------------------------------
   -- On_Volume_Change_Handler --
   ------------------------------

   procedure On_Volume_Change_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Volume_Change_Event /= null then
         Media.Unbind_Event ("volumechange");
      end if;

      Media.On_Volume_Change_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "volumechange",
                           Message => "");
      end if;
   end On_Volume_Change_Handler;

   procedure Fire_On_Volume_Change (Media : in out Multimedia_Type) is
   begin
      if Media.On_Volume_Change_Event /= null then
         Media.On_Volume_Change_Event (Media);
      end if;
   end Fire_On_Volume_Change;

   ------------------------
   -- On_Waiting_Handler --
   ------------------------

   procedure On_Waiting_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event)
   is
   begin
      if Media.On_Waiting_Event /= null then
         Media.Unbind_Event ("waiting");
      end if;

      Media.On_Waiting_Event := Handler;

      if Handler /= null then
         Media.Bind_Event (Event   => "waiting",
                           Message => "");
      end if;
   end On_Waiting_Handler;

   procedure Fire_On_Waiting (Media : in out Multimedia_Type) is
   begin
      if Media.On_Waiting_Event /= null then
         Media.On_Waiting_Event (Media);
      end if;
   end Fire_On_Waiting;

   ----------------
   -- On_Message --
   ----------------

   overriding
   procedure On_Message (Object  : in out Multimedia_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      if Event = "abort" then
         Object.Fire_On_Media_Abort;
      elsif Event = "error" then
         Object.Fire_On_Media_Error;
      elsif Event = "canplay" then
         Object.Fire_On_Can_Play;
      elsif Event = "canplaythrough" then
         Object.Fire_On_Can_Play_Through;
      elsif Event = "canplay" then
         Object.Fire_On_Can_Play;
      elsif Event = "durationchange" then
         Object.Fire_On_Duration_Change;
      elsif Event = "emptied" then
         Object.Fire_On_Emptied;
      elsif Event = "ended" then
         Object.Fire_On_Ended;
      elsif Event = "error" then
         Object.Fire_On_Media_Error;
      elsif Event = "loadeddata" then
         Object.Fire_On_Loaded_Data;
      elsif Event = "loadedmetadata" then
         Object.Fire_On_Loaded_Meta_Data;
      elsif Event = "loadstart" then
         Object.Fire_On_Load_Start;
      elsif Event = "pause" then
         Object.Fire_On_Pause;
      elsif Event = "play" then
         Object.Fire_On_Play;
      elsif Event = "playing" then
         Object.Fire_On_Playing;
      elsif Event = "progress" then
         Object.Fire_On_Progress;
      elsif Event = "ratechange" then
         Object.Fire_On_Rate_Change;
      elsif Event = "seeked" then
         Object.Fire_On_Seeked;
      elsif Event = "seeking" then
         Object.Fire_On_Seeking;
      elsif Event = "stalled" then
         Object.Fire_On_Stalled;
      elsif Event = "suspend" then
         Object.Fire_On_Suspend;
      elsif Event = "timeupdate" then
         Object.Fire_On_Time_Update;
      elsif Event = "volumechange" then
         Object.Fire_On_Volume_Change;
      elsif Event = "waiting" then
         Object.Fire_On_Waiting;
      else
         Gnoga.Gui.Base_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;

   ------------
   -- Create --
   ------------

   procedure Create (Audio     : in out Audio_Type;
                     Parent    : in out Gnoga.Gui.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "")
   is
      function Has_Controls return String;
      function Has_Preload return String;
      function Has_Autoplay return String;
      function Has_Autoloop return String;
      function Has_Muted return String;
      function Has_Source return String;

      function Has_Controls return String is
      begin
         if Controls then
            return " controls";
         else
            return "";
         end if;
      end Has_Controls;

      function Has_Preload return String is
      begin
         if Preload then
            return Escape_Quotes (" preload='auto'");
         else
            return "";
         end if;
      end Has_Preload;

      function Has_Autoplay return String is
      begin
         if Autoplay then
            return " autoplay";
         else
            return "";
         end if;
      end Has_Autoplay;

      function Has_Autoloop return String is
      begin
         if Autoloop then
            return " loop";
         else
            return "";
         end if;
      end Has_Autoloop;

      function Has_Muted return String is
      begin
         if Muted then
            return " muted";
         else
            return "";
         end if;
      end Has_Muted;

      function Has_Source return String is
      begin
         if Source /= "" then
            return Escape_Quotes (" src='" & Source & "'");
         else
            return "";
         end if;
      end Has_Source;
   begin
      Audio.Create_From_HTML (Parent => Parent,
                              HTML   => "<audio" &
                                Has_Controls &
                                Has_Preload &
                                Has_Autoplay &
                                Has_Autoloop &
                                Has_Muted &
                                Has_Source &
                                " />",
                              ID     => ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Video     : in out Video_Type;
                     Parent    : in out Gnoga.Gui.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Poster    : in     String  := "";
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "")
   is
      function Has_Controls return String;
      function Has_Preload return String;
      function Has_Autoplay return String;
      function Has_Autoloop return String;
      function Has_Muted return String;
      function Has_Poster return String;
      function Has_Source return String;

      function Has_Controls return String is
      begin
         if Controls then
            return " controls";
         else
            return "";
         end if;
      end Has_Controls;

      function Has_Preload return String is
      begin
         if Preload then
            return " preload='auto'";
         else
            return "";
         end if;
      end Has_Preload;

      function Has_Autoplay return String is
      begin
         if Autoplay then
            return " autoplay";
         else
            return "";
         end if;
      end Has_Autoplay;

      function Has_Autoloop return String is
      begin
         if Autoloop then
            return " loop";
         else
            return "";
         end if;
      end Has_Autoloop;

      function Has_Muted return String is
      begin
         if Muted then
            return " muted";
         else
            return "";
         end if;
      end Has_Muted;

      function Has_Poster return String is
      begin
         if Source /= "" then
            return Escape_Quotes (" poster='" & Poster & "'");
         else
            return "";
         end if;
      end Has_Poster;

      function Has_Source return String is
      begin
         if Source /= "" then
            return Escape_Quotes (" src='" & Source & "'");
         else
            return "";
         end if;
      end Has_Source;
   begin
      Video.Create_From_HTML (Parent => Parent,
                              HTML   => "<video" &
                                Has_Controls &
                                Has_Preload &
                                Has_Autoplay &
                                Has_Autoloop &
                                Has_Muted &
                                Has_Poster &
                                Has_Source &
                                " />",
                              ID     => ID);
   end Create;

end Ada_GUI.Gnoga.Gui.Element.Multimedia;

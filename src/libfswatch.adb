------------------------------------------------------------------------------
--                                                                          --
--                              Ada_Libfswatch                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with Interfaces.C;           use Interfaces.C;
with Interfaces.C.Strings;

with cmonitor_h;
with libfswatch_h;           use libfswatch_h;
with cevent_h;               use cevent_h;

with Libfswatch.Conversions; use Libfswatch.Conversions;

package body Libfswatch is

   type Root_Event_Monitor_Access is access all Root_Event_Monitor'Class;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Event_Monitor'Class, Root_Event_Monitor_Access);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Root_Event_Monitor_Access);

   ----------------------------
   -- Library initialization --
   ----------------------------

   --  The C library requires a global initialization: handle this here

   C_Lib_Initialized : Boolean := False;

   procedure Initialize_C_Lib;
   --  Call this for any call that requires the library to be initialized.
   --  You may call this any number of times, it won't do anything if the
   --  library has already been initialized.

   function Init_Session return FSW_HANDLE;
   --  Initialize a session with the default monitor, checking for errors

   procedure Low_Level_Callback
     (arg1 : access constant fsw_cevent;
      arg2 : unsigned;
      arg3 : System.Address) with Convention => C;

   procedure Add_Path (Session : FSW_HANDLE; Path : Virtual_File);
   --  Add a path to be monitored to Session

   ----------------------
   -- Initialize_C_Lib --
   ----------------------

   procedure Initialize_C_Lib is
      Init : int;
   begin
      if C_Lib_Initialized then
         return;
      end if;

      C_Lib_Initialized := True;
      Init := libfswatch_h.fsw_init_library;
      if Init /= 0 then
         raise Libfswatch_Error with ("fsw_init_library returned" & Init'Img);
      end if;
   end Initialize_C_Lib;

   ------------------
   -- Init_Session --
   ------------------

   function Init_Session return FSW_HANDLE is
      Session : FSW_HANDLE;
   begin
      Session := fsw_init_session (cmonitor_h.system_default_monitor_type);
      if Session = null then
         raise Libfswatch_Error with "fsw_init_session failed";
      end if;
      return Session;
   end Init_Session;

   -----------------
   -- Event_Image --
   -----------------

   function Event_Image (E : Event) return String is
      Result : Unbounded_String;
   begin
      Result := "Path: " & E.Path & ASCII.LF & "Flags:";
      for Flag of E.Flags loop
         Result := Result & " " & Event_Flags'Image (Flag);
      end loop;
      Result := Result & ASCII.LF;

      return To_String (Result);
   end Event_Image;

   ------------------------
   -- Low_Level_Callback --
   ------------------------

   procedure Low_Level_Callback
     (arg1 : access constant fsw_cevent;
      arg2 : unsigned;
      arg3 : System.Address)
   is
      use System;
      Events  : Event_Vectors.Vector;
      Monitor : Root_Event_Monitor_Access;
   begin
      --  Convert the C array of events to Ada structures
      Events := To_Ada (arg1, arg2);

      if arg3 /= Null_Address then
         Monitor := Convert (arg3);
         Monitor.Callback (Events);
      end if;
   end Low_Level_Callback;

   --------------
   -- Add_Path --
   --------------

   procedure Add_Path (Session : FSW_HANDLE; Path : Virtual_File) is
      Status : FSW_STATUS;
   begin
      Status := fsw_add_path
        (Session,
         Interfaces.C.Strings.New_String (+Path.Full_Name.all));
      if Status /= 0 then
         raise Libfswatch_Error with "fsw_add_path returned" & Status'Img;
      end if;
   end Add_Path;

   ----------------------
   -- Blocking_Monitor --
   ----------------------

   procedure Blocking_Monitor
     (Monitor : in out Root_Event_Monitor'Class;
      Paths   : File_Array)
   is
      Status : FSW_STATUS;
   begin
      --  Initialize the C library
      Initialize_C_Lib;

      --  Initialize a session
      Monitor.Session := Init_Session;

      for Path of Paths loop
         Add_Path (Monitor.Session, Path);
      end loop;

      Status := fsw_set_callback
        (Monitor.Session, Low_Level_Callback'Access,
         Monitor'Address);
      if Status /= 0 then
         raise Libfswatch_Error with "fsw_set_callback returned" & Status'Img;
      end if;

      Status := fsw_start_monitor (Monitor.Session);
      if Status /= 0 then
         raise Libfswatch_Error with "fsw_start_monitor returned" & Status'Img;
      end if;

      --  The call to fsw_start_monitor above is blocking. If we reach this,
      --  this means that we've received fsw_stop_monitor and can cleanup.

      Status := fsw_destroy_session (Monitor.Session);
      if Status /= 0 then
         raise Libfswatch_Error with
           "fsw_destroy_session returned" & Status'Img;
      end if;
   end Blocking_Monitor;

   ------------------
   -- Stop_Monitor --
   ------------------

   procedure Stop_Monitor (Monitor : in out Root_Event_Monitor'Class) is
      Status : FSW_STATUS;
   begin
      Status := fsw_stop_monitor (Monitor.Session);
      if Status /= 0 then
         raise Libfswatch_Error with "fsw_stop_monitor returned" & Status'Img;
      end if;
   end Stop_Monitor;

end Libfswatch;

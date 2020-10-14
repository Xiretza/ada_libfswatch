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

--  Demonstrator for the low-level use of Libfswatch

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with libfswatch_h; use libfswatch_h;
with libfswatch_types_h; use libfswatch_types_h;
with cmonitor_h;
with cevent_h; use cevent_h;
with System; use System;

with Libfswatch;             use Libfswatch;
with Libfswatch.Conversions; use Libfswatch.Conversions;

procedure Basic is
   Init    : int;
   Status  : FSW_STATUS;
   Session : FSW_HANDLE;

   task Stop_After_10_Seconds;
   --  Stop the session after 10 seconds.

   procedure Callback
     (arg1 : access constant fsw_cevent;
      arg2 : unsigned;
      arg3 : System.Address) with Convention => C;
   --  The function which is called when an event arrives

   --------------
   -- Callback --
   --------------

   procedure Callback
     (arg1 : access constant fsw_cevent;
      arg2 : unsigned;
      arg3 : System.Address)
   is
      Events : Event_Vectors.Vector;
   begin
      --  Convert the C array of events to Ada structures
      Events := To_Ada (arg1, arg2);

      --  Simple prints of the events we receive
      for E of Events loop
         Put_Line (Event_Image (E));
      end loop;
   end Callback;

   ---------------------------
   -- Stop_After_10_Seconds --
   ---------------------------

   task body Stop_After_10_Seconds is
   begin
      --  Wait 10 seconds
      delay 10.0;

      --  Stop the monitoring
      Status := fsw_stop_monitor (Session);
      if Status /= 0 then
         Put_Line ("Error when stopping the session");
      end if;
   end Stop_After_10_Seconds;

begin
   --------------------
   -- Initialization --
   --------------------

   --  Initialize the library
   Init := libfswatch_h.fsw_init_library;
   if Init /= 0 then
      Put_Line ("Error when initializing the library");
      return;
   end if;

   --  Initialize the session
   Session := fsw_init_session (cmonitor_h.system_default_monitor_type);
   if Session = null then
      Put_Line ("Error when initializing the session");
      return;
   end if;

   ----------------
   -- Monitoring --
   ----------------

   --  Add the monitoring of the current directory
   Status := fsw_add_path (Session, Interfaces.C.Strings.New_String ("."));
   if Status /= 0 then
      Put_Line ("Error when adding path");
      return;
   end if;

   --  Set a callback to be called whenever an event arrives on any
   --  of the paths being monitored.
   Status := fsw_set_callback
     (Session, Callback'Unrestricted_Access, System.Null_Address);
   if Status /= 0 then
      Put_Line ("Error when setting callback");
      return;
   end if;

   Put_Line ("Listening to events...");
   New_Line;

   --  Start the actual monitoring. This call is blocking - it only returns
   --  when the task calls fsw_stop_monitor.
   Status := fsw_start_monitor (Session);
   if Status /= 0 then
      Put_Line ("Error when starting monitor");
      return;
   end if;

   Put_Line ("Done.");
end Basic;

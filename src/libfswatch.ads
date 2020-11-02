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

--  This package contains an high-level Ada binding to the C library

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with libfswatch_types_h; use libfswatch_types_h;
with cevent_h;           use cevent_h;

package Libfswatch is

   Libfswatch_Error : exception;
   --  Used to report any library exception

   --------------------
   -- Event handling --
   --------------------

   type Event_Flags is
     (No_Op,
      Platform_Specific,
      Created,
      Updated,
      Removed,
      Renamed,
      Owner_Modified,
      Attribute_Modified,
      Moved_From,
      Moved_To,
      Is_File,
      Is_Dir,
      Is_Sym_Link,
      Link,
      Overflow);
   for Event_Flags use (No_Op              => 0,
                        Platform_Specific  => 1,
                        Created            => 2,
                        Updated            => 4,
                        Removed            => 8,
                        Renamed            => 16,
                        Owner_Modified     => 32,
                        Attribute_Modified => 64,
                        Moved_From         => 128,
                        Moved_To           => 256,
                        Is_File            => 512,
                        Is_Dir             => 1024,
                        Is_Sym_Link        => 2048,
                        Link               => 4096,
                        Overflow           => 8192);

   package Event_Flags_Vectors is
     new Ada.Containers.Vectors (Natural, Event_Flags);

   type Event is record
      Path  : Unbounded_String;
      Flags : Event_Flags_Vectors.Vector;
      --  TODO: add a platform-independent time representation
   end record;

   package Event_Vectors is new Ada.Containers.Vectors (Natural, Event);

   function Event_Image (E : Event) return String;
   --  Return a representation of an event as a string, useful for debugging

   type Event_Flags_Array is array (Natural range <>) of Event_Flags;

   ----------------
   -- Monitoring --
   ----------------

   type Root_Event_Monitor is abstract tagged private;
   --  A callback type. Inherit from this and override Callback to define
   --  your own callback.

   procedure Callback (Self   : in out Root_Event_Monitor;
                       Events : Event_Vectors.Vector) is abstract;
   --  Called when events are received on the paths being monitored

   procedure Blocking_Monitor
     (Monitor        : in out Root_Event_Monitor'Class;
      Paths          : File_Array;
      Events_Allowed : Event_Flags_Array := (1 .. 0 => No_Op));
   --  Monitor paths, calling Callback when events are received. This does
   --  not return until interrupted via Stop_Monitor below.
   --  If Events_Allowed is specified, Callback will only be called
   --  for those event types.
   --  TODO: add a timeout parameter?

   procedure Stop_Monitor (Monitor : in out Root_Event_Monitor'Class);
   --  Interrupt the monitoring. This is thread-safe.

private

   type Root_Event_Monitor is abstract tagged record
      Session : FSW_HANDLE;
   end record;

   type Event_Filter is new fsw_event_flag;
   No_Filter : constant Event_Filter := 0;

end Libfswatch;

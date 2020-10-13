------------------------------------------------------------------------------
--                                                                          --
--                              Ada_Libfswatch                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package contains an high-level Ada binding to the C library

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Libfswatch is

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

end Libfswatch;

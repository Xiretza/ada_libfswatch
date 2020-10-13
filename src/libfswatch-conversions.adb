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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Libfswatch.Conversions is

   ------------
   -- To_Ada --
   ------------

   function To_Ada (C : fsw_cevent) return Event is
      Result : Event;

      type Flags_Array is array (1 .. C.flags_num) of fsw_event_flag;
      type Flags_Array_Access is access Flags_Array;
      type event_flag_access is access all fsw_event_flag;
      function Convert is new Ada.Unchecked_Conversion
        (event_flag_access, Flags_Array_Access);
      Flags  : Flags_Array_Access;


   begin
      Result.Path := To_Unbounded_String (Value (C.path));

      Flags := Convert (event_flag_access (C.flags));
      for Flag of Flags.all loop
         Result.Flags.Append (Event_Flags'Enum_Val (Flag));
      end loop;

      return Result;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (c_event_array_start : access constant fsw_cevent;
      c_event_array_size  : unsigned) return Event_Vectors.Vector
   is
      Result : Event_Vectors.Vector;

      type Events_Array is array (1 .. c_event_array_size) of fsw_cevent;
      type Events_Array_Access is access Events_Array;
      type fsw_cevent_access is access constant fsw_cevent;
      function Convert is new Ada.Unchecked_Conversion
        (fsw_cevent_access, Events_Array_Access);

      X      : constant Events_Array_Access :=
                 Convert (fsw_cevent_access (c_event_array_start));
   begin
      for C_Event of X.all loop
         Result.Append (To_Ada (C_Event));
      end loop;

      return Result;
   end To_Ada;

end Libfswatch.Conversions;

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

--  A monitor which monitors one file, reading from it a directory to monitor

with GNATCOLL.VFS; use GNATCOLL.VFS;
with Libfswatch;   use Libfswatch;

package Reloading_Monitor is

   type Monitor is new Root_Event_Monitor with record
      Key_File                 : Virtual_File;
      New_Paths_Read_From_File : File_Array_Access;
   end record;

   overriding procedure Callback (Self   : in out Monitor;
                                  Events : Event_Vectors.Vector);
   --  Monitor the contents of Key_File. When the contents of Key_File
   --  changes, interpret this as a list of paths, and store them
   --  in New_Paths_Read_From_File.

end Reloading_Monitor;

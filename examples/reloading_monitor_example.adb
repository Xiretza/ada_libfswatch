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

--  Demonstrator for an adaptative monitor which:
--   - reads the paths to monitor from a file "paths.txt" (one path per line)
--   - reloads itself when the file changes
--   - quits when the file is deleted

with Ada.Text_IO;          use Ada.Text_IO;
with Libfswatch;           use Libfswatch;
with GNATCOLL.VFS;         use GNATCOLL.VFS;

with Reloading_Monitor;

procedure Reloading_Monitor_Example is

   M : Reloading_Monitor.Monitor;

begin
   M.Key_File := Create ("paths.txt");

   Put_Line ("Monitoring the contents of " &
             (+M.Key_File.Full_Name.all));

   M.New_Paths_Read_From_File := new File_Array'((1 => Create (".")));

   while M.New_Paths_Read_From_File /= null loop
      M.Blocking_Monitor (M.New_Paths_Read_From_File.all);
      --  The call above blocks until the key file has changed - when
      --  this occurs, the call will stop, and the monitor will fill
      --  M.New_Paths_Read_From_File with the contents of the key file.

      if not M.Key_File.Is_Regular_File then
         --  This is our exit condition
         Put_Line ((+M.Key_File.Full_Name.all)
                   & " has disappeared, exitting.");
         exit;
      end if;
   end loop;
end Reloading_Monitor_Example;

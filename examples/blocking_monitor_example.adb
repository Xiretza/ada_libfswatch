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

--  Demonstrator for the use of the Blocking_Monitor API

with Libfswatch;           use Libfswatch;
with GNATCOLL.VFS;         use GNATCOLL.VFS;

with Simple_Print_Monitor; use Simple_Print_Monitor;

procedure Blocking_Monitor_Example is

   --  Declare a simple print monitor
   Printer : Print_Monitor;

   task Stop_After_10_Seconds;
   --  Stop the session after 10 seconds.

   ---------------------------
   -- Stop_After_10_Seconds --
   ---------------------------

   task body Stop_After_10_Seconds is
   begin
      --  Wait 10 seconds then stop monitoring
      delay 10.0;
      Printer.Stop_Monitor;
   end Stop_After_10_Seconds;

begin
   --  Watch "." and "..": the monitor will print any events that might
   --  happen on these directories
   Printer.Blocking_Monitor ((1 => Create ("."),
                              2 => Create ("..")));
end Blocking_Monitor_Example;

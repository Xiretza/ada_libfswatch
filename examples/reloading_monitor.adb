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

with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body Reloading_Monitor is

   --------------
   -- Callback --
   --------------

   overriding procedure Callback (Self   : in out Monitor;
                                  Events : Event_Vectors.Vector)
   is
   begin
      for E of Events loop
         if Create (+To_String (E.Path)) = Self.Key_File then
            --  An event has occurred on the key file

            if E.Flags.Contains (Updated)
              or E.Flags.Contains (Created)
              or E.Flags.Contains (Moved_From)
            then
               Put_Line ("Key file has been modified, reloading. Monitoring:");

               --  Read the contents from file...
               declare
                  Contents : GNAT.Strings.String_Access :=
                    Read_File (Self.Key_File);
                  Splits   : constant Unbounded_String_Array := Split
                    ((if Contents = null then "" else Contents.all), ASCII.LF);
               begin
                  Free (Contents);

                  Unchecked_Free (Self.New_Paths_Read_From_File);
                  Self.New_Paths_Read_From_File := new File_Array
                    (Splits'First .. Splits'Last + 1);

                  for J in Splits'Range loop
                     Self.New_Paths_Read_From_File (J) :=
                       Create (+To_String (Splits (J)));
                     Put_Line ("   " & (To_String (Splits (J))));
                  end loop;

                  Self.New_Paths_Read_From_File
                    (Splits'Last + 1) := Create (".");
               end;

               --  ... and stop the monitoring
               Self.Stop_Monitor;

            elsif E.Flags.Contains (Removed)
              or else E.Flags.Contains (Moved_To)
            then
               --  The file has been removed
               Put_Line ("Key file has been removed, stopping");
               Self.New_Paths_Read_From_File := null;
               Self.Stop_Monitor;
            end if;
         else
            --  One of the other paths has been modified: print it
            Put_Line (Event_Image (E));
         end if;
      end loop;
   end Callback;

end Reloading_Monitor;

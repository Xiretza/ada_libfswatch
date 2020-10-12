pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with libfswatch_h; use libfswatch_h;
with libfswatch_types_h; use libfswatch_types_h;
with cmonitor_h;
with cevent_h; use cevent_h;
with System; use System;

procedure basic is
   res : constant int := libfswatch_h.fsw_init_library;
   Status : FSW_STATUS;
   Session : FSW_HANDLE;

   procedure Callback
     (arg1 : access constant fsw_cevent;
      arg2 : unsigned;
      arg3 : System.Address) with Convention => C;

   procedure Callback
     (arg1 : access constant fsw_cevent;
      arg2 : unsigned;
      arg3 : System.Address) is
   begin
      Put_Line (Arg1.all'Image);
   end Callback;
begin
   --  ??? linux-specific
   Session := fsw_init_session (cmonitor_h.windows_monitor_type);
   Status := fsw_add_path (Session, Interfaces.C.Strings.New_String ("."));
   Status := fsw_set_callback (Session, Callback'Unrestricted_Access,
                               System.Null_Address);
   Status := fsw_start_monitor (Session);
end basic;

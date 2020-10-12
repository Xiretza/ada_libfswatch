package Libfswatch is

   type Abstract_Directory_Callback is abstract tagged null record;

   function Monitor_Dirs (Callback : Abstract_Directory_Callback'Class);

end Libfswatch;

with System;

package MaRTE.Kernel.Application_Scheduler_Task_Body is

   package K renames MaRTE.Kernel;

   -----------------------------------------------
   --  Generic Application Scheduler Task Body  --
   -----------------------------------------------
   function Generic_Body (Sched : System.Address) return System.Address;

end MaRTE.Kernel.Application_Scheduler_Task_Body;

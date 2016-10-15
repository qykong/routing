--
--  Uwe R. Zimmer, Australia, September 2016
--

with Generic_Router;

generic

   with package Router is new Generic_Router (<>);

package Generic_Routers is

   use Router;
   use Message_Structures;
   use Routers_Configuration;

   Router_Tasks  : array (Router_Range) of aliased Router_Task;
   Router_Active : array (Router_Range) of Boolean := (others => True);

   Routers_Configured : Boolean := True; -- potentially set to False if the body does not successfully elaborate

   procedure Router_Shutdown (Id : Router_Range);
   procedure Global_Shutdown;

end Generic_Routers;

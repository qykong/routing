--
--  Uwe R. Zimmer, Australia, September 2011
--

generic

   type Router_Range  is (<>);
   type Router_Task_P is private;

   Null_Reference : Router_Task_P;

package Generic_Router_Links is

   type Router_Ports is record
      Id   : Router_Range;
      Link : Router_Task_P;
   end record;

   type Connected_Router_Ports is array (Positive range <>) of Router_Ports;
   type Ids_To_Links           is array (Router_Range)      of Router_Task_P;

   function To_Router_Ports (Task_Id : Router_Range; Links : Ids_To_Links) return Connected_Router_Ports;

end Generic_Router_Links;

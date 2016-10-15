--
--  Uwe R. Zimmer, Australia, September 2016
--

with Ada.Text_IO; use Ada.Text_IO;
with Topologies;  use Topologies;

package body Generic_Routers is

   procedure Router_Shutdown (Id : Router_Range) is

   begin
      select
         Router_Tasks (Id).Shutdown;
      or
         delay Command_Line_Parameters.Comms_Timeout;
         Put_Line ("Warning: Router" & Router_Range'Image (Id) & " did not respond to Shutdown call -> trying to abort it");
         abort Router_Tasks (Id);
      end select;
      Router_Active (Id) := False;
   end Router_Shutdown;

   procedure Global_Shutdown is

   begin
      for Id in Router_Range loop
         if Router_Active (Id) then
            Router_Shutdown (Id);
         end if;
      end loop;
   end Global_Shutdown;

    use Router_Link;

begin
   for Router_Id in Router_Range loop
      declare
         Links : Ids_To_Links := (others => null);
      begin
         for Potential_Neighbour_Id in Router_Range loop
            if Router_Id /= Potential_Neighbour_Id and then Nodes_Connected (Connection_Topology, Positive (Router_Id), Positive (Potential_Neighbour_Id)) then
               Links (Potential_Neighbour_Id) := Router_Tasks (Potential_Neighbour_Id)'Access;
            end if;
         end loop;
         select
            Router_Tasks (Router_Id).Configure (Links);
         or
            delay Command_Line_Parameters.Comms_Timeout;
            Routers_Configured := False; exit;
         end select;
      end;
   end loop;
end Generic_Routers;

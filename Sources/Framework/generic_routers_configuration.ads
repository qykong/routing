--
--  Uwe R. Zimmer, Australia, September 2011
--

with Routers_Configuration_Structures; use Routers_Configuration_Structures;
with Topologies;                       use Topologies;

generic
   Command_Line_Parameters : Command_Line_Options;

package Generic_Routers_Configuration is

   Connection_Topology : constant Topology_Kind'Class := Get_Topology (Command_Line_Parameters);

   type Router_Range is new Positive range 1 .. Nodes_in_Topology (Connection_Topology);

end Generic_Routers_Configuration;

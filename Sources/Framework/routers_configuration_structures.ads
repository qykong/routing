--
--  Uwe R. Zimmer, Australia, September 2016
--

with Topologies; use Topologies;

package Routers_Configuration_Structures is

   type Preconfigured_Topologies is (Line, Ring, Star, Fully_Connected, Tree, Mesh, Torus, Hypercube, Cube_Connected_Cycles, Butterfly, Wrap_Around_Butterfly);

   type Test_Modes is (One_To_All, All_to_One);

   type Command_Line_Options is record
      Selected_Topology   : Preconfigured_Topologies := Cube_Connected_Cycles;
      Size                : Positive                 := 20;
      Degree              : Positive                 := 3;
      Depths              : Positive                 := 4;
      Dimension           : Positive                 := 3;
      Print_Connections   : Boolean                  := True;
      Print_Distances     : Boolean                  := True;
      Routers_Settle_Time : Duration                 := 0.1;
      Comms_Timeout       : Duration                 := 0.1;
      Test_Mode           : Test_Modes               := One_To_All;
      Dropouts            : Natural                  := 0;
   end record;

   function Get_Topology (Command_Line_Parameters : Command_Line_Options) return Topology_Kind'Class;

end Routers_Configuration_Structures;

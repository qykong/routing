--
--  Uwe R. Zimmer, Australia, September 2016
--

package body Routers_Configuration_Structures is

   function Get_Topology (Command_Line_Parameters : Command_Line_Options) return Topology_Kind'Class is

      CLP : Command_Line_Options renames Command_Line_Parameters;

   begin
      case CLP.Selected_Topology is
         when Line                  => return Line                  (Size => CLP.Size);
         when Ring                  => return Ring                  (Size => CLP.Size);
         when Star                  => return Star                  (Size => CLP.Size);
         when Fully_Connected       => return Fully_Connected       (Size => CLP.Size);
         when Tree                  => return Trees                 (Degree => CLP.Degree, Depths => CLP.Depths);
         when Mesh                  => return Mesh                  (Dimension => CLP.Dimension, Size => CLP.Size);
         when Torus                 => return Torus                 (Dimension => CLP.Dimension, Size => CLP.Size);
         when Hypercube             => return Hypercube             (Dimension => CLP.Dimension);
         when Cube_Connected_Cycles => return Cube_Connected_Cycles (Dimension => CLP.Dimension);
         when Butterfly             => return Butterfly             (Dimension => CLP.Dimension);
         when Wrap_Around_Butterfly => return Wrap_Around_Butterfly (Dimension => CLP.Dimension);
      end case;
   end Get_Topology;

end Routers_Configuration_Structures;

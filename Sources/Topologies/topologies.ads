--
--  Uwe R. Zimmer, Australia, September 2011
--

package Topologies is

   type Topology_Kind is interface;

   function Nodes_in_Topology (Configuration : Topology_Kind) return Positive is abstract;
   function Nodes_Connected   (Configuration : Topology_Kind;
                               Node_A, Node_B : Positive)     return Boolean  is abstract;

   function Min_Degree        (Configuration : Topology_Kind'Class) return Natural;
   function Max_Degree        (Configuration : Topology_Kind'Class) return Natural;

   function Line                  (Size            : Positive) return Topology_Kind'Class;
   function Ring                  (Size            : Positive) return Topology_Kind'Class;
   function Star                  (Size            : Positive) return Topology_Kind'Class;
   function Fully_Connected       (Size            : Positive) return Topology_Kind'Class;
   function Trees                 (Degree, Depths  : Positive) return Topology_Kind'Class;
   function Mesh                  (Dimension, Size : Positive) return Topology_Kind'Class;
   function Torus                 (Dimension, Size : Positive) return Topology_Kind'Class;
   function Hypercube             (Dimension       : Positive) return Topology_Kind'Class;
   function Cube_Connected_Cycles (Dimension       : Positive) return Topology_Kind'Class;
   function Butterfly             (Dimension       : Positive) return Topology_Kind'Class;
   function Wrap_Around_Butterfly (Dimension       : Positive) return Topology_Kind'Class;

end Topologies;

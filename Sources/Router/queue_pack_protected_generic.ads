--
-- Uwe R. Zimmer, Australia, 2014
--

pragma Initialize_Scalars;

generic
   type Element is private;
   type Index   is (<>);  -- Modulo defines size of the queue.

package Queue_Pack_Protected_Generic is

   type Queue_Type is limited private;

   protected type Protected_Queue is

      entry Enqueue (Item :     Element);
      entry Dequeue (Item : out Element);

      procedure Empty_Queue;

      function Is_Empty return Boolean;
      function Is_Full  return Boolean;

   private
      Queue : Queue_Type;

   end Protected_Queue;

private
   type List is array (Index) of Element;
   type Queue_Type is record
      Top, Free : Index   := Index'First;
      Is_Empty  : Boolean := True;
      Elements  : List;
   end record;
end Queue_Pack_Protected_Generic;

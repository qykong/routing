--
-- Uwe R. Zimmer, Australia, 2014
--

package body Queue_Pack_Protected_Generic is

   protected body Protected_Queue is

      function Is_Empty return Boolean is
        (Queue.Is_Empty);

      function Is_Full return Boolean is
        (not Queue.Is_Empty and then Queue.Top = Queue.Free);
      entry Enqueue (Item : Element) when not Is_Full is

      begin
         Queue.Elements (Queue.Free) := Item;
         if Queue.Free = Index'Last then
            Queue.Free := Index'First;
         else
            Queue.Free := Index'Succ (Queue.Free);
         end if;
         Queue.Is_Empty := False;
      end Enqueue;

      entry Dequeue (Item : out Element) when not Is_Empty is

      begin
         Item           := Queue.Elements (Queue.Top);
         if Queue.Top = Index'Last then
            Queue.Top := Index'First;
         else
            Queue.Top := Index'Succ (Queue.Top);
         end if;
         Queue.Is_Empty := Queue.Top = Queue.Free;
      end Dequeue;

      procedure Empty_Queue is

      begin
         Queue.Top      := Index'First;
         Queue.Free     := Index'First;
         Queue.Is_Empty := True;
      end Empty_Queue;

   end Protected_Queue;
end Queue_Pack_Protected_Generic;

--
--  Uwe R. Zimmer, Australia, September 2011
--

package body Generic_Router_Links is

   function To_Router_Ports (Task_Id : Router_Range; Links : Ids_To_Links) return Connected_Router_Ports is

      No_of_Links : Natural := 0;

   begin
      for Router_Id in Router_Range loop
         if Task_Id /= Router_Id and then Links (Router_Id) /= Null_Reference then
            No_of_Links := No_of_Links + 1;
         end if;
      end loop;
      declare
         Channels   : Connected_Router_Ports (1 .. No_of_Links);
         Channel_Ix : Positive range Channels'Range := Channels'First;
      begin
         for Router_Id in Router_Range loop
            if Task_Id /= Router_Id and then Links (Router_Id) /= Null_Reference then
               Channels (Channel_Ix) := (Id => Router_Id, Link => Links (Router_Id));
               if Channel_Ix < Channels'Last then
                  Channel_Ix := Channel_Ix + 1;
               end if;
            end if;
         end loop;
         return Channels;
      end;
   end To_Router_Ports;

end Generic_Router_Links;

--
--  Framework: Uwe R. Zimmer, Australia, 2015
--

with Exceptions; use Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Queue_Pack_Protected_Generic;

package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);
         Router_Range_Array : array (Router_Range) of Router_Range := (others => Router_Range'Invalid_Value);
         package Queue_For_Sending_Initial_Message_Package is new Queue_Pack_Protected_Generic (For_Sending_Initial_Message, Router_Range);
         Station : Queue_For_Sending_Initial_Message_Package.Protected_Queue;
         package Queue_Initial_Message_Package is new Queue_Pack_Protected_Generic (Initial_Message, Router_Range);
         Recieve_Station : Queue_Initial_Message_Package.Protected_Queue;
         package Queue_Inner_Message_Package is new Queue_Pack_Protected_Generic (Inner_Messages, Router_Range);
         Inner_Recieve_Station : Queue_Inner_Message_Package.Protected_Queue;
         Inner_Send_Station : Queue_Inner_Message_Package.Protected_Queue;

         task Sending_Task;

         task body Sending_Task is
            M : For_Sending_Initial_Message;
            M2 : Inner_Messages;
         begin
            loop
               Station.Dequeue (M);
               if M.Finish then
                  exit;
               end if;
               Port_List (M.To).Link.all.Receive_Init_Message (M.M);
            end loop;

            loop
               Inner_Send_Station.Dequeue (M2);
               if M2.Finish then
                  exit;
               end if;
               for s in Port_List'Range loop
                  if Port_List (s).Id = Router_Range_Array (M2.Destination) then
                     --                             Put_Line ("in" & Router_Range'Image (M.Destination) & " " & Router_Range'Image (Port_List (s).Id));
                     Port_List (s).Link.all.Receive_Inner_Message (M2);
                     --                             Put_Line ("out");
                     exit;
                  end if;
               end loop;
            end loop;
         end Sending_Task;

         task Initialisation_Task;
         task body Initialisation_Task is
         begin
            declare
--                 type Wanted_List is array (Router_Range) of Boolean;
--                 Wanted_List_For_Neighbours : array (Port_List'Range) of Wanted_List := (others => (others => False));
               Hops_Array : array (Router_Range) of Positive := (others => Positive'Invalid_Value);
               Current_Round : Positive := Positive'First;
               Finish : Boolean := False;
               Finished_Neighbour : Natural := 0;
               Finished_Neighbour_A : array (Port_List'Range) of Boolean := (others => False);
               No_Need_Neighbour : Natural := 0;
               No_Need_Neighbour_A : array (Port_List'Range) of Positive := (others => 1);
               I_Will_Not_Send_To : array (Port_List'Range) of Boolean := (others => False);
            begin
               Router_Range_Array (Task_Id) := Task_Id;
               Hops_Array (Task_Id) := Positive'Last;
               for I in Port_List'Range loop
                  Router_Range_Array (Port_List (I).Id) := Port_List (I).Id;
                  Hops_Array (Port_List (I).Id) := 1;
               end loop;
               loop
                  Finish := (for all s of Hops_Array => s'Valid);

                  declare
                     New_M : Initial_Message;
                  begin
                     New_M.Round := Current_Round;
                     New_M.Sender := Task_Id;
                     New_M.finish := Finish;
                     for j in Router_Range loop
                        if Hops_Array (j)'Valid and then Hops_Array (j) = Current_Round then
                           New_M.Next_Array (New_M.No_M) := j;
                           New_M.No_M := Router_Range'Succ (New_M.No_M);
                        end if;
                     end loop;
                     for I in Port_List'Range loop
                        if not Finished_Neighbour_A (I) and then (No_Need_Neighbour_A (I) < 3 or else (No_Need_Neighbour_A (I) = 3 and then Finish)) and then not I_Will_Not_Send_To (I) then
                           New_M.See_You := No_Need_Neighbour_A (I) > 1;
                           declare
                              M : For_Sending_Initial_Message;
                           begin
                              M.M := New_M;
                              M.To := I;
                              Station.Enqueue (M);
                           end;
                        end if;
                     end loop;
                  end;

                  for I in Port_List'First .. Port_List'Last - Finished_Neighbour - No_Need_Neighbour loop
                     declare
                        Get_M : Initial_Message;
                        sender_port : Integer;
                     begin
                        loop
--                                               Put_Line (Router_Range'Image (Task_Id) & " " & Positive'Image (Current_Round) & " in ");

                           Recieve_Station.Dequeue (Get_M);
                           --                                               Put_Line (Router_Range'Image (Task_Id) & " " & Positive'Image (Current_Round) & " out" );

                           if Get_M.Round = Current_Round then
                              exit;
                           end if;
                           Recieve_Station.Enqueue (Get_M);
                        end loop;
                        for j in Port_List'Range loop
                           if Port_List (j).Id = Get_M.Sender then
                              sender_port := j;
                              exit;
                           end if;
                        end loop;
                        if Get_M.finish then
                           Finished_Neighbour := Finished_Neighbour + 1;
                           Finished_Neighbour_A (sender_port) := True;
                        end if;
                        if No_Need_Neighbour_A (sender_port) = 2 then
                           No_Need_Neighbour := No_Need_Neighbour + 1;
                           No_Need_Neighbour_A (sender_port) := 3;
                        end if;
                        I_Will_Not_Send_To (sender_port) := Get_M.See_You;
                        declare
                           flag : Boolean := True;
                        begin
                           if Get_M.No_M /= Router_Range'First then
                              for k in Router_Range'First .. Router_Range'Pred (Get_M.No_M) loop
                                 if not Hops_Array (Get_M.Next_Array (k))'Valid then
                                    Router_Range_Array (Get_M.Next_Array (k)) := Get_M.Sender;
                                    Hops_Array (Get_M.Next_Array (k)) := Current_Round + 1;
                                    flag := False;
                                 end if;
                              end loop;
                           end if;
                           if flag then
                              No_Need_Neighbour_A (sender_port) := 2;
                           end if;
                        end;
--                          if Get_M.No_Needs /= Router_Range'Last then
--                             for k in Router_Range'Succ (Get_M.No_Needs) .. Router_Range'Last loop
--                                Wanted_List_For_Neighbours (sender_port) (k) := True;
--                             end loop;
--                          end if;
--                          for k in Router_Range loop
--                             if (not Hops_Array (k)'Valid) and then k /= Task_Id and then Get_M.Next_Array (k)'Valid then
--                                Router_Range_Array (k) := Get_M.Sender;
--                                Hops_Array (k) := Current_Round + 1;
--                             end if;
--                          end loop;
                     end;
                  end loop;

                  Current_Round := Current_Round + 1;

                  if Finish or else Finished_Neighbour = Port_List'Length then
                     exit;
                  end if;
               end loop;
            end;
            declare
               M : For_Sending_Initial_Message;
            begin
               M.Finish := True;
               Station.Enqueue (M);
            end;
            Finish_Init;

         end Initialisation_Task;
      begin
         -- Initialisation stage
         loop
            select
               accept Receive_Init_Message (Message : in Initial_Message) do
                  Recieve_Station.Enqueue (Message);
               end Receive_Init_Message;
            or
               accept Finish_Init;
               exit;
            end select;
         end loop;

         loop
            select
--                 when not Inner_Send_Station.Is_Full =>
                  accept Send_Message (Message : in Messages_Client) do
                     declare
                        M : Inner_Messages;
                     begin
                        M.Sender := Task_Id;
                        M.Destination := Message.Destination;
                        M.The_Message := Message.The_Message;
                        Inner_Send_Station.Enqueue (M);
                     end;
                  end Send_Message;
            or
               accept Receive_Inner_Message (M : in Inner_Messages) do
                  declare
                     M2 : Inner_Messages := M;
                  begin
                     M2.Hop_Counter := M2.Hop_Counter + 1;
                     if M2.Destination = Task_Id then
                        Inner_Recieve_Station.Enqueue (M2);
                     else
                        Inner_Send_Station.Enqueue (M2);
                     end if;
                  end;

               end Receive_Inner_Message;
            or
                 when not Inner_Recieve_Station.Is_Empty =>
               accept Receive_Message (Message : out Messages_Mailbox) do

                  declare
                     M : Inner_Messages;
                  begin
                     Inner_Recieve_Station.Dequeue (M);
                     Message.Sender := M.Sender;
                     Message.The_Message := M.The_Message;
                     Message.Hop_Counter := M.Hop_Counter;
                  end;
               end Receive_Message;
            or
               accept Shutdown;
               declare
                  M : Inner_Messages;
               begin
                  M.Finish := True;
                  Inner_Send_Station.Enqueue (M);
               end;
               exit;
            end select;
         end loop;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;

--
-- Uwe R. Zimmer, Australia 2015
--

package body Id_Dispenser is

   protected Dispenser is

      procedure Draw_Id (Id : out Element);

   private

      Next_Id       : Element := Element'First;
      IDs_exhausted : Boolean := False;

   end Dispenser;

   protected body Dispenser is

      procedure Draw_Id (Id : out Element) is

      begin
         if IDs_exhausted then
            raise Out_Of_Ids;
         else
            Id := Next_Id;
            if Next_Id = Element'Last then
               IDs_exhausted := True;
            else
               Next_Id := Element'Succ (Next_Id);
            end if;
         end if;
      end Draw_Id;

   end Dispenser;

   function Draw_Id return Element is

      Unique_Id : Element := Element'Invalid_Value;

   begin
      Dispenser.Draw_Id (Id => Unique_Id);
      return Unique_Id;
   end Draw_Id;

end Id_Dispenser;

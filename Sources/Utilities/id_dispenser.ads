--
-- Uwe R. Zimmer, Australia 2015
--

generic

   type Element is (<>);

package Id_Dispenser is

   function Draw_Id return Element;

   Out_Of_Ids : exception;

end Id_Dispenser;

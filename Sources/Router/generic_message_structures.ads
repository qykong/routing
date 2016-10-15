--
--  Framework: Uwe R. Zimmer, Australia, 2015
--

with Ada.Strings.Bounded;           use Ada.Strings.Bounded;
with Generic_Routers_Configuration;

generic
   with package Routers_Configuration is new Generic_Routers_Configuration (<>);

package Generic_Message_Structures is

   use Routers_Configuration;

   package Message_Strings is new Generic_Bounded_Length (Max => 80);
   use Message_Strings;

   subtype The_Core_Message is Bounded_String;

   type Messages_Client is record
      Destination : Router_Range;
      The_Message : The_Core_Message;
   end record;

   type Messages_Mailbox is record
      Sender      : Router_Range     := Router_Range'Invalid_Value;
      The_Message : The_Core_Message := Message_Strings.To_Bounded_String ("");
      Hop_Counter : Natural          := 0;
   end record;

   -- Leave anything above this line as it will be used by the testing framework
   -- to communicate with your router.

   --  Add one or multiple more messages formats here ..

   type Routers is array (Router_Range) of Router_Range;

   type Initial_Message is record
      Sender     : Router_Range := Router_Range'Invalid_Value;
      Round      : Positive := Positive'Invalid_Value;
      Next_Array : Routers      := (others => Router_Range'Invalid_Value);
      finish     : Boolean  := False;
      No_M       : Router_Range := Router_Range'First;
      See_You    : Boolean := False;
   end record;

   type For_Sending_Initial_Message is record
      M : Initial_Message;
      To : Positive := Positive'Invalid_Value;
      Finish : Boolean := False;
   end record;

   type Inner_Messages is record
      Destination : Router_Range;
      The_Message : The_Core_Message;
      Hop_Counter : Natural := 0;
      Sender      : Router_Range;
      Finish      : Boolean := False;
   end record;
end Generic_Message_Structures;

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System.Garlic.Thin;

private package System.Garlic.Network_Utilities is

   function Port_To_Network (Port : Interfaces.C.unsigned_short)
     return Interfaces.C.unsigned_short;
   pragma Inline (Port_To_Network);
   --  Convert a port number into a network port number

   function Network_To_Port (Net_Port : Interfaces.C.unsigned_short)
     return Interfaces.C.unsigned_short;
   pragma Inline (Network_To_Port);
   --  Symetric operation

   function To_Sockaddr_Access is
      new Ada.Unchecked_Conversion (Thin.Sockaddr_In_Access,
                                    Thin.Sockaddr_Access);
   --  Useful conversion

   function To_Chars_Ptr is
     new Ada.Unchecked_Conversion (Thin.Int_Access,
                                   Interfaces.C.Strings.chars_ptr);
   --  Likewise

   function To_Chars_Ptr is
      new Ada.Unchecked_Conversion (Address,
                                    Interfaces.C.Strings.chars_ptr);

end System.Garlic.Network_Utilities;

package body AdaBroker.GIOP is

   --------------------------------
   -- Reply_Status_Type_To_C_Int --
   --------------------------------

   function Reply_Status_Type_To_C_Int
     (Status : in Reply_Status_Type)
      return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Reply_Status_Type'Pos (Status));
   end Reply_Status_Type_To_C_Int;

   --------------------------------
   -- C_Int_To_Reply_Status_Type --
   --------------------------------

   function C_Int_To_Reply_Status_Type
     (N : in Interfaces.C.int)
      return Reply_Status_Type is
   begin
      return Reply_Status_Type'Val (Integer (N));
   end C_Int_To_Reply_Status_Type;

end AdaBroker.GIOP;

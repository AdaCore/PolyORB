with PolyORB.If_Descriptors;
with PolyORB.If_Descriptors.CORBA_IR;

procedure IR_AB_Names_Setup is
begin
   PolyORB.If_Descriptors.Default_If_Descriptor
     := new PolyORB.If_Descriptors.CORBA_IR.IR_If_Descriptor;
end IR_AB_Names_Setup;


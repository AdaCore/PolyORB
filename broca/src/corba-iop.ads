with CORBA;

package CORBA.Iop is
   subtype Profile_Id is CORBA.Unsigned_Long;

   Tag_Internet_Iop : constant Profile_Id := 0;
   Tag_Multiple_Components : constant Profile_Id := 1;
end CORBA.Iop;

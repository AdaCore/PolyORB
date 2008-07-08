--  Utility package to store and retrieve IORs

with CORBA.Object;

package IOR_Utils is

   procedure Put_Ref (Filename : String; Ref : CORBA.Object.Ref);
   --  Put Ref in Filename file

   procedure Get_Ref (Filename : String; Ref : in out CORBA.Object.Ref'Class);
   --  Get Ref from Filenamen file

end IOR_Utils;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with CORBA.ORB;
with CosNaming.NamingContext.Helper;
use CosNaming, CosNaming.NamingContext, CosNaming.NamingContext.Helper;

package body Naming_Tools is

   function Split (Path : String) return Name;

   ------------
   -- Locate --
   ------------

   function Locate (IOR_Or_Name : String) return CORBA.Object.Ref is
   begin
      if IOR_Or_Name (IOR_Or_Name'First .. IOR_Or_Name'First + 3) = "IOR:" then
         declare
            Obj : CORBA.Object.Ref;
         begin
            CORBA.ORB.String_To_Object
              (CORBA.To_CORBA_String (IOR_Or_Name), Obj);
            return Obj;
         end;
      end if;

      declare
         RNS  : constant NamingContext.Ref :=
           To_Ref (CORBA.ORB.Resolve_Initial_References
                   (CORBA.ORB.To_Corba_String ("NamingService")));
         N    : constant Name := Split (IOR_Or_Name);
      begin
         return resolve (RNS, N);
      end;
   end Locate;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False)
   is
      RNS : constant NamingContext.Ref :=
        To_Ref (CORBA.ORB.Resolve_Initial_References
                (CORBA.ORB.To_Corba_String ("NamingService")));
      N   : CosNaming.Name;
      NC  : NameComponent;
   begin
      NC.Kind := CosNaming.To_CORBA_String ("");
      NC.Id   := CosNaming.To_CORBA_String (Name);
      Append (N, NC);
      if Rebind then
         NamingContext.rebind (RNS, N, Ref);
      else
         bind (RNS, N, Ref);
      end if;
   end Register;

   -----------
   -- Split --
   -----------

   function Split (Path : String) return Name is
      Slash : constant Natural := Index (Path, "/", Ada.Strings.Backward);
      N     : Name;
      NC    : NameComponent;
   begin
      NC.Kind := To_CORBA_String ("");
      if Slash = 0 then
         NC.Id   := To_CORBA_String (Path);
      else
         N     := Split (Path (Path'First .. Slash - 1));
         NC.Id := To_CORBA_String (Path (Slash + 1 .. Path'Last));
      end if;
      Append (N, NC);
      return N;
   end Split;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Name   : in String)
   is
      RNS : constant NamingContext.Ref :=
        To_Ref (CORBA.ORB.Resolve_Initial_References
                (CORBA.ORB.To_Corba_String ("NamingService")));
      N   : CosNaming.Name;
      NC  : NameComponent;
   begin
      NC.Kind := CosNaming.To_CORBA_String ("");
      NC.Id   := CosNaming.To_CORBA_String (Name);
      Append (N, NC);
      Unbind (RNS, N);
   end Unregister;

end Naming_Tools;

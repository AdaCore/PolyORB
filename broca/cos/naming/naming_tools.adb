with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with CORBA.ORB;
with CosNaming.NamingContext.Helper;
use CosNaming, CosNaming.NamingContext, CosNaming.NamingContext.Helper;

package body Naming_Tools is

   function Split_Path (Path : String) return NameComponent_Array;
   --  Split a sequence of name component specifications separated
   --  with '/' characters into a name component array.

   function Split_Component (Component : String) return NameComponent;
   --  Split a name component specification of the form ID '.' KIND
   --  (where KIND contains no periods) into a NameComponent.

   ------------
   -- Locate --
   ------------

   function Locate
     (Name : NameComponent_Array)
     return CORBA.Object.Ref
   is
      RNS  : constant NamingContext.Ref :=
        To_Ref (CORBA.ORB.Resolve_Initial_References
                (CORBA.ORB.To_Corba_String ("NamingService")));
      N    : constant CosNaming.Name
        := CosNaming.To_Sequence (Name);
   begin
      return resolve (RNS, N);
   end Locate;


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

      return Locate (Split_Path (IOR_Or_Name));
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

   ----------------
   -- Split_Path --
   ----------------

   function Split_Path (Path : String) return NameComponent_Array
   is
      use CosNaming.IDL_SEQUENCE_CosNaming_NameComponent;

      Slash : constant Natural := Index (Path, "/", Ada.Strings.Backward);
      N     : Name;
   begin
      if Slash = 0 then
         return (1 => Split_Component (Path));
      else
         return Split_Path (Path (Path'First .. Slash - 1))
           & Split_Component (Path (Slash + 1 .. Path'Last));
      end if;
   end Split_Path;

   ---------------------
   -- Split_Component --
   ---------------------

   function Split_Component (Component : String) return NameComponent is
      Period : constant Natural := Index (Component, ".", Ada.Strings.Backward);
   begin
      if Period = 0 then
         return NameComponent'
           (Id   => To_CORBA_String (Component),
            Kind => To_CORBA_String (""));
      else
         return NameComponent'
           (Id   => To_CORBA_String
            (Component (Component'First .. Period - 1)),
            Kind => To_CORBA_String
            (Component (Period + 1 .. Component'Last)));
      end if;
   end Split_Component;

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

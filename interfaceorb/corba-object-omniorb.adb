with CORBA.Object;
with AdaBroker;
with AdaBroker.OmniObject;
with AdaBroker.Debug;
pragma Elaborate (AdaBroker.Debug);

use AdaBroker;
use AdaBroker.OmniObject;

package body CORBA.Object.OmniORB is

   Flag : constant Natural
     := AdaBroker.Debug.Is_Active ("corba.object.omnicore");
   procedure O is new AdaBroker.Debug.Output (Flag);

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in CORBA.String;
      To   : out CORBA.Object.Ref'Class)
   is
      OMG_Repository     : CORBA.String;
      Most_Derived_Type : Constant_Ref_Ptr;
   begin
      --  Get the AdaBroker.OmniObject
      To.OmniObj := AdaBroker.OmniObject.String_To_Object (From);

      if To.OmniObj /= null then

         --  Check if the omniobject we got can be put into To (type
         --  implied the repoId)
         OMG_Repository :=
           AdaBroker.OmniObject.Get_Repository_Id (To.OmniObj.all);

         pragma Debug (O ("String_To_Object : repoid = " &
                          CORBA.To_Standard_String (OMG_Repository)));

         Most_Derived_Type :=
           Get_Dynamic_Type_From_Repository_Id (OMG_Repository);

         --  Dyn_type is now an object of the most derived type of the new
         --  created object

         if Is_A (Most_Derived_Type.all, Get_Repository_Id (To)) then
            To.Dynamic_Type := Most_Derived_Type;
            return;
         end if;
      end if;

      --  Otherwise, the operation is illegal return Nil_Ref in the right
      --  class

      To.OmniObj      := null;
      To.Dynamic_Type := null;
   end String_To_Object;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String is
   begin
      if Is_Nil (Obj) then
         return OmniObject.Object_To_String (null);
      else
         return OmniObject.Object_To_String (Obj.OmniObj);
      end if;
   end Object_To_String;

end CORBA.Object.OmniORB;

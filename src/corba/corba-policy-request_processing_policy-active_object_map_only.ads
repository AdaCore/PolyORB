with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.POA_Types;     use CORBA.POA_Types;

package CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only is

   type Active_Map_Only_Policy is new RequestProcessingPolicy with null record;
   type Active_Map_Only_Policy_Access is access all Active_Map_Only_Policy;

   function Create return Active_Map_Only_Policy_Access;

end CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only;

with CORBA.Object;

package CORBA.Object.OmniORB is

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String;
   --  Returns the IOR corresponding to this object it is called by
   --  CORBA.ORB.Object_To_String see CORBA specification for details

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Returns a Ref'Class out of an IOR it is called by
   --  CORBA.ORB.String_To_Object see CORBA specification for details

end CORBA.Object.OmniORB;

------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Name Spaces                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
------------------------------------------------------------------------------




with Droopi.Representations.SOAP;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects;
with Droopi.Requests;
with Sequences.Unbounded;

package Droopi.Representations.SOAP.Name_Spaces is

   use Droopi.Obj_Adapters.Simple;
   use Droopi.Objects;

   NS_Free      : constant Integer := 0;
   Number_Of_NS : constant Integer := 50;
   subtype NS_Index is Integer range 0 .. Number_Of_NS;

   type Namespace_Object is private;
   type Namespace_Object_Access is access all Namespace_Object;

   package Namespace_Seq is new Sequences.Unbounded (Namespace_Object_Access);

   protected type Protected_Namespace_Table is
      procedure Allocate (data : in Namespace_Object_Access;
           idx : out NS_Index);
      procedure Deallocate (idx : in NS_Index);
      function  Instance (idx  : in NS_Index) return Namespace_Object_Access;
      function  Instance (urn  : in XML_String) return Namespace_Object_Access;
   private
      NS        : Namespace_Seq.Sequence  := Namespace_Seq.Null_Sequence;
   end Protected_Namespace_Table;

   NS_Table : Protected_Namespace_Table;

   procedure Add_Element
     (URN   : XML_String := XML_Null_String;
      Element : XML_Component_Access);

   function URN (This : Namespace_Object_Access)
       return XML_String;

   procedure Add_Name
     (Element  : XML_Component_Access;
      Urn    : XML_String);

   procedure Add_Droopi_Method
     (OA     : access Simple_Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id;
      Urn    : XML_String);


private

   type Namespace_Object is record
     URN : XML_String := XML_Null_String;
     Names : Container_Element_Access := null;
     Reference : Integer := 0;
   end record;

end Droopi.Representations.SOAP.Name_Spaces;

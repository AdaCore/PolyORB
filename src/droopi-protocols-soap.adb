
with Droopi.Representations.SOAP;
with Droopi.Representations.SOAP.Any;

with Droopi.Any.NVList;
with Droopi.Types;
with Droopi.Requests;

package body Droopi.Protocols.SOAP  is

   use Droopi.Any;
   use Droopi.Any.NVList;
   use Droopi.Types;
   use Droopi.Representations.SOAP.Any;

   procedure Request_To_SOAP_Method
     (Operation : Droopi.Types.Identifier;
      Args  : Droopi.Any.NVList.Ref;
      Uri : XML_String;
      Method : out XML_Component_Access)
   is
      use Internals;
      use Internals.NV_Sequence;
      List : NV_Sequence_Access;
      Current_Arg  : NamedValue;
      XML_Comp  : XML_Component_Access;

   begin
      Method := new XML_Component;
      Initialize (Method,  Method_Tag_Reference & ":"
         & XML_String (Operation), XML_Null_String,
         Xsd_Struct);

      Add_Attributes (Method, "xmlns:" & Method_Tag_Reference,
                      """" & Uri & """");

      List :=  List_Of (Args);
      for I in 1 ..  Get_Count (Args) loop
         Current_Arg := NV_Sequence.Element_Of (List.all, Positive (I));
         Any_To_XML_Components (Current_Arg.Name,
            Current_Arg.Argument, XML_Comp);
         Add_Child (Method, XML_Comp);
         Set_Parent (XML_Comp, Method);
      end loop;
   end Request_To_SOAP_Method;

   procedure Set_Body
      (Mess : access SOAP_Message;
       Req  : Requests.Request_Access)
   is
      XML_Comp : XML_Component_Access := new XML_Component;
      XML_Comp_Body : XML_Component_Access;
   begin

      Initialize (XML_Comp, SOAP_Tag & Body_Tag, XML_Null_String,
           Xsd_Struct);
      Request_To_SOAP_Method (Req.Operation, Req.Args,
         Mess.NSURN, XML_Comp_Body);

      Add_Child (XML_Comp, XML_Comp_Body);
      Set_Parent (XML_Comp_Body, XML_Comp);

      Mess.Body_Field := XML_Comp_Body;
   end Set_Body;

   procedure Fault
     (Mess : access SOAP_Message;
      Faultcode   : Integer;
      Runcode     : XML_String;
      Faultstring : XML_String := XML_Null_String;
      Detail      : XML_String := XML_Null_String)
   is
      XML_Comp_Body : XML_Component_Access := new XML_Component;
      XML_Comp_Fault : XML_Component_Access := new XML_Component;
      XML_Comp_Faultcode : XML_Component_Access := new XML_Component;
      XML_Comp_Faultruncode : XML_Component_Access := new XML_Component;
      XML_Comp_Faultstring : XML_Component_Access := new XML_Component;
      XML_Comp_Faultdetail : XML_Component_Access := new XML_Component;
   begin
      Initialize (XML_Comp_Body, SOAP_Tag & Body_Tag,
          XML_Null_String, Xsd_Struct);
      Initialize (XML_Comp_Fault, SOAP_Tag & Fault_Tag,
          XML_Null_String, Xsd_Struct);

      Add_Child (XML_Comp_Body, XML_Comp_Fault);
      Set_Parent (XML_Comp_Fault, XML_Comp_Body);

      Initialize (XML_Comp_Faultcode, Faultcode_Tag,
         To_Droopi_String (Integer'Image (Faultcode)), Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultcode);
      Set_Parent (XML_Comp_Faultcode, XML_Comp_Fault);

      Initialize (XML_Comp_Faultruncode, Runcode_Tag, Runcode, Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultruncode);
      Set_Parent (XML_Comp_Faultruncode, XML_Comp_Fault);

      Initialize (XML_Comp_Faultstring, Faultstring_Tag,
           Faultstring, Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultstring);
      Set_Parent (XML_Comp_Faultstring, XML_Comp_Fault);

      Initialize (XML_Comp_Faultdetail, Detail_Tag,
           Detail, Xsd_Simple);
      Add_Child (XML_Comp_Fault, XML_Comp_Faultdetail);
      Set_Parent (XML_Comp_Faultdetail, XML_Comp_Fault);
   end Fault;


   procedure Set_NSURN
      (Mess : access SOAP_Message;
       Urn : XML_String)
   is
   begin
      Mess.NSURN := Urn;
   end Set_NSURN;

   function To_XML_String
     (Mess : access SOAP_Message)
      return XML_String
   is
      S : XML_String := XML_Null_String;
   begin
      Append (S, "<" & SOAP_Tag & Envelope_Tag & " ");
      Append (S, "xmlns:" & SOAP_Tag & "=" & Envelope_Uri);
      Append (S, " " & SOAP_Tag & "=" & Encoding_Style_Uri);
      if Mess.Header_Field /= null then
         Append (S, To_XML_String (Mess.Header_Field));
      end if;

      if Mess.Body_Field /= null then
         Append (S, To_XML_String (Mess.Header_Field));
      end if;

      Append (S, "</" & SOAP_Tag & Envelope_Tag & ">");
      return S;
   end To_XML_String;

end Droopi.Protocols.SOAP;

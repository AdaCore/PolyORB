
with Droopi.Representations.SOAP;
with Droopi.Representations.SOAP.Any;

with Droopi.Any.NVList;
with Droopi.Requests;

package body Droopi.Protocols.SOAP  is

   use Droopi.Any;
   use Droopi.Any.NVList;
   use Droopi.Representations.SOAP.Any;

   procedure Request_To_Soap_Method
     (Req : Requests.Request_Access;
      Uri : XML_String;
      Method : out XML_Component_Access)
   is
      use Internals;
      use Internals.NV_Sequence;
      List : NV_Sequence_Access;
      Arg  : NamedValue;
      XML_Comp  : XML_Component_Access;

   begin
      Method := new XML_Component;
      Initialize_XML_Comp (Method,  Method_Tag_Reference & ":"
         & XML_String (Req.Operation), Xsd_Struct);

      Add_Attributes (Method, "xmlns:" & Method_Tag_Reference,
                      """" & Uri & """");

      List :=  List_Of (Req.Args);
      for I in 1 ..  Get_Count (Req.Args) loop
         Arg := NV_Sequence.Element_Of (List.all, Positive (I));
         Any_To_XML_Components (Arg.Name, Arg.Argument, XML_Comp);
         Add_Child (Method, XML_Comp);
         Set_Parent (XML_Comp, Method);
      end loop;
   end Request_To_Soap_Method;


   procedure Set_Body (Mess : access SOAP_Message;
                      Req  : Requests.Request_Access)
   is
      XML_Comp : XML_Component_Access := new XML_Component;
      XML_Comp_Body : XML_Component_Access;
   begin

      Initialize_XML_Comp (XML_Comp, SOAP_Tag & Body_Tag, Xsd_Struct);
      Request_To_Soap_Method (Req, Mess.Body_NS,
                      XML_Comp_Body);

      Add_Child (XML_Comp, XML_Comp_Body);
      Set_Parent (XML_Comp_Body, XML_Comp);

      Mess.Body_Field := XML_Comp_Body;
   end Set_Body;


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

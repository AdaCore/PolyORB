
------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Protocols                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
------------------------------------------------------------------------------



with Droopi.Representations.SOAP;
--  with Droopi.Any.NVList;
--  with Droopi.Types;


package Droopi.Protocols.SOAP is

   use Droopi.Representations.SOAP;

   type Fault_Component is private;
   type Fault_Component_Access is access all Fault_Component;

   type SOAP_Message is private;
   type SOAP_Message_Access is access all SOAP_Message;

   type SOAP_Session is private;
   type SOAP_Session_Access is access all SOAP_Session;

   procedure Request_To_Soap_Method
     (Req : Requests.Request_Access;
      Uri : XML_String;
      Method : out XML_Component_Access);

   procedure Set_Body (Mess : access SOAP_Message;
                      Req  : Requests.Request_Access);

   function To_XML_String
      (Mess : access SOAP_Message)
      return  XML_String;

private


   type Fault_Component is record
      Faultcode   : XML_String := XML_Null_String;
      Faultstring : XML_String := XML_Null_String;
      Runcode     : XML_String := XML_Null_String;
      Detail      : XML_String := XML_Null_String;
   end record;


   type SOAP_Message is record
      Header_NS     : XML_String := XML_Null_String;
      Header_Field  : XML_Component_Access;
      Body_NS       : XML_String := XML_Null_String;
      Body_Field    : XML_Component_Access;
      Fault         : Fault_Component_Access;
   end record;


   type SOAP_Session is record
      Req : SOAP_Message;
      Resp : SOAP_Message;
   end record;


   SOAP_Tag : constant XML_String := To_Droopi_String
               ("SOAP-ENV:");

   Envelope_Tag : constant XML_String := To_Droopi_String
               ("Envelope");

   Body_Tag : constant XML_String := To_Droopi_String
               ("Body");

   Header_Tag : constant XML_String := To_Droopi_String
               ("Header");

   Encoding_Style_Tag : constant XML_String := To_Droopi_String
               ("encodingStyle");

   Encoding_Style_Uri : constant XML_String := To_Droopi_String
     ("""http://schemas.xmlsoap.org/soap/encoding/""");

   Envelope_Uri : constant XML_String := To_Droopi_String
     ("""http://schemas.xmlsoap.org/soap/envelope/""");

   Method_Tag_Reference : constant XML_String :=
             To_Droopi_String ("m");
   Header_Tag_Reference : constant XML_String :=
             To_Droopi_String ("t");


end Droopi.Protocols.SOAP;

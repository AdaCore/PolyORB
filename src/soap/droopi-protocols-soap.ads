
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
with Droopi.Any.NVList;
with Droopi.Types;


package Droopi.Protocols.SOAP is

   use Droopi.Representations.SOAP;

   Version_Missmatch    : constant Natural;
   Must_Understand      : constant Natural;
   Invalid_Request      : constant Natural;
   Application_Faulted  : constant Natural;

   type SOAP_Message is private;
   type SOAP_Message_Access is access all SOAP_Message;

   type SOAP_Session is private;
   type SOAP_Session_Access is access all SOAP_Session;


   procedure Request_To_SOAP_Method
     (Operation : Droopi.Types.Identifier;
      Args  : Droopi.Any.NVList.Ref;
      Uri : XML_String;
      Method : out XML_Component_Access);


   procedure Set_Body (Mess : access SOAP_Message;
                       Req  : Requests.Request_Access);

   procedure Set_NSURN
      (Mess : access SOAP_Message;
       Urn : XML_String);

   procedure Fault
     (Mess : access SOAP_Message;
      Faultcode   : Integer;
      Runcode     : XML_String;
      Faultstring : XML_String := XML_Null_String;
      Detail      : XML_String := XML_Null_String);

   function To_XML_String
      (Mess : access SOAP_Message)
      return  XML_String;



private

   type SOAP_Message is record
      NSURN         : XML_String := XML_Null_String;
      Header_Field  : XML_Component_Access;
      Body_Field    : XML_Component_Access;
      Body_Fault_Field : XML_Component_Access;
   end record;


   type SOAP_Session is record
      Req : SOAP_Message;
      Resp : SOAP_Message;
   end record;

   Version_Missmatch    : constant Natural := 100;
   Must_Understand      : constant Natural := 200;
   Invalid_Request      : constant Natural := 300;
   Application_Faulted  : constant Natural := 400;

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

   Fault_Tag  : constant XML_String :=
                       To_Droopi_String ("fault");

   Faultcode_Tag  : constant XML_String :=
                       To_Droopi_String ("faultcode");
   Runcode_Tag : constant XML_String :=
                       To_Droopi_String ("runcode");
   Faultstring_Tag : constant XML_String :=
                       To_Droopi_String ("faultstring");
   Detail_Tag : constant XML_String :=
                       To_Droopi_String ("detail");

   Method_Tag_Reference : constant XML_String :=
             To_Droopi_String ("m");
   Header_Tag_Reference : constant XML_String :=
             To_Droopi_String ("t");


end Droopi.Protocols.SOAP;

------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P R O T O C O L S . S O A P                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Representations.SOAP;
with PolyORB.Representations.SOAP.Any;

with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Requests;

package body PolyORB.Protocols.SOAP  is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Types;
   use PolyORB.Representations.SOAP.Any;

   procedure Request_To_SOAP_Method
     (Operation : PolyORB.Types.Identifier;
      Args  : PolyORB.Any.NVList.Ref;
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
         To_PolyORB_String (Integer'Image (Faultcode)), Xsd_Simple);
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

end PolyORB.Protocols.SOAP;

//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.7 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include <adabe.h>

//----------------------------------//
// adabe_attribute::adabe_attribute //
//----------------------------------//

adabe_attribute::adabe_attribute (idl_bool ro,
				  AST_Type *ft,
				  UTL_ScopedName *n,
				  UTL_StrList *p)
  : AST_Attribute (ro, ft, n, p),
    AST_Field (AST_Decl::NT_attr, ft, n, p),
    AST_Decl (AST_Decl::NT_attr, n, p),
    adabe_name (AST_Decl::NT_attr, n, p)
{
  // there's nothing specific to be done
}

//------------------------------//
// adabe_attribute::produce_ads //
//------------------------------//

void
adabe_attribute::produce_ads (dep_list & with,
			      string   & body,
			      string   & previous)
{
  compute_ada_name ();
  D (D_ATTRIBUTE, "produce spec for attribute " + get_ada_local_name ());

  // Function to read attribute.
  body += "   function Get_" + get_ada_local_name () +"\n";
  body += "     (Self : in Ref)\n";
  body += "     return ";
  AST_Decl *d = field_type ();
  string name = dynamic_cast<adabe_name *>(d)->dump_name (with, previous);
  body += name + ";\n\n"; // name is the attribute type

  // If argument not read only, provide a function to assign a value.
  if (!readonly ())
    {
      body += "   procedure Set_" + get_ada_local_name () + "\n";
      body += "     (Self : in Ref;\n";
      body += "      To   : in ";
      body += name;
      body += ");\n\n";
    }
}

//------------------------------//
// adabe_attribute::produce_adb //
//------------------------------//

void
adabe_attribute::produce_adb (dep_list & with,
			      string   & body,
			      string   & previous)
{
  compute_ada_name ();
  D (D_ATTRIBUTE, "produce body for attribute " + get_ada_local_name ());

  // space is computed for the presentation (length of the name).
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name ().length ();i++) space += " ";

  // This string contains all the arguments.
  string in_decls = "";

  // This string contains the in arguments.
  string in_args = "";

  // This string contains the out arguments.
  string out_args = "";

  // Is there no out argument ?
  bool no_out = true;

  string unmarshall = "";

  string tmp = "";
  AST_Decl *d = field_type ();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  string type_name = e->dump_name (with, tmp);

  e->is_marshal_imported (with);

  in_decls +=
    "To : " + type_name;
  
  unmarshall += "            Unmarshall (Stream, "
    + get_ada_local_name () + ");\n";

  with.add ("Broca.Giop");
  with.add ("Broca.Object");
  with.add ("Broca.Marshalling");
  with.add ("Broca.Refs");

  // Create a CORBA string containing the name of the attribute ops.
  string set_attr_name = get_ada_local_name () + "_Set_Attribute";
  body += "   ";
  body += set_attr_name;
  body += 
    " : constant CORBA.Identifier :=\n"
    "     CORBA.To_CORBA_String (\"Set_";
  body += get_ada_local_name ();
  body += "\");\n\n";

  string get_attr_name = get_ada_local_name () + "_Get_Attribute";
  body +=
    "   " + get_attr_name +
    " : constant CORBA.Identifier :=\n"
    "     CORBA.To_CORBA_String (\"Get_";
  body += get_ada_local_name ();
  body += "\");\n\n";

  body +=
    "   procedure Set_" + get_ada_local_name () + "\n"
    "     (Self : in Ref; " + in_decls + ")\n   is\n";

  // Produce now the body of the operation
  adabe_name  *c = dynamic_cast<adabe_name *>(ScopeAsDecl (defined_in ()));

  // This string contains the name of the package in which the
  // operation is defined.
  string name_of_the_package = c->get_ada_local_name ();

  // Then, produce the necessary fields to call the operation.
  // FIXME: name conflict
  body +=
    "      use Broca.Marshalling;\n"
    "      use Broca.Refs;\n"
    "      Handler : Broca.Giop.Request_Handler;\n"
    "      Sr_Res : Broca.Giop.Send_Request_Result_Type;\n"
    "   begin\n"
    "      loop\n"
    "         Broca.Giop.Send_Request_Size\n"
    "           (Handler, Broca.Object.Object_Ptr (Get (Self)),\n"
    "           " + set_attr_name + ");\n"
    "\n"
    "         --  In and inout parameter.\n"
    "         Compute_New_Size (Handler.Buffer, To);\n"
    "\n"
    "         Broca.Giop.Send_Request_Marshall\n"
    "           (Handler, True, " + set_attr_name + ");\n"
    "         Marshall (Handler.Buffer, To);\n"
    "         Broca.Giop.Send_Request_Send\n"
    "           (Handler, Broca.Object.Object_Ptr (Get (Self)),"
    " True, Sr_Res);\n"
    "         case Sr_Res is\n"
    "            when Broca.Giop.Sr_Reply =>\n"
    "               return;\n"
    "            when Broca.Giop.Sr_No_Reply |\n"
    "                 Broca.Giop.Sr_User_Exception =>\n"
    "               raise Program_Error;\n"
    "            when Broca.Giop.Sr_Forward =>\n"
    "               null;\n"
    "         end case;\n"
    "      end loop;\n";
  body += "   end Set_" + get_ada_local_name () + ";\n\n";




  body += "   function Get_" + get_ada_local_name () + "\n";
  body += "     (Self : in Ref) return " + type_name;
  body += "\n   is\n";

  // Then, produce the necessary fields to call the operation.
  // FIXME: name conflict
  body +=
    "      use Broca.Marshalling;\n"
    "      use Broca.Refs;\n"
    "      Handler : Broca.Giop.Request_Handler;\n"
    "      Sr_Res : Broca.Giop.Send_Request_Result_Type;\n"
    "      Result : " + type_name + ";\n"
    "   begin\n"
    "      loop\n"
    "         Broca.Giop.Send_Request_Size\n"
    "           (Handler, Broca.Object.Object_Ptr (Get (Self)),\n"
    "           " + get_attr_name + ");\n"
    "\n"
    "         Broca.Giop.Send_Request_Marshall\n"
    "           (Handler, True"
    ", " + get_attr_name + ");\n"
    "         Broca.Giop.Send_Request_Send\n"
    "           (Handler, Broca.Object.Object_Ptr (Get (Self)), True";
  body += ", Sr_Res);\n"
    "         case Sr_Res is\n"
    "            when Broca.Giop.Sr_Reply =>\n"
    "               --  OD: Operation dependant.\n"
    "               --  Outcoming arguments.\n"
    "               Unmarshall (Handler.Buffer, Result);\n"
    "               return Result;\n"
    "            when Broca.Giop.Sr_No_Reply |\n"
    "                 Broca.Giop.Sr_User_Exception =>\n"
    "               raise Program_Error;\n"
    "            when Broca.Giop.Sr_Forward =>\n"
    "               null;\n"
    "         end case;\n"
    "      end loop;\n"
    "   end Get_" + get_ada_local_name () + ";\n\n";
}

//-----------------------------------//
// adabe_attribute::produce_skel_ads //
//-----------------------------------//

void
adabe_attribute::produce_skel_ads (dep_list & with,
				   string   & body,
				   string   & previous)
{
  compute_ada_name ();
  D (D_ATTRIBUTE, "produce skel spec for attribute " + get_ada_local_name ());

  // To get an attribute (server).
  body +=
    "   function Get_" + get_ada_local_name () +"\n"
    "     (Self : access Object)\n"
    "      return "; 

  AST_Decl *d = field_type ();
  string name = dynamic_cast<adabe_name *>(d)->dump_name (with, previous);
  
  body += name + " is abstract;\n\n";

  // To set an attribute (server).
  if (!readonly ())
    {
      body += 
	"   procedure Set_" + get_ada_local_name () + "\n"
	"     (Self : access Object;\n"
	"      To   : in " + name +
	") is abstract;\n\n";
    }
  
}

//-----------------------------------//
// adabe_attribute::produce_skel_adb //
//-----------------------------------//

void
adabe_attribute::produce_skel_adb (dep_list & with,
				   string   & body,
				   string   & previous)
{
  compute_ada_name ();

  // Full name of the attribute
  string full_name = get_ada_full_name ();

  string in_decls = "";
  string unmarshall = "";

  string tmp = "";
  AST_Decl *d = field_type ();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  string type_name = e->dump_name (with, tmp);

  e->is_marshal_imported (with);

  in_decls +=
    "            " + get_ada_local_name () +
    " : " + type_name + ";\n";
  
  unmarshall += "            Unmarshall (Stream, "
    + get_ada_local_name () + ");\n";

  // Set value
  body +=
    "      if Operation = \"Set_" + get_ada_local_name () + "\" then\n"
    "         declare\n" + 
    in_decls +
    "         begin\n" +
    "            --  Unmarshalls arguments\n" +
    unmarshall +
    "            --  Call implementation\n"
    "            Set_" + get_ada_local_name () +
    " (Object_Ptr (Obj), " + get_ada_local_name () + ");\n";
  
  body +=
    "            Broca.GIOP.Compute_GIOP_Header_Size (Stream);\n"
    "            --  service context\n"
    "            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "            --  Request_id\n"
    "            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "            --  reply_status\n"
    "            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "            Broca.Giop.Marshall_GIOP_Header\n"
    "              (Stream, Broca.Giop.Reply);\n"
    "\n"
    "            --  service context\n"
    "            Marshall (Stream, CORBA.Unsigned_Long (Broca.Giop.No_Context));\n"
    "            --  request id\n"
    "            Marshall (Stream, Request_Id);\n"
    "            --  reply status\n"
    "            Broca.Giop.Marshall (Stream, Broca.Giop.No_Exception);\n"
    "            return;\n"
    "         end;\n"
    "      end if;\n"
    "\n";

  // Get value
  body +=
    "      if Operation = \"Get_" + get_ada_local_name () + "\" then\n"
    "         declare\n" + 
    "            Returns : " + type_name + ";\n"
    "         begin\n" +
    "            --  Call implementation\n"
    "            Returns := Get_" + get_ada_local_name () +
    " (Object_Ptr (Obj));\n";
  
  body +=
    "            Broca.GIOP.Compute_GIOP_Header_Size (Stream);\n"
    "            --  service context\n"
    "            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "            --  Request_id\n"
    "            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "            --  reply_status\n"
    "            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "            --  return value\n"
    "            Compute_New_Size (Stream, Returns);\n";

  body +=
    "            Broca.Giop.Marshall_GIOP_Header\n"
    "              (Stream, Broca.Giop.Reply);\n"
    "\n"
    "            --  service context\n"
    "            Marshall (Stream, CORBA.Unsigned_Long (Broca.Giop.No_Context));\n"
    "            --  request id\n"
    "            Marshall (Stream, Request_Id);\n"
    "            --  reply status\n"
    "            Broca.Giop.Marshall (Stream, Broca.Giop.No_Exception);\n"
    "            --  return value\n"
    "            Marshall (Stream, Returns);\n"
    "            return;\n"
    "         end;\n"
    "      end if;\n"
    "\n";
}

IMPL_NARROW_METHODS1 (adabe_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL (adabe_attribute)
IMPL_NARROW_FROM_SCOPE (adabe_attribute)

//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.5 $
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

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
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

////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_ads (dep_list & with,
			      string   & body,
			      string   & previous)
{
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

////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_adb (dep_list & with,
			      string   & body,
			      string   & previous)
{
  with.add ("AdaBroker.OmniProxyCallWrapper");

  // To get an attribute.
  body += "   function Get_" + get_ada_local_name () +"\n";
  body += "     (Self : in Ref)\n";
  body += "      return "; 
  AST_Decl *d = field_type ();  
  string name = dynamic_cast<adabe_name *>(d)->dump_name (with, previous);
  body += name + "\n   is\n";

  string name_of_the_package =
    dynamic_cast<adabe_name *>(ScopeAsDecl (defined_in ()))->get_ada_full_name ();
  body += "      Operation : " + name_of_the_package;
  body += ".Proxy.Get_" + get_ada_local_name () + "_Proxy;\n";
  body += "   begin \n";
  body += "      " + name_of_the_package + ".Proxy.Init (Operation);\n";
  body += "      AdaBroker.OmniProxyCallWrapper.Invoke (Self, Operation);\n";
  body += "      return " + name_of_the_package;
  body += ".Proxy.Get_Result (Operation);\n";
  body += "   end Get_" + get_ada_local_name () +";\n\n";

  // To set an attribute.
  if (!readonly ())
    {
      body += "   procedure Set_" + get_ada_local_name () + "\n";;
      body += "     (Self : in Ref;\n";
      body += "      To   : in ";
      body += name;
      body += ") is \n";
      body += "      Operation : " + name_of_the_package;
      body += ".Proxy.Set_" + get_ada_local_name () + "_Proxy;\n";
      body += "   begin\n";
      body += "      " + name_of_the_package + ".Proxy.Init\n";
      body += "        (Operation, To);\n";
      body += "      AdaBroker.OmniProxyCallWrapper.Invoke\n";
      body += "        (Self, Operation);\n";
      body += "   end Set_" + get_ada_local_name () + ";\n\n";    
    }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_skel_ads (dep_list & with,
				   string   & body,
				   string   & previous)
{
  // To get an attribute (server).
  body += "   function Get_" + get_ada_local_name () +"\n";
  body += "     (Self : access Object)\n";
  body += "      return "; 
  AST_Decl *d = field_type ();
  string name = dynamic_cast<adabe_name *>(d)->dump_name (with, previous);
  body += name + ";\n\n";

  // To set an attribute (server).
  if (!readonly ())
    {
      body += "   procedure Set_" + get_ada_local_name () + "\n";
      body += "     (Self : access Object;\n";
      body += "      To   : in ";
      body += name;
      body += ");\n\n";
    }
  
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_skel_adb (dep_list & with,
				   string   & body,
				   string   & previous)
{
  body += "   function Get_" + get_ada_local_name () + "\n";
  body += "     (Self : access Object)\n";
  body += "      return ";
  AST_Decl *d = field_type ();
  string name = dynamic_cast<adabe_name *>(d)->dump_name (with, previous);
  body += name + "\n   is\n";
  body += "   begin\n";
  body += "      -- Insert user code\n";
  body += "   end Get_" + get_ada_local_name () + ";\n\n";
  
  if (!readonly ())
    {
      body += "   procedure Set_" + get_ada_local_name () + "\n";
      body += "     (Self : access Object;\n";
      body += "      To   : in ";
      body += name;
      body += ")\n   is\n";
      body += "   begin\n";
      body += "      -- Insert user code\n";
      body += "   end Set_" + get_ada_local_name () + ";\n\n"; 
    } 
}

////////////////////////////////////////////////////////////////////////
////////////////     miscellaneous           ///////////////////////////
////////////////////////////////////////////////////////////////////////
IMPL_NARROW_METHODS1 (adabe_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL (adabe_attribute)
IMPL_NARROW_FROM_SCOPE (adabe_attribute)

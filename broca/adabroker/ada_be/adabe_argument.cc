//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.9 $
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

//--------------------------------//
// adabe_argument::adabe_argument //
//--------------------------------//

adabe_argument::adabe_argument (AST_Argument::Direction   d,
				AST_Type                * ft,
				UTL_ScopedName          * n,
				UTL_StrList             * p)
  : AST_Argument (d, ft, n, p),
    AST_Field (AST_Decl::NT_argument, ft, n, p),
    AST_Decl (AST_Decl::NT_argument, n, p),
    adabe_name (AST_Decl::NT_argument, n, p)
{
}


//-----------------------------//
// adabe_argument::produce_ads //
//-----------------------------//

void
adabe_argument::produce_ads (dep_list & with,
			     string   & body,
			     string   & previous)
  // Produce code for the .ads file. Produce actually for this
  // argument the code needed in a function declaration.
{
  compute_ada_name ();
  D (D_ARGUMENT, "produce spec for argument " + get_ada_local_name ());

  // add the name of the argument to the body
  body += get_ada_local_name () + " : ";

  // add its direction (in, inout or out)
  AST_Argument::Direction dir = direction ();
  switch (dir)
    {
    case dir_IN :
      body += "in ";
      break;
    case dir_OUT :
      body += "out ";
      break;
    case dir_INOUT :
      body += "in out ";
      break;
    }

  // get the type of the argument as an AST_Decl object
  AST_Decl *d = field_type ();

  // add the name of the type to the body
  body +=  dynamic_cast<adabe_name *>(d)->dump_name (with, previous);
}


//-----------------------------//
// adabe_argument::produce_adb //
//-----------------------------//

void
adabe_argument::produce_adb (dep_list & with,
			     bool     & no_out,
			     string     space,
			     string   & in_decls,
			     string   & in_args,
			     string   & out_args,
			     string   & marshall_size,
			     string   & marshall,
			     string   & unmarshall)
  // Actually, it produces several pieces of code and booleans
  //    - the boolean no_out is put to false if the argument is in
  //      out or inout mode
  //    - in_decls contains the field declarations to put in the object
  //      declaration at the end of .adb file
  //    - in_args contains the list of arguments to pass to the Init
  //      function corresponding to the operation
  //    - out_args contains the list of arguments to pass to the Get_Result
  //      function corresponding to the operation
  //    -  space is used for indentation
  //    - align_size contains the code to put in the align_size function
  //    - marshall contains the code to put in the marshall function
  //    - unmarshall_decls contains the list of declarations to put in
  //     the header of the unmarshal_returned_values function
  //    - unmarshall contains the code to put in the
  //      unmarshall_returned_values function
  //    - finalize contains the code to put in the finalize function
  //    - out_args contains the list of arguments to put in the declaration
  //      of the Get_Result function corresponding to the operation
  //    - result_decls contains the code to put in the get_result function
{
  compute_ada_name ();
  D (D_ARGUMENT, "produce body for argument " + get_ada_local_name ());
  
  // get the direction of the argument (in, out or inout)
  AST_Argument::Direction dir = direction ();

  string dir_st = "";
  switch (dir)
    {
    case dir_IN :
      dir_st = "in ";
      break;
    case dir_OUT :
      dir_st = "out ";
      break;
    case dir_INOUT :
      dir_st = "in out ";
      break;
    }

  // Get the type of the argument as an AST_Decl object
  AST_Decl *d = field_type ();
  string junk = "";
  
  // Get the name of the argument via dump_name function
  string type_name = dynamic_cast<adabe_name *>(d)->dump_name (with, junk);
  
  // computation of in_decls string
  in_decls += ";\n      " + get_ada_local_name ();
  in_decls += " : " + dir_st +  type_name;
  
  // computation of in_args string if the argument is in in or inout mode
  if ((dir == AST_Argument::dir_IN) || (dir == AST_Argument::dir_INOUT))
    in_args += ",\n         " + get_ada_local_name ();
    
  // computation of out_args string if the argument is in out or inout mode
  // in this case, put no_out to false
  if ((dir == AST_Argument::dir_OUT) || (dir == AST_Argument::dir_INOUT))
    {
      no_out = false;
      out_args += ",          " + get_ada_local_name ();
    }

  dynamic_cast<adabe_name *>(d)->is_marshal_imported (with);

  string full_type_name = dynamic_cast<adabe_name *>(d)->get_ada_full_name ();
  
  if ((direction () == AST_Argument::dir_IN) ||
      (direction () == AST_Argument::dir_INOUT)) {
    marshall_size += "         Compute_New_Size (Handler.Buffer, ";
    marshall_size += get_ada_local_name ();
    marshall_size += ");\n";

    marshall += "         Marshall (Handler.Buffer, ";
    marshall += get_ada_local_name ();
    marshall += ");\n";
  }

  if ((direction () == AST_Argument::dir_OUT) ||
      (direction () == AST_Argument::dir_INOUT)) {
    // XXX Is this necessary? Thomas 1999-08-05
    // (we already call it right above)
    // dynamic_cast<adabe_name *>(d)->is_marshal_imported (with);
    unmarshall += "            Unmarshall (handler.Buffer, ";
    unmarshall += get_ada_local_name ();
    unmarshall += ");\n";
  }
}

//----------------------------------//
// adabe_argument::produce_skel_adb //
//----------------------------------//

void
adabe_argument::produce_skel_adb (dep_list & with,
				  string   & in_decls,
				  bool     & no_in,
				  bool       no_out,
				  string   & unmarshall,
				  string   & call_args,
				  string   & marshall,
				  string   & align_size)
{
  compute_ada_name ();
  D (D_ARGUMENT, "produce skel body for argument " + get_ada_local_name ());

  string previous = "";
  AST_Decl *d = field_type ();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  string type_name = e->dump_name (with, previous);

  e->is_marshal_imported (with);

  in_decls += "            ";
  in_decls += get_ada_local_name ();
  in_decls += " : ";
  in_decls += type_name;
  in_decls += ";\n";
    
  if ((direction () == AST_Argument::dir_IN) ||
      (direction () == AST_Argument::dir_INOUT))
    {
      no_in = false;
      unmarshall += "            Unmarshall (Stream, ";
      unmarshall += get_ada_local_name ();
      unmarshall += ");\n";
    }

  call_args += ", ";
  call_args += get_ada_local_name ();

  if ((direction () == AST_Argument::dir_OUT)
      || (direction () == AST_Argument::dir_INOUT))
    {
      no_out = false;
      marshall += "            Marshall (Stream, ";
      marshall += get_ada_local_name ();
      marshall += ");\n";
      align_size += "            Size := Compute_New_Size (Stream, ";
      align_size += get_ada_local_name ();
      align_size += ");\n";
    }      
}

IMPL_NARROW_METHODS1 (adabe_argument, AST_Argument)
IMPL_NARROW_FROM_DECL (adabe_argument)

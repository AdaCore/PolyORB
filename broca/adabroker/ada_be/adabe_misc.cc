//  Misc function for ada back-end.
#include "adabe.h"

#include <idl.hh>
#include <string>

void
gen_marshalling_declarations (string &body, const string type_name)
{
  body +=
    "   procedure Marshall\n"
    "      (Stream : in out Broca.Buffers.Buffer_Descriptor;\n"
    "       Val : " + type_name + ");\n"
    "\n"
    "   procedure Unmarshall\n"
    "      (Stream : in out Broca.Buffers.Buffer_Descriptor;\n"
    "       Res : out " + type_name + ");\n"
    "\n"
    "   procedure Compute_New_Size\n"
    "      (Stream : in out Broca.Buffers.Buffer_Descriptor;\n"
    "       Val : " + type_name + ");\n"
    "\n";
}

//-------//
// lower //
//-------//

char * lower (const char * s)
{
  char * r = new char [strlen (s) + 1];
  strcpy (r, s);
  for (unsigned int i = 0; i < strlen (s); i++) 
    {
      if ((s [i] > 64) && (s [i] < 91))
	r [i] = s [i] + 32;
    }
  return r;
}

//------------//
// remove_dot //
//------------//

static string remove_dot (string name)
{
  char c;
  while ((c = name.find (".")) != -1) 
    name[c]='-';
  return name;
}

//--------------//
// produce_file //
//--------------//

void produce_file (string name, source_type type, string code) {
  string n = remove_dot (name);
  
  if ((type == is_skel_spec) || (type == is_skel_body))
    n += "-skel";
  
  if ((type == is_stream_spec) || (type == is_stream_body))
    n += "-stream";
  
  if ((type == is_spec) || (type == is_skel_spec) || (type == is_stream_spec))
     n += ".ads";
  
  if ((type == is_body) || (type == is_skel_body) || (type == is_stream_body))
    n += ".adb";
  
  char * filename = lower (n.c_str());
  ofstream file (filename);
  delete [] filename;
  
  file << code;
  file.close ();
}

void D (unsigned long flag, string message) {
  if (adabe_global::debug_flag (flag)) 
    std::cerr << message << std::endl;
}

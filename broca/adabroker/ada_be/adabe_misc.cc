//  Misc function for ada back-end.

#include <string>

void
gen_marshalling_declarations (string &body, const string type_name)
{
  body +=
    "   procedure Marshall\n"
    "      (Stream : in out Broca.Types.Buffer_Descriptor;\n"
    "       Val : " + type_name + ");\n"
    "\n"
    "   procedure Unmarshall\n"
    "      (Stream : in out Broca.Types.Buffer_Descriptor;\n"
    "       Res : out " + type_name + ");\n"
    "\n"
    "   procedure Marshall_Size\n"
    "      (Stream : in out Broca.Types.Buffer_Descriptor;\n"
    "       Val : " + type_name + ");\n"
    "\n";
}

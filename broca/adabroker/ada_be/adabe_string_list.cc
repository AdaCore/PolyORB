//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.1 $
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
#define DEFAULT_SIZE 4
#include <adabe.h>
#include <string>

adabe_string_list::adabe_string_list () {
  nb_item_in_list = 0;
  max_item_in_list = DEFAULT_SIZE;
  str_list = new str_ptr[max_item_in_list];
}

adabe_string_list::~adabe_string_list () {
  /*  for (int i=0; i < nb_item_in_list; i++)
    delete str_list[i];
    delete str_list;*/
}

bool adabe_string_list::check (string str) {
  int i;
  for (i=0; i < nb_item_in_list; i++) 
    if ( str == *(str_list[i]))
      return true;
  return false;
}

void adabe_string_list::add (string str) 
{
  if (check (str)) 
    return;
  if (nb_item_in_list == max_item_in_list) 
    {
      int i=0;
      string **temp_list;
      temp_list = new str_ptr[max_item_in_list*2];
      for (i=0; i<max_item_in_list; i++)
	{
	  temp_list[i] = str_list[i];
	}
      max_item_in_list *=2;
      delete str_list; 
      str_list = temp_list;
    }
  str_list[nb_item_in_list] = new string (str);
  nb_item_in_list++;
}

string *adabe_string_list::produce () {
  int i;
  string *output;
  output =new string ("");
  for (i = 0; i<nb_item_in_list; i++) {
    (*output) += *str_list[i] +"\n";
  }
  return output;
}

string *adabe_string_list::produce (string repeat) {
  int i;
  string *output;
  output = new string ("");
  for (i = 0; i < nb_item_in_list; i++) {
    (*output) += repeat + *str_list[i] +";"; 
    string substring =str_list[i]->substr (str_list[i]->find_last_of ('.') + 1);
    string lower_string = lower (substring.c_str ());
    //    if ((lower_string ==  "marshal") && (repeat != "use "))      
    //      (*output) += " use " + *str_list[i] +";\n";
    /* else*/  (*output) += "\n";
  }
  return output;
}











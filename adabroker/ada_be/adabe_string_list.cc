#define DEFAULT_SIZE 4
#include <string>
#include <adabe.h>

string_list::string_list() {
  nb_item_in_list = 0;
  max_item_in_list = DEFAULT_SIZE;
  str_list = new str_ptr[max_item_in_list];
}

string_list::~string_list() {
  /*  for (int i=0; i < nb_item_in_list; i++)
    delete str_list[i];
    delete str_list;*/
}

bool string_list::check (string str) {
  int i;
  for (i=0; i < nb_item_in_list; i++) 
    if ( str == *(str_list[i]))
      return true;
  return false;
}

void string_list::add (string str) 
{
  if (check(str)) 
    return;
  if (nb_item_in_list == max_item_in_list) 
    {
      int i=0;
      string **temp_list;
      temp_list = new str_ptr[max_item_in_list*2];
      for (i=0; i<max_item_in_list ; i++)
	{
	  temp_list[i] = str_list[i];
	}
      max_item_in_list *=2;
      delete str_list; 
      str_list = temp_list;
    }
  str_list[nb_item_in_list] = new string(str);
  nb_item_in_list++;
}

string *string_list::produce () {
  int i;
  string *output;
  output =new string("");
  for (i = 0; i<nb_item_in_list; i++) {
    (*output) += *str_list[i] +"\n";
  }
  return output;
}

string *string_list::produce (string repeat) {
  int i;
  string *output;
  output = new string("");
  for (i = 0; i < nb_item_in_list; i++) {
    (*output) += repeat + *str_list[i] +" ;"; 
    string substring =str_list[i]->substr(str_list[i]->find_last_of('.') + 1);
    string lower_string = lower(substring.c_str());
    if (lower_string ==  "marshal")      
      (*output) += " use " + *str_list[i] +" ;\n";
    else  (*output) += "\n";
  }
  return output;
}











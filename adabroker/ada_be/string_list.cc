#define DEFAULT_SIZE 4
#include <string>

string_list::string_list() {
  nb_item_in_list = 0;
  max_item_in_list = DEFAULT_SIZE;
  list = new string[max_item_in_list];
}

string_list::~string_list() {
  delete[] list;
}

bool string_list::check (string str) {
  int i;
  for (i=0; i < nb_item_in_list; i++) 
    if ( str == list[i])
      return true;
  return false;
}

string_list::add (string str) {
  if (nb_item_in_list == max_item_in_list) {
    int i;
    string *temp_list;
    max_item_in_list *=2;
    temp_list = new string[max_item_in_list];
    for (i=0; i<max_item_in_list / 2; i++){
      temp_list[i] = list[i];
    }
    delete list;
    list = temp_list;
  }
  list[nb_item_in_list] = str;
  nb_item_in_list++;
}

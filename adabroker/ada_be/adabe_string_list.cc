/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_string_list.cc                                    ***
***                                                                                            ***
***      This file provides the implementation of class adabe_string_list declared in adabe.h  ***
***   (L 70 ).                                                                                 ***
***      It also contains methods to add and to check the used files in the dep_list.          ***
***                                                                                            ***
***   Copyright 1999                                                                           ***
***   Jean Marie Cottin, Laurent Kubler, Vincent Niebel                                        ***
***                                                                                            ***
***   This is free software; you can redistribute it and/or modify it under terms of the GNU   ***
***   General Public License, as published by the Free Software Foundation.                    ***
***                                                                                            ***
***  This back-end is distributed in the hope that it will be usefull, but WITHOUT ANY         ***
***  WARRANTY; without even the implied waranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR ***
***  PURPOSE.                                                                                  ***
***                                                                                            ***
***  See the GNU General Public License for more details.                                      ***
***                                                                                            ***
***                                                                                            ***
*************************************************************************************************/
#define DEFAULT_SIZE 4
#include <string>
#include <adabe.h>

adabe_string_list::adabe_string_list() {
  nb_item_in_list = 0;
  max_item_in_list = DEFAULT_SIZE;
  str_list = new str_ptr[max_item_in_list];
}

adabe_string_list::~adabe_string_list() {
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

string *adabe_string_list::produce () {
  int i;
  string *output;
  output =new string("");
  for (i = 0; i<nb_item_in_list; i++) {
    (*output) += *str_list[i] +"\n";
  }
  return output;
}

string *adabe_string_list::produce (string repeat) {
  int i;
  string *output;
  output = new string("");
  for (i = 0; i < nb_item_in_list; i++) {
    (*output) += repeat + *str_list[i] +" ;"; 
    string substring =str_list[i]->substr(str_list[i]->find_last_of('.') + 1);
    string lower_string = lower(substring.c_str());
    //    if ((lower_string ==  "marshal") && (repeat != "use "))      
    //      (*output) += " use " + *str_list[i] +" ;\n";
    /* else*/  (*output) += "\n";
  }
  return output;
}











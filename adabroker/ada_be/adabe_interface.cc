// file adabe_interface 

adabe_interface::adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
	       UTL_StrList *p);
//constructor
/*
if (nih == -1) pd_is_forwarded = true else pd_is_forwarded = false;

 */

IMPL_NARROW_METHODS1(adabe_interface, AST_Interface);
IMPL_NARROW_FROM_DECL(adabe_interface);
IMPL_NARROW_FROM_SCOPE(adabe_interface);

void
adabe_interface::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
string Previous = ""
string tmp = ""

with.add("Corba.Object");
if (!pd_is_imported) ada_name.compute();  /////// forwarded then defined
String += "pakage " + get_ada_name() + " is /n"

if (n_inherits()==0) String += "type Ref = new(Corba.Object.ref); /n"



 // forward declarated

if (pd_is_forwarded == true)           
   {
   with.add(get_ada_name()+"_Forward")
   tmp += "package Convert is new " + get_ada_name() + "_Forward.Convert(Ref)"
   if (inherits() == NULL) String += "type Ref = new(Corba.Object.ref); /n"
   }

int i = 0
if (pd_n_inherits > 0)
   {
   adabe_interface inher = inherits()[i];
   String += "type Ref = new(" + inher.get_ada_full_name() + ".Ref)"
   scan de UTL_Scope of inher
       {
       if the node is a type, a constant or an exception
           {
	   cast th type in his real NT
	   tmp += "subtype" +  NT.get_ada_name() + " is " + NT.get_ada_full_name()
	   }
       }   
   }
   i++ ;  
   inher = inherits()[i];
   if (inher==NULL) String += "with NULL record \n"

// multiple inheritance
   
   while (i < pd_n_inherits)
       {
       String += "with record \n"
       String += "Adabroker_father" + int2str(i+1) + " : access " + inher.get_ada_full_name() + ".Ref; \n"
       scan de UTL_Scope of inher
           {
	   if the node is a type, a constant or an exception
               {
	       cast th type in his real NT
	       tmp += "subtype" +  NT.get_ada_name() + " is " + NT.get_ada_full_name() +"\n"
	       }
	   }
       i++ ;
       inher = inherits()[i];
       if (inher==NULL) String += "end record \n" 
       }   
   }
String += tmp;
String += "function To_Ref(The_Ref : in Corba.Object.ref'CLASS) return Ref \n"

// instructions

scan du UTL_Scope de this
   {
   cast du NT en son veritable type et
       {
       string tmp1 = "";
       string tmp2 = "";
       NT.produce_ads(with,tmp1,tmp2);
       String += tmp2 + tmp1;  
       }   
   }
String += "\n end " + get_ada_name() + "\n"    

*/


adabe_interface::produce_adb(dep_list with,string &String, string &previousdefinition);
/*
string Previous = ""
string tmp = ""

with.add("Ada.Tags");
with.add("Ada.exceptions");
with.add("Ada.Omniproxycallwrapper");
with.add("Ada.Proxies");
with.add("Ada.Object");
String += "pakage body" + get_ada_name() + " is /n"

String += "function To_Ref(The_Ref : in Corba.Object.ref'CLASS) return Ref \n"
//////////////////////////// a completer /////////////////////////////////////


// instructions

scan du UTL_Scope de this
   {
   cast du NT en son veritable type et
       {
       string tmp1 = "";
       string tmp2 = "";
       NT.produce_ads(with,tmp1,tmp2);
       String += tmp2 + tmp1;  
       }   
   }
String += "\n end " + get_ada_name() + "\n"    

*/

void
adabe_interface::produce_impl_ads(dep_list with,string &String, string &previousdefinition);
/*
string Previous = ""
string tmp = ""


if (!pd_is_imported) ada_name.compute();  /////// forwarded then defined
String += "pakage " + get_ada_name() + " is /n"

if (n_inherits()==0) String += "type Object = new(Corba.Object.Object); /n"



 // forward declarated

if (pd_is_forwarded == true)           
   {
   with.add(get_ada_name()+"_Forward")
   tmp += "package Convert is new " + get_ada_name() + "_Forward.Convert(Object)"
   if (inherits() == NULL) String += "type Object = new(Corba.Object.Object); /n"
   }

int i = 0
if (pd_n_inherits > 0)
   {
   adabe_interface inher = inherits()[i];
   String += "type Object = new(" + inher.get_ada_full_name() + ".Object)"
   scan de UTL_Scope of inher
       {
       if the node is a type, a constant or an exception
           {
	   cast th type in his real NT
	   tmp += "subtype" +  NT.get_ada_name() + " is " + NT.get_ada_full_name()
	   }
       }   
   }
   i++ ;  
   inher = inherits()[i];
   if (inher==NULL) String += "with NULL record \n"

// multiple inheritance
   
   while (i < pd_n_inherits)
       {
       String += "with record \n"
       String += "Adabroker_father" + int2str(i+1) + " : access " + inher.get_ada_full_name() + ".Object; \n"
       scan de UTL_Scope of inher
           {
	   if the node is a type, a constant or an exception
               {
	       cast th type in his real NT
	       tmp += "subtype" +  NT.get_ada_name() + " is " + NT.get_ada_full_name() +"\n"
	       }
	   }
       i++ ;
       inher = inherits()[i];
       if (inher==NULL) String += "end record \n" 
       }   
   }
String += tmp;


// instructions

scan du UTL_Scope de this
   {
   cast du NT en son veritable type et
       {
       string tmp1 = "";
       string tmp2 = "";
       NT.produce_ads(with,tmp1,tmp2);
       String += tmp2 + tmp1;  
       }   
   }
String += "\n end " + get_ada_name() + "\n"    

*/



//  void produce_adb(std::fstream& s);
//  void produce_impl_ads(std::fstream& s);
//  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);

















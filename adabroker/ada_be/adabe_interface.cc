// file adabe_interface 

adabe_interface::adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
	       UTL_StrList *p);
//constructor


IMPL_NARROW_METHODS1(adabe_interface, AST_Interface);
IMPL_NARROW_FROM_DECL(adabe_interface);
IMPL_NARROW_FROM_SCOPE(adabe_interface);

adabe_interface::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
string Previous = NULL
string tmp = NULL

with.add("Corba.Object");
if (is_defined()==I_TRUE) name = ada_name.compute() else name = get_ada_name();  /////// forwarded then defined
String += "pakage " + name + " is /n"

if (n_inherits()==0) String += "type Ref = new(Corba.Object.ref); /n"



 // forward declarated

if (is_defined()==I_FALSE)           
   {
   with.add(name+"_Forward")
   tmp += "package Convert is new " + name + "_Forward.Convert(Ref)"
   if (inherits()[0]==NULL) String += "type Ref = new(Corba.Object.ref); /n"
   }

int i = 0
adabe_interface inher = inherits()[i];
if (inher!= NULL)
   {
   String += "type Ref = new(" + inher.get_ada_full_name() + ".Ref)"
   scan de UTL_Scope of inher
       {
       if the node is a type, a constant or an exception
           {
	   cast th type in his real NT
	   tmp += "subtype" +  NT.get_ada_name + " is " + NT.get_ada_full_name
	   }
       }   
   }
   i++ ;  
   inher = inherits()[i];
   if (inher==NULL) String += "with NULL record"

// multiple inheritance
   
   while (inher!= NULL)
       {
       String += "with record \n"
       String += "Adabroker_father" + int2str(i+1) + " : access " + inher.get_ada_full_name + ".Ref; \n"
       scan de UTL_Scope of inher
           {
	   if the node is a type, a constant or an exception
               {
	       cast th type in his real NT
	       tmp += "subtype" +  NT.get_ada_name + " is " + NT.get_ada_full_name +"\n"
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
       dep-list with1 = NULL;
       string tmp1 = "";
       string tmp2 = "";
       NT.produce_ads(with1,tmp1,tmp2);
       String += with1.produce() + tmp2 + tmp1;            /////////////////les with se cumule?????????
       }   
   }
String += "\n end " + name + "\n"    

*/




//  void produce_adb(std::fstream& s);
//  void produce_impl_ads(std::fstream& s);
//  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);


















///////////////////////////////////////////////////////////////////////
// Module 	: COS Name Service
// Composant 	: orbTools.h
// Creation 	: 28/05/96
//
// Commentaires : Quelques outils pour l'utilisation d'omniORB2
// MOD1		:
///////////////////////////////////////////////////////////////////////


#ifndef ORBTOOLS_H

#define ORBTOOLS_H 

#include <omniORB2/CORBA.h>
#include "panic.h"


// MACRO pour le catch d'exception dans omniORB2
#define CATCHORB	catch(CORBA::COMM_FAILURE& ex)  {\
				cerr <<"Caught system exception COMM_FAILURE, unable to contact the "\
				         << "object." << endl;\
				fprintf(stderr,"in file %s, around ligne %d \n",__FILE__, __LINE__);\
			}\
  			catch(omniORB::fatalException& ex) {\
			    cerr << "Caught omniORB2 fatalException. This indicates a bug is caught "\
			         << "within omniORB2.\nPlease send a bug report.\n"\
			         << "The exception was thrown in file: " << ex.file() << "\n"\
			         << "                            line: " << ex.line() << "\n"\
			         << "The error message is: " << ex.errmsg() << endl;\
			  }\
		  	catch(...) {\
		    		cerr << "Caught a system exception." << endl;\
		  	}
	




// Permet de recuper un objet corba a partir d'un contexte et d'un nom
//
CORBA::Object_ptr getObjectReference(CORBA::ORB_ptr orb, CosNaming::Name name);



//  Permet d'ajouter au serveur de nom un contexe et un nom d'objet 
//
CORBA::Boolean bindObjectToName(CORBA::ORB_ptr orb, CosNaming::Name context, CosNaming::Name name,
	CORBA::Object_ptr);



#endif 






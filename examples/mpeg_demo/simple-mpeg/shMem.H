///////////////////////////////////////////////////////////////////////
// Module       : polka
// Composant    : shMem.H
// Creation     : 26/06/97
//
// Commentaires :  Classe encapsulant un segment de memoire partage
// MOD1         :
///////////////////////////////////////////////////////////////////////
#ifndef SHMMEM_H
#define SHMMEM_H

#include <iostream.h>
#include "panic.h"


template<class E> class c_shMem {

public:

	// Construction d'un segment de memoire de longeur
	// de "nb"*sizeof(E) avec la clef "key"
        // COMPLEXITE O(1)
		c_shMem(int key, int nb);
		c_shMem(void){};
	
	// Connection au segment de memoire 
        // COMPLEXITE O(1)
		E*  map(void);


	// deconnection du segment
        // COMPLEXITE O(1)
		void unmap(void);
	
	
	// Destruction du segment
        // COMPLEXITE O(1)
		void deleteShMem(void);




protected:
	
	// Adresse de connection
	void* adr;

	// Identifiant systeme pour le segment 
	int msgId;

	// Clef ftok utilisee pour la creation
	// du segment
	int key;
};



#ifdef GCC
#include "shMem.C"
#endif


#endif

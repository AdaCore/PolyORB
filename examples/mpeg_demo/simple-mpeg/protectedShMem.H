///////////////////////////////////////////////////////////////////////
// Module       : polka
// Composant    : protectShMem.H
// Creation     : 26/06/97
//
// Commentaires :  Classe encapsulant un segment de memoire partage
//		   protoge par un semaphore 
// MOD1         :
///////////////////////////////////////////////////////////////////////
#ifndef PROTECTED_SHMMEM_H
#define PROTECTED_SHMMEM_H

#include <iostream.h>
#include "panic.h"
#include "shMem.H"
#include <thread.h>

template<class E> class c_protectedShMem : public c_shMem<E>  {

public:

	// Constructeur 
	// Alloue un segment de "nb" element de typerr "E"
	// avec le clef "key"
	c_protectedShMem(int key, int nb);

	// Initialisation du semaphore
	// COMPLEXITE : o(1)
	void initLock(int valeurInitiale);

	// Attachement du segment de memoire
	// COMPLEXITE : o(1)
	E* map(void);

	// Verouillage du segment
	// COMPLEXITE : o(1)
	void lock(void);


	// Deverouillage du segment
	// COMPLEXITE : o(1)
	void unlock(void);



protected:

	// Mutex pour le verouillage du segment de 
	// memoire partage	
	sema_t* mut;


	// Nombre d element du tableau de memoire partagee
	int nb;

};



#ifdef GCC
#include "protectedShMem.C"
#endif



#endif

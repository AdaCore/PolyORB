///////////////////////////////////////////////////////////////////////
// Module       : polka
// Composant    : varBuffer.H
// Creation     : 02/09/97
//
// Commentaires :  Implantation de buffer pour la realisation de schema
//		   producteur/consomateur
//		   Contrairement a buffer.H, l'ecriture et la lecture 
// 		   ne se fait plus en nombre de bloc mais en element E
//
//
//		   c_vMetaBuffer :  classe abstraite
//		   c_vFreeBuffer :  buffer sans protection par
//				   semaphores. Le buffer est accede en exclusion
//				   mutuelle dans Polka car l'algorithme d'ordo-
//				   nnancement est non-preemtible.
//		   c_vProtecBuffer : buffer protege par semaphores.
//		   c_vShProtecBuffer, c_vShFreeBuffer :
//				   idem que c_freeBuffer et c_protecBuffer
//				   mais les buffers sont dans des segments
//				   de memoires partages 
// MOD1         :
///////////////////////////////////////////////////////////////////////
#ifndef VARBUFFER_H
#define VARBUFFER_H

#include <iostream.h>
#include "protectedShMem.H"


template<class E> class c_vMetaBuffer {

public:


	// Construction du buffer
	// Les parametres sont :
	//	tBuffer : taille totale du buffer en nombre
	//		de <class E>
	c_vMetaBuffer( long tBuffer );


	//	On ecrit dans le buffer
	// 	L ecriture est un peu particulier : en effet,
	//	pour eviter de passer a la methode "write"
	//	les donnees et faire une recopie, l ecriture
	//	consiste uniquement a fournir a processus l'adresse
	//	de la donnee ou il doit ecrire 
	// 	Charge a lui de vraiment faire les ecritures
	//
	//	le parametre nb est le nombre d'element a prendre
	//	en compte
	virtual void writeBuffer(E* dta, long nb)=0;


	// 	Idem pour la lecture 
	virtual void readBuffer(E* dta, long nb)=0;


	// 	Dump a l ecran
	void	dump(void);


protected:


	// Taille du buffer en nombre de <class E>
	long tailleBuffer;


	// Nombre de <class E> actuellement
	// present
	long nbPresent;


	// Pointeur sur le debut du buffer
	E* data;


	// Pointeur courant en sortie
	E* out;
	

	// Pointeur courant en entree
	E* in;



};



template<class E> class c_vProtecBuffer : public c_vMetaBuffer<E> {

public:

	// Construction du buffer
	c_vProtecBuffer( long tBuffer );


	void writeBuffer(E* dta, long nb);
	void readBuffer(E* dta, long nb);


	~c_vProtecBuffer(void);



protected:
	
	// semaphore pour gerer le tampon
	sema_t 	plein;
	sema_t	libre;
	
	
	// Taille des donnees manipulees lors des dernieres
	// requetes
	// Si ces variables sont egales a zero, alors
	// aucune requete n est en attente
	long nbWrite;
	long nbRead;



	// mutex pour les variables
	mutex_t mutex;

	
	// Methodes de support
	void circularWrite(E* dta, long nb);
	void circularRead(E* dta, long nb);


};



#ifdef GCC
#include "varBuffer.C"
#endif



#endif

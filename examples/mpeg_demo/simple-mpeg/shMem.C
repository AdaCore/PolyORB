
///////////////////////////////////////////////////////////////////////
// Module       : polka
// Composant    : shMem.C
// Creation     :  25/06/97
//
// Commentaires : Segment de memoire partage 
// MOD2         : (14/08/97)  Creation de polka_extern.h qui contient
//                            toutes les variables globales de Polka
//                            Suppression des options de compil DEBUG BIGDEBUG et ORDO
//                            de la libPolka.a par des variables. Ceci afin de rendre
//                            l utilisation de polkad plus souple
///////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "shMem.H"





template<class E> c_shMem<E>::c_shMem(int k, int nb)
{

	key=k;


	if ((msgId=shmget(key, sizeof(E)*nb, IPC_CREAT|0666)) < 0)
		PANIC(TRUE,"Pb creation segment memoire \n");
}
		
template<class E> E* c_shMem<E>::map(void)
{


	adr=shmat(msgId,NULL,0);

	if (adr ==(int *) -1)
		PANIC(TRUE,"Pb sur shmat \n");

	return ((E*)adr);
}
 
template<class E> void c_shMem<E>::unmap(void)
{

	if (shmdt((char*)adr) == -1)
		PANIC(TRUE,"Pb sur shmdt \n");

}

template<class E> void c_shMem<E>::deleteShMem(void)
{

	// destruction du segment 
	shmctl(msgId,IPC_RMID,0);
}



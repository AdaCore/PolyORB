///////////////////////////////////////////////////////////////////////
// Module       : polka
// Composant    : ProtectedShMem.C
// Creation     :  25/06/97
//
// Commentaires : Segment de memoire partage proteger par
//		  semaphore  
// MOD2         : (14/08/97)  Creation de polka_extern.h qui contient
//                            toutes les variables globales de Polka
//                            Suppression des options de compil DEBUG BIGDEBUG et ORDO
//                            de la libPolka.a par des variables. Ceci afin de rendre
//                            l utilisation de polkad plus souple
///////////////////////////////////////////////////////////////////////


#include <sys/types.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "protectedShMem.H"




template<class E> c_protectedShMem<E>::c_protectedShMem(int k, int n) 
{
	key=k;	
	nb=n;


       if ((msgId=shmget(key, (sizeof(E)*n)+sizeof(sema_t), IPC_CREAT|0666)) < 0)
                PANIC(TRUE,"Pb creation segment memoire \n");
}


template<class E> E* c_protectedShMem<E>::map(void)
{

        adr=shmat(msgId,NULL,0);

        if (adr ==(int *) -1)
                PANIC(TRUE,"Pb sur shmat \n");

	mut=(sema_t*) ( ((char*)(adr))+(nb*sizeof(E)) );

        return ((E*)adr);
}


template<class E> void  c_protectedShMem<E>::initLock(int val)
{
        if(sema_init(mut,val,USYNC_PROCESS,NULL)!=0)
                PANIC(TRUE,"Pb mutex_init");
}


template<class E> void c_protectedShMem<E>::lock(void)
{
       	if (sema_wait(mut)!=0)
                PANIC(TRUE,"pb lock\n");

}



template<class E> void c_protectedShMem<E>::unlock(void)
{

       	if (sema_post(mut)!=0)
                PANIC(TRUE,"pb unlock  \n");
}





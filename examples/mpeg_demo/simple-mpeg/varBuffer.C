///////////////////////////////////////////////////////////////////////
// Module       : polka
// Composant    : varBuffer.C
// Creation     : 02/08/97
//
// Commentaires :  Implantation de buffer pour la realisation de schema
//		   producteur/consomateur
// MOD1         :
///////////////////////////////////////////////////////////////////////

#include "varBuffer.H"



template<class E> c_vMetaBuffer<E>::c_vMetaBuffer(long t)
{


	tailleBuffer=t;


	// Allocation du tableau
	data = new E [t];
	if (data == NULL)
		PANIC(FALSE,"Pb alloc data\n");

	out=data;
	in=data;
	nbPresent=0;
	

}


template<class E> void c_vMetaBuffer<E>::dump(void)
{

	cout<<"DUMP c_vMetaBuffer : "<<endl;
	cout<<" Taille du Buffer en nombre de <class E> = "<<tailleBuffer<<endl;
	cout<<" Nombre d'information presents = "<<nbPresent<<endl;
	cout<<endl;

	cout<<" Adresse debut buffer = "<<data<<endl;
	cout<<" Position pour la sortie "<<out<<endl;
	cout<<" Position pour l'entree "<<in<<endl;
	cout<<endl<<endl;
}



template<class E> c_vProtecBuffer<E>::c_vProtecBuffer(long t)
		:  c_vMetaBuffer<E>(t)
{


	// On initialise les semaphores
        if(sema_init(&libre,0,USYNC_THREAD,NULL)!=0)
                PANIC(TRUE,"Pb sema_init libre");

        if(sema_init(&plein,0,USYNC_THREAD,NULL)!=0)
                PANIC(TRUE,"Pb sema_init plein");

        if(mutex_init(&mutex,USYNC_THREAD,NULL)!=0)
                PANIC(TRUE,"Pb mutex_init mutex");
	

	nbRead=0;
	nbWrite=0;
}





template<class E> c_vProtecBuffer<E>::~c_vProtecBuffer(void)
{

	sema_destroy(&plein);
	sema_destroy(&libre);
	mutex_destroy(&mutex);
	delete data;
}




template<class E> void c_vProtecBuffer<E>::writeBuffer(E* dta, long nb)
{

	if (mutex_lock(&mutex)!=0)
		PANIC(TRUE,"pb lock\n");

	if((tailleBuffer - nbPresent)>=nb)
		{
		circularWrite(dta,nb);
	
		// On reveille la potentielle lecture en cours
        	if(nbRead>0)
                	if(nbPresent >= nbRead)
                        	{
                        	nbRead=0;
                        	if(sema_post(&libre)!=0)
                                	PANIC(TRUE,"pb lock\n");
                        	}

		}

	else	{
		nbWrite=nb;
		
		if (mutex_unlock(&mutex)!=0)
			PANIC(TRUE,"pb unlock\n");

		if(sema_wait(&plein)!=0)
			PANIC(TRUE,"pb lock\n");

		
		if (mutex_lock(&mutex)!=0)
			PANIC(TRUE,"pb lock\n");

		circularWrite(dta,nb);

		// On reveille la potentielle lecture en cours
        	if(nbRead>0)
                	if(nbPresent >= nbRead)
                        	{
                        	nbRead=0;
                        	if(sema_post(&libre)!=0)
                                	PANIC(TRUE,"pb lock\n");
                        	}

		}

	

	if (mutex_unlock(&mutex)!=0)
		PANIC(TRUE,"pb unlock\n");


}




template<class E> void c_vProtecBuffer<E>::readBuffer(E* dta, long nb)
{


	if (mutex_lock(&mutex)!=0)
		PANIC(TRUE,"pb lock\n");

	if(nbPresent>=nb)
		{
		circularRead(dta,nb);
	
		// On reveille la potentielle ecriture en cours
		if(nbWrite>0)
			if ((tailleBuffer - nbPresent) >= nbWrite)
				{		
				nbWrite=0;
				if(sema_post(&plein)!=0)
					PANIC(TRUE,"pb lock\n");
				}
		}

	else	{
		nbRead=nb;
		
		if (mutex_unlock(&mutex)!=0)
			PANIC(TRUE,"pb unlock\n");

		if(sema_wait(&libre)!=0)
			PANIC(TRUE,"pb lock\n");

		
		if (mutex_lock(&mutex)!=0)
			PANIC(TRUE,"pb lock\n");

		circularRead(dta,nb);

		// On reveille la potentielle ecriture en cours
		if(nbWrite>0)
			if ((tailleBuffer - nbPresent) >= nbWrite)
				{		
				nbWrite=0;
				if(sema_post(&plein)!=0)
					PANIC(TRUE,"pb lock\n");
				}

		}

	

	if (mutex_unlock(&mutex)!=0)
		PANIC(TRUE,"pb unlock\n");

}






template<class E> void c_vProtecBuffer<E>::circularRead(E* dta, long nb)
{

	// on regarde si l'on peut faire un memcpy ?
	if ( out+nb < data+tailleBuffer)
		{
		memcpy(dta,out,nb);	
		out=out+nb;
		}

	else	{
        	for(int j=0;j<nb;j++)
                	{
                	*dta=*out;
                	if(out==(data+tailleBuffer))
                        	out=data;
                	else    out++;
                	dta++;
                	}
		}

	// On a un bloc de base de  moins
	nbPresent=nbPresent-nb;

}



template<class E> void c_vProtecBuffer<E>::circularWrite(E* dta, long nb)
{
	if ( in+nb < data+tailleBuffer)
		{
		memcpy(in,dta,nb);	
		in=in+nb;
		}
	else	{
        	for(int j=0;j<nb;j++)
                	{
                	*in=*dta;
                	if(in==(data+tailleBuffer))
                        	in=data;
                	else    in++;
                	dta++;
                	}
                }
	
	// On a un bloc de plus
	nbPresent=nbPresent+nb;

}


#ifndef PANIC_H
#define PANIC_H

#include <stdio.h>  
#include <errno.h>  
#include <stdlib.h>
#include <string.h>
#include "bool.h"


//
// Arret de l application
//
#define PANIC(diag,msg) \
	{  \
	fprintf(stdout,"PANIC : file %s, line %d >>> %s",  \
	__FILE__, __LINE__, msg);  \
 	if (diag==TRUE)  \
		{\
		fprintf(stdout,"(errno %d) ",errno);\
		fprintf(stdout,"%s\n ",strerror(errno));\
		}\
	exit(0);   \
	}  


//
// Affichage d'une anomalie non bloquante
//
#define ERROR(diag,msg) \
	{  \
	fprintf(stdout,"ERROR : file %s, line %d >>> %s",  \
	__FILE__, __LINE__, msg);  \
 	if (diag==TRUE)  \
		{\
		fprintf(stdout,"(errno %d) ",errno);\
		fprintf(stdout,"%s\n ",strerror(errno));\
		}\
	}  

#endif

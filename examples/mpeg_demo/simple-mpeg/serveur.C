
#include <iostream.h>
#include "dominante.hh"
#include "imageMpegLocal.H"
#include "orbTools.H"


extern "C"
{
  void* imageDecoder(void* arg);
};





int main(int argc, char **argv)
{


  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");



  try {

 
	//
	// Creation de l'objet
	//
  	imageMpegLocal_impl *obj = new 	imageMpegLocal_impl("mpegDecoder");
  	if (obj==NULL)
		PANIC(FALSE,"obj == nil\n");

  	obj->_obj_is_ready(boa);



        CosNaming::Name objectName;
        CosNaming::Name contextName;

        contextName.length(1);
        contextName[0].id   = (const char*) "mpeg";
        contextName[0].kind = (const char*) "context";

        objectName.length(1);

        objectName[0].id   = (const char*) "dominante";
        objectName[0].kind = (const char*) "Object";

        {
        mpeg::mpegDecoder_var ref = obj->_this();
        if (!bindObjectToName(orb,contextName,objectName,ref)) 
                return 1; 
        }



	// On cree la thread qui va afficher les donnees
	//
	thread_t tid;
	if  ((thr_create(NULL,0,imageDecoder,obj,THR_DETACHED|THR_BOUND|THR_NEW_LWP,&tid))!= 0)
                PANIC(TRUE,"Pb creation thread \n");



  	boa->impl_is_ready();
  
  	return 0;

   }
   CATCHORB;


}




// La thread qui decode les donnees mpeg
//
void* imageDecoder(void* arg)
{

    try {

	imageMpegLocal_impl* iml=(imageMpegLocal_impl*)arg;


        long cont=1;

        while(cont)
                {
                cont=iml->displayFrame();
                }



        exit(0);
        return 0;
    }

  CATCHORB;


}












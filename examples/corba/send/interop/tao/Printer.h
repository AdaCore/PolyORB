
#ifndef PRINTER_H
#define PRINTER_H
#include "ace/pre.h"

#include "printS.h"

#if defined (_MSC_VER)
# if (_MSC_VER >= 1200)
#  pragma warning(push)
# endif /* _MSC_VER >= 1200 */
# pragma warning (disable:4250)
#endif /* _MSC_VER */

class Printer
  : public virtual POA_Test::Printer
  , public virtual PortableServer::RefCountServantBase
{
public:
  /// Constructor
  Printer (CORBA::ORB_ptr orb,
	   int instance);
         

  // = The skeleton methods
  virtual void printLong (CORBA::Long K ACE_ENV_ARG_DECL)
    ACE_THROW_SPEC ((CORBA::SystemException));

  virtual void printString (const char * Mesg ACE_ENV_ARG_DECL)
    ACE_THROW_SPEC ((CORBA::SystemException));

  virtual CORBA::Long echoLong (CORBA::Long K ACE_ENV_ARG_DECL)
    ACE_THROW_SPEC ((CORBA::SystemException));

  virtual char* echoString (const char * Mesg ACE_ENV_ARG_DECL)
    ACE_THROW_SPEC ((CORBA::SystemException));

private:
  /// Use an ORB reference to convert strings to objects and shutdown
  /// the application.
  CORBA::ORB_var orb_;

  /// The instance number. (Useful when debugging multiple servants)
  int instance_;
};

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
# pragma warning(pop)
#endif /* _MSC_VER */

#include "ace/post.h"
#endif /* PRINTER_H */

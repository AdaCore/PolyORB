//--------------------------------------------------------------------------//
//                                                                          //
//                           POLYORB COMPONENTS                             //
//                                                                          //
//                         b e n c h s S e r v e r                          //
//                                                                          //
//                                 J A V A                                  //
//                                                                          //
//           Copyright (C) 2009, Free Software Foundation, Inc.             //
//                                                                          //
// PolyORB is free software; you  can  redistribute  it and/or modify it    //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. PolyORB is distributed  in the hope that it will be  useful,    //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with PolyORB; see file COPYING. If    //
// not, write to the Free Software Foundation, 51 Franklin Street, Fifth    //
// Floor, Boston, MA 02111-1301, USA.                                       //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//                  PolyORB is maintained by AdaCore                        //
//                     (email: sales@adacore.com)                           //
//                                                                          //
//--------------------------------------------------------------------------//
import org.omg.CORBA.*;
import org.omg.PortableServer.*;

class benchsImpl extends benchsPOA {
  private ORB orb;
  private short data = 123;

  public void setORB (ORB orb_val) {
    orb = orb_val;
  }

  public short noParameter () {
    return data;
  }

  public void azerty () {
  }

  public boolean echoBoolean (boolean arg) {
    return arg;
  }

  public short echoShort (short arg) {
    return arg;
  }

  public int echoLong (int arg) {
    return arg;
  }

  public float echoFloat (float arg) {
    return arg;
  }

  public double echoDouble (double arg) {
    return arg;
  }

  public char echoChar (char arg) {
    return arg;
  }

  public char echoWChar (char arg) {
    return arg;
  }

  public String echoString (String arg) {
    return arg;
  }

  public String echoWstring (String arg) {
    return arg;
  }

  public benchsPackage.Color echoColor (benchsPackage.Color arg) {
    return arg;
  }

  public benchsPackage.Color[] echoRainbow (benchsPackage.Color[] arg) {
    return arg;
  }

  public benchsPackage.myUnion echoUnion (benchsPackage.myUnion arg) {
    return arg;
  }

  public benchsPackage.myUnionEnumSwitch echoUnionEnumSwitch (benchsPackage.myUnionEnumSwitch arg) {
    return arg;
  }

  public benchsPackage.simple_struct echoStruct (benchsPackage.simple_struct arg) {
    return arg;
  }

  public benchsPackage.array_struct echoArrayStruct (benchsPackage.array_struct arg) {
    return arg;
  }

  public int[][] echoSixteenKb (int[][] arg) {
    return arg;
  }

  public benchsPackage.nested_struct echoNestedStruct (benchsPackage.nested_struct arg) {
    return arg;
  }

  public short[] echoUsequence (short[] arg) {
    return arg;
  }

  public void StopServer () {
    orb.shutdown(false);
  }
}

public class benchsServer {

  public static void main (String args[]) {
    try {
      ORB orb = ORB.init (args, null);
      POA poa = POAHelper.narrow (orb.resolve_initial_references ("RootPOA"));
      poa.the_POAManager ().activate ();

      benchsImpl impl = new benchsImpl ();
      impl.setORB (orb);
      benchs ref = benchsHelper.narrow (poa.servant_to_reference (impl));

      System.out.println (orb.object_to_string (ref));
      System.out.println ("benchServer ready and waiting...");

      orb.run ();

    } catch (Exception e) {
      System.err.println ("ERROR: " + e);
      e.printStackTrace (System.out);
    }

    System.out.println ("benchServer Exiting...");
  }

}

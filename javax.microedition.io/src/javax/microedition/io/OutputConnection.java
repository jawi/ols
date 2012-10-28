package javax.microedition.io;


import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;


/**
 * @since CLDC 1.0
 */
public interface OutputConnection extends Connection
{
  /**
   * @throws IOException
   */
  DataOutputStream openDataOutputStream() throws IOException;

  /**
   * @throws IOException
   */
  OutputStream openOutputStream() throws IOException;
}

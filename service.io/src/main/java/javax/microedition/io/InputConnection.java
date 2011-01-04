package javax.microedition.io;


import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;


/**
 * @since CLDC 1.0
 */
public interface InputConnection extends Connection
{
  /**
   * @throws IOException
   */
  DataInputStream openDataInputStream() throws IOException;

  /**
   * @throws IOException
   */
  InputStream openInputStream() throws IOException;
}

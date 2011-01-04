package javax.microedition.io;


import java.io.IOException;


/**
 * @since CLDC 1.0
 */
public interface Connection
{
  /**
   * @throws IOException
   */
  void close() throws IOException;
}

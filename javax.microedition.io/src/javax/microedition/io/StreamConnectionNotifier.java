package javax.microedition.io;


import java.io.IOException;


/**
 * @since CLDC 1.0
 */
public interface StreamConnectionNotifier extends Connection
{
  /**
   * @throws IOException
   */
  StreamConnection acceptAndOpen() throws IOException;
}

package javax.microedition.io;


import java.io.IOException;


/**
 * @since CLDC 1.0
 */
public interface DatagramConnection extends Connection
{
  /**
   * @throws IOException
   */
  int getMaximumLength() throws IOException;

  /**
   * @throws IOException
   */
  int getNominalLength() throws IOException;

  /**
   * @throws IOException
   * @throws IllegalArgumentException
   */
  Datagram newDatagram( byte[] buf, int size ) throws IOException;

  /**
   * @throws IOException
   * @throws IllegalArgumentException
   */
  Datagram newDatagram( byte[] buf, int size, String addr ) throws IOException;

  /**
   * @throws IOException
   * @throws IllegalArgumentException
   */
  Datagram newDatagram( int size ) throws IOException;

  /**
   * @throws IOException
   * @throws IllegalArgumentException
   */
  Datagram newDatagram( int size, String addr ) throws IOException;

  /**
   * @throws IOException
   * @throws InterruptedIOException
   */
  void receive( Datagram dgram ) throws IOException;

  /**
   * @throws IOException
   * @throws InterruptedIOException
   */
  void send( Datagram arg0 ) throws IOException;
}

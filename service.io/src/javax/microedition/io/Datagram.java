package javax.microedition.io;


import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;


/**
 * @since CLDC 1.0
 */
public interface Datagram extends DataInput, DataOutput
{
  String getAddress();

  byte[] getData();

  int getLength();

  int getOffset();

  void reset();

  /**
   * @throws IllegalArgumentException
   */
  void setAddress( Datagram reference );

  /**
   * @throws IllegalArgumentException
   * @throws IOException
   */
  void setAddress( String addr ) throws IOException;

  /**
   * @throws IllegalArgumentException
   */
  void setData( byte[] buffer, int offset, int len );

  /**
   * @throws IllegalArgumentException
   */
  void setLength( int len );
}

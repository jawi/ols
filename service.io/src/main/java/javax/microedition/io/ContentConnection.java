package javax.microedition.io;


/**
 * @since CLDC 1.0
 */
public interface ContentConnection extends StreamConnection
{
  String getEncoding();

  long getLength();

  String getType();
}

package javax.microedition.io;


import java.io.IOException;


/**
 * @since CLDC 1.0
 */
public class ConnectionNotFoundException extends IOException
{
  private static final long serialVersionUID = 8617995896221113348L;

  public ConnectionNotFoundException()
  {
    super();
  }

  public ConnectionNotFoundException( final String s )
  {
    super( s );
  }
}

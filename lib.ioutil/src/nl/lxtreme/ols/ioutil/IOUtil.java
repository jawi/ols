/*
 * OpenBench LogicSniffer / SUMP project 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
 *
 * Copyright (C) 2006-2010 Michael Poppitz, www.sump.org
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.ioutil;


import java.io.*;


/**
 * 
 */
public class IOUtil
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link IOUtil} instance.
   */
  private IOUtil()
  {
    // Nop
  }

  // METHODS

  /**
   * Closes a given resource.
   * <p>
   * If the given resource also implements the {@link Flushable} interface, the
   * resource is flushed before being closed.
   * </p>
   * 
   * @param aResource
   *          the resource to close, can be <code>null</code>, it might already
   *          be closed.
   * @return <code>true</code> if the close operation succeeded,
   *         <code>false</code> if it is unsure whether it succeeded.
   */
  public static final boolean closeResource( final Closeable aResource )
  {
    boolean result = false;
    if ( aResource != null )
    {
      try
      {
        if ( aResource instanceof Flushable )
        {
          ( ( Flushable )aResource ).flush();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }
      finally
      {
        try
        {
          aResource.close();
          // Indicate success...
          result = true;
        }
        catch ( IOException exception )
        {
          // Ignore...
        }
      }

    }
    return result;
  }

  /**
   * Flushes the given input stream by reading as many bytes as there are still
   * available.
   * 
   * @param aResource
   *          the resource to flush, can be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems/
   */
  public static final void flushInputStream( final InputStream aResource ) throws IOException
  {
    if ( aResource != null )
    {
      while ( aResource.read() >= 0 )
      {
      }
    }
  }

  /**
   * This method calls Thread.currentThread().interrupt() if any exception in
   * the hierarchy (including all parent causes) is either an
   * {@link InterruptedIOException} or {@link InterruptedException}. This method
   * should be called in every catch(IOException), catch(Exception) or
   * catch(Throwable) block.
   * 
   * @param aThrowable
   *          the exception to be checked for interruption. Does nothing if
   *          <code>null</code>.
   * @return <code>true</code> if the exception is "handled" by this method,
   *         <code>false</code> otherwise.
   */
  public static boolean handleInterruptedException( final Throwable aThrowable )
  {
    if ( aThrowable == null )
    {
      return true;
    }

    Throwable current = aThrowable;
    do
    {
      if ( ( current instanceof InterruptedIOException ) || ( current instanceof InterruptedException ) )
      {
        Thread.currentThread().interrupt();
        return true;
      }
      current = current.getCause();
    }
    while ( current != null );

    return false;
  }

}

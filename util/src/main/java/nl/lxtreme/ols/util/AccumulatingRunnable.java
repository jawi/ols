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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.util;


import java.util.*;

import javax.swing.*;


/**
 * Shameless copy from <tt>sun.swing.AccumulatingRunnable</tt>.
 * 
 * @param <T>
 *          the type this {@code Runnable} accumulates
 */
public abstract class AccumulatingRunnable<T> implements Runnable
{
  // VARIABLES

  private Deque<T> arguments = null;

  // METHODS

  /**
   * appends arguments and sends this {@cod Runnable} for the execution if
   * needed.
   * <p>
   * This implementation uses {@see #submit} to send this {@code Runnable} for
   * execution.
   * 
   * @param args
   *          the arguments to accumulate
   */
  public final synchronized void add( final T... args )
  {
    boolean isSubmitted = true;
    if ( this.arguments == null )
    {
      isSubmitted = false;
      this.arguments = new LinkedList<T>();
    }
    Collections.addAll( this.arguments, args );
    if ( !isSubmitted )
    {
      submit();
    }
  }

  /**
   * {@inheritDoc}
   * <p>
   * This implementation calls {@code run(List<T> args)} mehtod with the list of
   * accumulated arguments.
   */
  public final void run()
  {
    run( flush() );
  }

  /**
   * Equivalent to {@code Runnable.run} method with the accumulated arguments to
   * process.
   * 
   * @param aArguments
   *          the accumulated arguments to process.
   */
  protected abstract void run( Deque<T> aArguments );

  /**
   * Returns accumulated arguments and flashes the arguments storage.
   * 
   * @return accumulated arguments
   */
  private synchronized Deque<T> flush()
  {
    Deque<T> list = this.arguments;
    this.arguments = null;
    return list;
  }

  /**
   * Sends this {@code Runnable} for the execution
   * <p>
   * This method is to be executed only from {@code add} method.
   * <p>
   * This implementation uses {@code SwingWorker.invokeLater}.
   */
  private void submit()
  {
    SwingUtilities.invokeLater( this );
  }
}

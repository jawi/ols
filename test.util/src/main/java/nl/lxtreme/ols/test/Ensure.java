/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package nl.lxtreme.ols.test;


import static org.junit.Assert.*;

import java.io.*;


/**
 * Helper class to make sure that steps in a test happen in the correct order.
 * Instantiate this class and subsequently invoke <code>step(nr)</code> with
 * steps starting at 1. You can also have threads wait until you arrive at a
 * certain step.
 * <p>
 * Taken from Apache Felix Dependency Manager. (C) Copyright 2010-11 Apache
 * Felix Project.
 * </p>
 */
public class Ensure
{
  // CONSTANTS

  private static final int RESOLUTION = 100;

  private static long instance = 0;

  // VARIABLES

  private final boolean debug;
  private int step = 0;
  private PrintStream outStream = System.out;

  // CONSTRUCTORS

  /**
   * Creates a new Ensure object.
   */
  public Ensure()
  {
    this( false /* aDebug */ );
  }

  /**
   * Creates a new Ensure object.
   *
   * @param aDebug
   *          <code>true</code> to output more information about the steps this
   *          class enters/waits for, <code>false</code> otherwise.
   */
  public Ensure( final boolean aDebug )
  {
    this.debug = aDebug;
    instance++;
  }

  // METHODS

  /**
   * Factory method to create a runnable step that steps to the given number.
   *
   * @param aEnsure
   *          the ensure class to use for setting the step;
   * @param aNr
   *          the number to set upon invoking the returned runnable.
   * @return a runnable setting the ensured step.
   */
  public static Runnable createRunnableStep( final Ensure aEnsure, final int aNr )
  {
    return new Runnable()
    {
      public void run()
      {
        aEnsure.step( aNr );
      }
    };
  }

  /**
   * Sets the output stream for printing the results of steps to.
   *
   * @param aOutput
   *          the print stream to set, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given stream was <code>null</code>.
   */
  public synchronized void setStream( final PrintStream aOutput )
  {
    if ( aOutput == null )
    {
      throw new IllegalArgumentException( "Print stream cannot be null!" );
    }
    this.outStream = aOutput;
  }

  /**
   * Mark this point as the next step.
   */
  public synchronized void step()
  {
    this.step++;
    if ( this.debug )
    {
      this.outStream.println( "[Ensure " + instance + "] next step " + this.step );
    }
    notifyAll();
  }

  /**
   * Mark this point as step <code>nr</code>.
   *
   * @param aNr
   *          the step we are in.
   */
  public synchronized void step( final int aNr )
  {
    this.step++;
    assertEquals( aNr, this.step );
    if ( this.debug )
    {
      this.outStream.println( "[Ensure " + instance + "] step " + this.step );
    }
    notifyAll();
  }

  /**
   * Wait until we arrive at least at step <code>nr</code> in the process, or
   * fail if that takes more than <code>timeout</code> milliseconds. If you
   * invoke wait on a thread, you are effectively assuming some other thread
   * will invoke the <code>step(nr)</code> method.
   *
   * @param aNr
   *          the step to wait for
   * @param aTimeout
   *          the number of milliseconds to wait
   * @throws IllegalStateException
   *           in case current step isn't the step we expected.
   */
  public synchronized void waitForStep( final int aNr, final int aTimeout )
  {
    final int initialTimeout = aTimeout;

    if ( this.debug )
    {
      this.outStream.println( "[Ensure " + instance + "] waiting for step " + aNr );
    }

    int timeout = aTimeout;
    while ( ( this.step < aNr ) && ( timeout > 0 ) )
    {
      try
      {
        wait( RESOLUTION );
        timeout -= RESOLUTION;
      }
      catch ( final InterruptedException exception )
      {
        // Make sure to update the current Thread's administration correctly!
        Thread.currentThread().interrupt();
      }
    }

    if ( this.step < aNr )
    {
      throw new IllegalStateException(
          "Timed out waiting for " + initialTimeout + " ms for step " + aNr + ", we are still at step " + this.step );
    }

    if ( this.debug )
    {
      this.outStream.println( "[Ensure " + instance + "] arrived at step " + aNr );
    }
  }
}

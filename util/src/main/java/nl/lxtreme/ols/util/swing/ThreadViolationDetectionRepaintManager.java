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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing;


import java.lang.ref.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;


/**
 * ThreadViolationDetectionRepaintManager provides a custom repaint manager that
 * can be used to determine whether all Swing components are correctly created
 * on the EDT.
 * <p>
 * Code originates from the SwingHelper project, for more information see:
 * {@link "https://swinghelper.dev.java.net/"}.
 * </p>
 */
public final class ThreadViolationDetectionRepaintManager extends RepaintManager
{
  // VARIABLES

  private final boolean completeCheck;
  /** The last component. */
  private WeakReference<JComponent> lastComponent;

  // CONSTRUCTORS

  /**
   * Creates a new ThreadViolationDetectionRepaintManager instance.
   */
  public ThreadViolationDetectionRepaintManager()
  {
    this( true );
  }

  /**
   * Creates a new ThreadViolationDetectionRepaintManager instance.
   * 
   * @param aCompleteCheck
   *          <code>true</code> to perform a full check on all stack frames,
   *          <code>false</code> to only at the first few.
   */
  public ThreadViolationDetectionRepaintManager( final boolean aCompleteCheck )
  {
    this.completeCheck = aCompleteCheck;
  }

  // METHODS

  /**
   * Creates a new <code>{@link ThreadViolationDetectionRepaintManager}</code>
   * and sets it as the current repaint manager.
   * <p>
   * On Sun JVMs, this method will install the new repaint manager the first
   * time only. Once installed, subsequent calls to this method will not install
   * new repaint managers. This optimization may not work on non-Sun JVMs, since
   * we use reflection to check if a {@code CheckThreadViolationRepaintManager}
   * is already installed.
   * </p>
   * 
   * @return the created (and installed) repaint manager.
   * @see RepaintManager#setCurrentManager(RepaintManager)
   */
  public static ThreadViolationDetectionRepaintManager install()
  {
    final Object m = currentRepaintManager();
    if ( m instanceof ThreadViolationDetectionRepaintManager )
    {
      return ( ThreadViolationDetectionRepaintManager )m;
    }
    return installNew();
  }

  /**
   * Returns the current repaint manager.
   * 
   * @return a repaint manager, can be <code>null</code>.
   */
  private static Object currentRepaintManager()
  {
    try
    {
      final java.lang.reflect.Method method = SwingUtilities.class.getMethod( "appContextGet", Object.class );
      method.setAccessible( true );
      return method.invoke( null, RepaintManager.class );
    }
    catch ( final Exception exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      HostUtils.handleInterruptedException( exception );

      return null;
    }
  }

  /**
   * Installs the ThreadViolationDetectionRepaintManager as current repaint
   * manager.
   * 
   * @return the installed repaint manager, never <code>null</code>.
   */
  private static ThreadViolationDetectionRepaintManager installNew()
  {
    final ThreadViolationDetectionRepaintManager manager = new ThreadViolationDetectionRepaintManager();
    System.err.println( "Installing custom repaint manager..." );
    setCurrentManager( manager );
    return manager;
  }

  /**
   * @see javax.swing.RepaintManager#addDirtyRegion(javax.swing.JComponent, int,
   *      int, int, int)
   */
  @Override
  public void addDirtyRegion( final JComponent aComponent, final int aX, final int aY, final int aWidth,
      final int aHeight )
  {
    checkThreadViolations( aComponent );
    super.addDirtyRegion( aComponent, aX, aY, aWidth, aHeight );
  }

  /**
   * @see javax.swing.RepaintManager#addInvalidComponent(javax.swing.JComponent)
   */
  @Override
  public synchronized void addInvalidComponent( final JComponent aComponent )
  {
    checkThreadViolations( aComponent );
    super.addInvalidComponent( aComponent );
  }

  /**
   * Check thread violations.
   * 
   * @param aComponent
   *          the c
   */
  private void checkThreadViolations( final JComponent aComponent )
  {
    if ( !SwingUtilities.isEventDispatchThread() && ( this.completeCheck || aComponent.isShowing() ) )
    {
      boolean repaint = false;
      boolean fromSwing = false;
      boolean imageUpdate = false;

      final StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
      for ( StackTraceElement st : stackTrace )
      {
        // for details see
        // <https://swinghelper.dev.java.net/issues/show_bug.cgi?id=1>
        final String className = st.getClassName();
        if ( repaint && className.startsWith( "javax.swing." ) && !className.startsWith( "javax.swing.SwingWorker" ) )
        {
          fromSwing = true;
        }
        if ( repaint && "imageUpdate".equals( st.getMethodName() ) )
        {
          imageUpdate = true;
        }
        if ( "repaint".equals( st.getMethodName() ) )
        {
          repaint = true;
          fromSwing = false;
        }
      }

      if ( imageUpdate )
      {
        // assuming it is java.awt.image.ImageObserver.imageUpdate(...)
        // image was asynchronously updated, that's ok
        return;
      }

      if ( repaint && !fromSwing )
      {
        // no problems here, since repaint() is thread safe
        return;
      }

      // ignore the last processed component
      if ( ( this.lastComponent != null ) && ( aComponent == this.lastComponent.get() ) )
      {
        return;
      }

      this.lastComponent = new WeakReference<JComponent>( aComponent );

      System.err.println( "\nEDT violation detected!\n" );
      System.err.println( "Component: " + aComponent );
      System.err.println( "Trace:" );
      for ( StackTraceElement stackTraceElement : stackTrace )
      {
        System.err.println( "\t" + stackTraceElement.toString() );
      }
    }
  }

}

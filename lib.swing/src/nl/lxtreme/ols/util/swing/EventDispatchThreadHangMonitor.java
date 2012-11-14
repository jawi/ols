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
package nl.lxtreme.ols.util.swing;


import java.awt.*;
import java.io.*;
import java.util.*;


/**
 * Monitors the AWT event dispatch thread for events that take longer than a
 * certain time to be dispatched. The principle is to record the time at which
 * we start processing an event, and have another thread check frequently to see
 * if we're still processing. If the other thread notices that we've been
 * processing a single event for too long, it prints a stack trace showing what
 * the event dispatch thread is doing, and continues to time it until it finally
 * finishes. This is useful in determining what code is causing your Java
 * application's GUI to be unresponsive.
 * 
 * @author Elliott Hughes <enh@jessies.org>
 */
public final class EventDispatchThreadHangMonitor extends EventQueue
{
  // INNER TYPES

  private class HangChecker extends TimerTask
  {
    @Override
    public void run()
    {
      // Synchronize on the outer class, because that's where all
      // the state lives.
      synchronized ( INSTANCE )
      {
        checkForHang();
      }
    }

    private void checkForHang()
    {
      if ( EventDispatchThreadHangMonitor.this.startedLastEventDispatchAt == NO_CURRENT_EVENT )
      {
        // We don't destroy the timer when there's nothing happening
        // because it would mean a lot more work on every single AWT
        // event that gets dispatched.
        return;
      }
      if ( timeSoFar() > UNREASONABLE_DISPATCH_DURATION_MS )
      {
        reportHang();
      }
    }

    private void printStackTrace( final PrintStream out, final StackTraceElement[] stackTrace )
    {
      // We know that it's not interesting to show any code above where
      // we get involved in event dispatch, so we stop printing the stack
      // trace when we get as far back as our code.
      final String ourEventQueueClassName = EventDispatchThreadHangMonitor.class.getName();
      for ( StackTraceElement stackTraceElement : stackTrace )
      {
        if ( stackTraceElement.getClassName().equals( ourEventQueueClassName ) )
        {
          return;
        }
        out.println( "    " + stackTraceElement );
      }
    }

    private void reportHang()
    {
      if ( EventDispatchThreadHangMonitor.this.reportedHang )
      {
        // Don't keep reporting the same hang every 100 ms.
        return;
      }

      EventDispatchThreadHangMonitor.this.reportedHang = true;
      System.out.println( "--- event dispatch thread stuck processing event for " + timeSoFar() + " ms:" );
      StackTraceElement[] stackTrace = EventDispatchThreadHangMonitor.this.eventDispatchThread.getStackTrace();
      printStackTrace( System.out, stackTrace );
    }
  }

  // CONSTANTS

  private static final EventQueue INSTANCE = new EventDispatchThreadHangMonitor();

  // Time to wait between checks that the event dispatch thread isn't hung.
  private static final long CHECK_INTERVAL_MS = 100;

  // Maximum time we won't warn about.
  private static final long UNREASONABLE_DISPATCH_DURATION_MS = 500;

  // VARIABLES

  // Used as the value of startedLastEventDispatchAt when we're not in
  // the middle of event dispatch.
  private static final long NO_CURRENT_EVENT = 0;

  // When we started dispatching the current event, in milliseconds.
  private long startedLastEventDispatchAt = NO_CURRENT_EVENT;

  // Have we already dumped a stack trace for the current event dispatch?
  private boolean reportedHang = false;

  // The event dispatch thread, for the purpose of getting stack traces.
  private Thread eventDispatchThread = null;

  // CONSTRUCTORS

  /**
   * Creates a new {@link EventDispatchThreadHangMonitor} instance.
   */
  private EventDispatchThreadHangMonitor()
  {
    initTimer();
  }

  // METHODS

  /**
   * Sets up hang detection for the event dispatch thread.
   */
  public static void install()
  {
    Toolkit.getDefaultToolkit().getSystemEventQueue().push( INSTANCE );
  }

  /**
   * Overrides EventQueue.dispatchEvent to call our pre and post hooks either
   * side of the system's event dispatch code.
   */
  @Override
  protected void dispatchEvent( final AWTEvent event )
  {
    preDispatchEvent();
    super.dispatchEvent( event );
    postDispatchEvent();
  }

  /**
   * Sets up a timer to check for hangs frequently.
   */
  private void initTimer()
  {
    final long initialDelayMs = 0;
    final boolean isDaemon = true;
    Timer timer = new Timer( "EventDispatchThreadHangMonitor", isDaemon );
    timer.schedule( new HangChecker(), initialDelayMs, CHECK_INTERVAL_MS );
  }

  /**
   * Reports the end of any ongoing hang, and notes that we're no longer
   * processing an event.
   */
  private synchronized void postDispatchEvent()
  {
    if ( this.reportedHang )
    {
      System.out.println( "--- event dispatch thread unstuck after " + timeSoFar() + " ms." );
    }
    this.startedLastEventDispatchAt = NO_CURRENT_EVENT;
  }

  /**
   * Stores the time at which we started processing the current event.
   */
  private synchronized void preDispatchEvent()
  {
    if ( this.eventDispatchThread == null )
    {
      // I don't know of any API for getting the event dispatch thread,
      // but we can assume that it's the current thread if we're in the
      // middle of dispatching an AWT event...
      this.eventDispatchThread = Thread.currentThread();
    }

    this.reportedHang = false;
    this.startedLastEventDispatchAt = System.currentTimeMillis();
  }

  /**
   * Returns how long we've been processing the current event (in milliseconds).
   */
  private long timeSoFar()
  {
    long currentTime = System.currentTimeMillis();
    return ( currentTime - this.startedLastEventDispatchAt );
  }
}

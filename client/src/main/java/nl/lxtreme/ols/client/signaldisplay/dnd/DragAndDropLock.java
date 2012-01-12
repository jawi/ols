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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.dnd;


import java.util.concurrent.atomic.*;


/**
 * Provides a convenience lock mechanism for keeping the correct administration
 * in the various drag-and-drop routines.
 */
public final class DragAndDropLock
{
  // CONSTANTS

  private static AtomicReference<Object> lockedMonitor = new AtomicReference<Object>( null );

  // METHODS

  /**
   * Returns whether the given monitor has the exclusive DnD-lock.
   * 
   * @return <code>true</code> if the given monitor has the exclusive DnD-lock,
   *         <code>false</code> otherwise.
   */
  public static boolean isLocked( final Object aMonitor )
  {
    return lockedMonitor.compareAndSet( aMonitor, aMonitor );
  }

  /**
   * Tries to obtain the exclusive DnD-lock.
   * 
   * @param aMonitor
   *          the monitor for the DnD-lock, cannot be <code>null</code>.
   * @return <code>true</code> if the lock was obtained successfully by the
   *         given monitor, <code>false</code> otherwise.
   */
  public static boolean obtainLock( final Object aMonitor )
  {
    return lockedMonitor.compareAndSet( null, aMonitor );
  }

  /**
   * Releases the exclusive DnD-lock, if held by the given monitor.
   * 
   * @param aMonitor
   *          the monitor for the DnD-lock, cannot be <code>null</code>.
   * @return <code>true</code> if the lock was released successfully for the
   *         given monitor, <code>false</code> otherwise.
   */
  public static boolean releaseLock( final Object aMonitor )
  {
    return lockedMonitor.compareAndSet( aMonitor, null );
  }
}

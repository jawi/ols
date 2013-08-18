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
package nl.lxtreme.ols.api.acquisition;


/**
 * Denotes the status of an acquisition.
 */
public final class AcquisitionResultStatus
{
  // INNER TYPES

  /**
   * Denotes the how the acquisition ended.
   */
  public static enum ResultStatus
  {
    NORMAL, ABORTED, FAILED;
  }

  // VARIABLES

  private final ResultStatus status;
  private final String message;

  // CONSTRUCTORS

  /**
   * Creates a new AcquisitionResultStatus instance.
   * 
   * @param aStatus
   *          the status of the acquisition result, cannot be <code>null</code>.
   */
  public AcquisitionResultStatus( final ResultStatus aStatus )
  {
    this( aStatus, null /* aMessage */);
  }

  /**
   * Creates a new AcquisitionResultStatus instance.
   * 
   * @param aStatus
   *          the status of the acquisition result, cannot be <code>null</code>;
   * @param aMessage
   *          the message of the acquisition result, may be <code>null</code>.
   */
  public AcquisitionResultStatus( final ResultStatus aStatus, final String aMessage )
  {
    this.status = aStatus;
    this.message = aMessage;
  }

  // METHODS

  /**
   * Creates an {@link AcquisitionResultStatus} instance for the given exception
   * with the status FAILED.
   * 
   * @param aThrowable
   *          the exception to create a {@link AcquisitionResultStatus} for,
   *          cannot be <code>null</code>.
   * @return a new instance of {@link AcquisitionResultStatus}, never
   *         <code>null</code>.
   */
  public static AcquisitionResultStatus create( final Throwable aThrowable )
  {
    return new AcquisitionResultStatus( ResultStatus.FAILED, aThrowable.getMessage() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof AcquisitionResultStatus ) )
    {
      return false;
    }

    final AcquisitionResultStatus other = ( AcquisitionResultStatus )aObject;
    if ( this.status != other.status )
    {
      return false;
    }

    if ( this.message == null )
    {
      if ( other.message != null )
      {
        return false;
      }
    }
    else if ( !this.message.equals( other.message ) )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns an <em>optional</em> status message, for example, containing
   * details about why an acquisition failed.
   * 
   * @return a status message, can be <code>null</code>.
   */
  public String getMessage()
  {
    return this.message;
  }

  /**
   * Returns the status of the acquisition.
   * 
   * @return a status, never <code>null</code>.
   */
  public ResultStatus getStatus()
  {
    return this.status;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( ( this.message == null ) ? 0 : this.message.hashCode() );
    result = ( prime * result ) + ( ( this.status == null ) ? 0 : this.status.hashCode() );
    return result;
  }

  /**
   * Returns whether the status of this {@link AcquisitionResultStatus} is
   * aborted.
   * 
   * @return <code>true</code> if this {@link AcquisitionResultStatus} has the
   *         status aborted, <code>false</code> otherwise.
   */
  public boolean isAborted()
  {
    return this.status == ResultStatus.ABORTED;
  }

  /**
   * Returns whether the status of this {@link AcquisitionResultStatus} is
   * normal.
   * 
   * @return <code>true</code> if this {@link AcquisitionResultStatus} has the
   *         status normal, <code>false</code> otherwise.
   */
  public boolean isCompletedNormally()
  {
    return this.status == ResultStatus.NORMAL;
  }

  /**
   * Returns whether the status of this {@link AcquisitionResultStatus} is
   * failed.
   * 
   * @return <code>true</code> if this {@link AcquisitionResultStatus} has the
   *         status failed, <code>false</code> otherwise.
   */
  public boolean isFailed()
  {
    return this.status == ResultStatus.FAILED;
  }
}

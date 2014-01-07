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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import java.awt.*;

import nl.lxtreme.ols.common.*;


/**
 * Provides a builder for creating {@link Cursor}s.
 */
public class CursorBuilder
{
  // INNER TYPES

  /**
   * Provides a default implementation of {@link Cursor}.
   */
  static final class CursorImpl implements Cursor
  {
    // CONSTANTS

    private static final long UNDEFINED = -1L;

    // VARIABLES

    private final int index;
    private AcquisitionData data;

    private String label;
    private long timestamp;
    private Color color;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CursorImpl} instance.
     */
    private CursorImpl( int aIndex, Color aColor, String aLabel, long aTimestamp )
    {
      this.index = aIndex;

      this.color = aColor;
      this.label = aLabel;
      this.timestamp = aTimestamp;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear()
    {
      this.timestamp = UNDEFINED;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Cursor clone()
    {
      try
      {
        CursorImpl clone = ( CursorImpl )super.clone();
        clone.label = this.label;
        clone.timestamp = this.timestamp;
        return clone;
      }
      catch ( CloneNotSupportedException exception )
      {
        throw new RuntimeException( "Clone contract broken?!" );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Cursor aOther )
    {
      return getIndex() - aOther.getIndex();
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
      if ( ( aObject == null ) || !( aObject instanceof CursorImpl ) )
      {
        return false;
      }

      CursorImpl other = ( CursorImpl )aObject;
      if ( this.index != other.index )
      {
        return false;
      }
      if ( this.timestamp != other.timestamp )
      {
        return false;
      }

      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Color getColor()
    {
      return this.color;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getIndex()
    {
      return this.index;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getLabel()
    {
      return this.label;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getLabel( final LabelStyle aStyle )
    {
      if ( !isDefined() )
      {
        return "";
      }

      final String timestampText;
      if ( this.data.hasTimingData() )
      {
        double _ts = this.timestamp;
        if ( this.data.hasTriggerData() )
        {
          _ts -= this.data.getTriggerPosition();
        }
        _ts = _ts / this.data.getSampleRate();

        timestampText = Unit.Time.toUnit( _ts ).formatHumanReadable( _ts );
      }
      else
      {
        timestampText = "#" + this.timestamp;
      }

      Integer index = Integer.valueOf( this.index + 1 );

      String label = getLabel();
      if ( !hasLabel() )
      {
        label = index.toString();
      }

      switch ( aStyle )
      {
        case LABEL_TIME:
          return String.format( "%s: %s", label, timestampText );
        case INDEX_LABEL:
          return String.format( "%d: %s", index, label );
        case TIME_ONLY:
          return timestampText;
        case LABEL_ONLY:
          return label;
        case INDEX_ONLY:
        default:
          return String.format( "%d", index );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getTimestamp()
    {
      if ( this.timestamp < 0L )
      {
        throw new IllegalStateException( "Undefined cursor!" );
      }
      return this.timestamp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + this.index;
      result = ( prime * result ) + ( int )( this.timestamp ^ ( this.timestamp >>> 32 ) );
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasLabel()
    {
      return ( this.label != null ) && !"".equals( this.label.trim() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean inArea( long aTimestamp, double aDelta )
    {
      if ( !isDefined() )
      {
        return false;
      }

      final double min = this.timestamp - aDelta;
      final double max = this.timestamp + aDelta;

      return ( ( aTimestamp >= min ) && ( aTimestamp <= max ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isDefined()
    {
      return this.timestamp >= 0L;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColor( Color aColor )
    {
      this.color = aColor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLabel( final String aLabel )
    {
      this.label = aLabel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setTimestamp( final long aTimestamp )
    {
      this.timestamp = aTimestamp;
    }

    void setAcquisitionData( AcquisitionData aData )
    {
      this.data = aData;
    }
  }

  // VARIABLES

  private Color color;
  private String name;
  private long timestamp;
  private int index;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CursorBuilder} instance.
   */
  CursorBuilder()
  {
    this.index = -1;
    this.timestamp = CursorImpl.UNDEFINED;
    this.color = null;
    this.name = null;
  }

  public CursorBuilder setColor( Color aColor )
  {
    this.color = aColor;
    return this;
  }

  public CursorBuilder setIndex( int aIndex )
  {
    this.index = aIndex;
    return this;
  }

  public CursorBuilder setLabel( String aLabel )
  {
    this.name = aLabel;
    return this;
  }

  public CursorBuilder setTimestamp( long aTimestamp )
  {
    this.timestamp = aTimestamp;
    return this;
  }

  CursorImpl build( AcquisitionDataBuilder aDataBuilder )
  {
    if ( this.index < 0 || this.index >= OlsConstants.MAX_CURSORS )
    {
      throw new IllegalArgumentException( "Invalid cursor index " + this.index + "!" );
    }

    return new CursorImpl( this.index, this.color, this.name, this.timestamp );
  }
}

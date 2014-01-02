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


import nl.lxtreme.ols.common.acquisition.ChannelGroupBuilder.ChannelGroupImpl;


/**
 * Provides a builder for creating {@link Channel}s.
 */
public class ChannelBuilder
{
  // INNER TYPES

  /**
   * Provides a default implementation of {@link Channel}.
   */
  static final class ChannelImpl implements Channel
  {
    // VARIABLES

    private final int index;
    private final int mask;

    private String label;
    private boolean enabled;
    private ChannelGroupImpl group;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelImpl} instance.
     * 
     * @param aIndex
     *          the index of the channel to represent;
     * @param aEnabled
     *          <code>true</code> if the channel is enabled, <code>false</code>
     *          otherwise.
     */
    public ChannelImpl( int aIndex, String aLabel, boolean aEnabled )
    {
      this.index = aIndex;
      this.enabled = aEnabled;
      this.mask = 1 << aIndex;
      this.label = aLabel;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Channel aOther )
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
      if ( ( aObject == null ) || !( aObject instanceof ChannelImpl ) )
      {
        return false;
      }

      ChannelImpl other = ( ChannelImpl )aObject;
      if ( this.index != other.index )
      {
        return false;
      }
      if ( this.mask != other.mask )
      {
        return false;
      }

      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ChannelGroup getGroup()
    {
      return this.group;
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
    public int getMask()
    {
      return this.mask;
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
      result = ( prime * result ) + this.mask;
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasName()
    {
      return ( this.label != null ) && !"".equals( this.label.trim() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEnabled()
    {
      return this.enabled;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setEnabled( final boolean aEnabled )
    {
      this.enabled = aEnabled;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLabel( final String aName )
    {
      if ( ( aName == null ) || "".equals( aName.trim() ) )
      {
        this.label = getDefaultLabel( this.index );
      }
      else
      {
        this.label = aName.trim();
      }
    }

    /**
     * Removes this channel from its group (if connected to such group).
     */
    void removeFromGroup()
    {
      if ( this.group != null )
      {
        this.group.remove( this );
      }
    }

    void setChannelGroup( ChannelGroupImpl aGroup )
    {
      if ( this.group != null && !this.group.equals( aGroup ) )
      {
        throw new IllegalArgumentException( "Channel " + this.index + " already belongs to group "
            + this.group.getIndex() + " and cannot be added to group " + aGroup.getIndex() );
      }
      this.group = aGroup;
    }
  }

  // VARIABLES

  private String name;
  private boolean enabled;
  private int index;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelBuilder} instance.
   */
  ChannelBuilder()
  {
    this.index = -1;
    this.enabled = true;
    this.name = null;
  }

  /**
   * Returns the default label for a channel.
   * 
   * @param aIndex
   *          the index of the channel to create the label for.
   * @return a default label, never <code>null</code>.
   */
  static String getDefaultLabel( final int aIndex )
  {
    return String.format( "Channel %d", Integer.valueOf( aIndex ) );
  }

  public ChannelBuilder setLabel( String aLabel )
  {
    this.name = aLabel;
    return this;
  }

  public ChannelBuilder setIndex( int aIndex )
  {
    this.index = aIndex;
    return this;
  }

  public ChannelBuilder setEnabled( boolean aEnabled )
  {
    this.enabled = aEnabled;
    return this;
  }

  ChannelImpl build( AcquisitionDataBuilder aDataBuilder )
  {
    if ( this.index < 0 || this.index > 31 )
    {
      throw new IllegalArgumentException( "Invalid channel index " + this.index + "!" );
    }
    if ( this.name == null )
    {
      this.name = getDefaultLabel( this.index );
    }

    return new ChannelImpl( this.index, this.name, this.enabled );
  }
}

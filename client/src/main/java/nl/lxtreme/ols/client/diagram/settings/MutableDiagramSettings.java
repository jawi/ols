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
package nl.lxtreme.ols.client.diagram.settings;


import java.awt.*;
import java.util.*;
import java.util.Map.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides mutable diagram settings.
 */
public class MutableDiagramSettings implements DiagramSettings
{
  // INNER TYPES

  /**
   * Provides a user settings iterator.
   */
  static final class UserSettingsIterator implements Iterator<Map.Entry<String, Object>>
  {
    // VARIABLES

    private final Iterator<Entry<Object, Object>> entries;

    // CONSTRUCTORS

    /**
     * Creates a new UserSettingsIterator instance.
     * 
     * @param aProperties
     *          the properties to iterate over, cannot be <code>null</code>.
     */
    public UserSettingsIterator( final Properties aProperties )
    {
      this.entries = aProperties.entrySet().iterator();
    }

    // METHODS

    /**
     * @see java.util.Iterator#hasNext()
     */
    @Override
    public boolean hasNext()
    {
      return this.entries.hasNext();
    }

    /**
     * @see java.util.Iterator#next()
     */
    @Override
    public Entry<String, Object> next()
    {
      final Entry<Object, Object> nextEntry = this.entries.next();

      final String key = String.valueOf( nextEntry.getKey() );
      final Object value = nextEntry.getValue();

      return new AbstractMap.SimpleImmutableEntry<String, Object>( key, value );
    }

    /**
     * @see java.util.Iterator#remove()
     */
    @Override
    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }

  // CONSTANTS

  public static final String NAME = "DiagramSettings";

  private static final long serialVersionUID = 1L;

  private static final ColorScheme DEFAULT_COLOR_SCHEME = ColorScheme.DARK;
  private static final boolean DEFAULT_SHOW_CURSOR_TIMING = true;
  private static final ColorTarget DEFAULT_COLOR_TARGET = ColorTarget.SIGNALS;
  private static final int DEFAULT_GROUP_SETTING = DISPLAY_CHANNELS;
  private static final int DEFAULT_CHANNEL_HEIGHT = 35;
  private static final int DEFAULT_SIGNAL_HEIGHT = 20;
  private static final int DEFAULT_SCOPE_HEIGHT = 133;
  private static final SignalAlignment DEFAULT_SIGNAL_ALIGNMENT = SignalAlignment.CENTER;
  private static final EdgeSlope DEFAULT_EDGE_SLOPE = EdgeSlope.NON_PERPENDICULAR;

  // VARIABLES

  private final String name;
  private final Properties properties;

  // CONSTRUCTORS

  /**
   * Creates a new MutableDiagramSettings instance.
   */
  public MutableDiagramSettings()
  {
    this.name = NAME;
    this.properties = new Properties();

    setDefaultColorScheme();
  }

  /**
   * Creates a new MutableDiagramSettings instance.
   * 
   * @param aDiagramSettings
   *          the diagram settings to use as default, may be <code>null</code>
   *          in which case this constructor falls back to the default settings.
   */
  public MutableDiagramSettings( final DiagramSettings aDiagramSettings )
  {
    this();

    if ( aDiagramSettings != null )
    {
      setColorScheme( aDiagramSettings.getColorScheme() );
      setBackgroundColor( aDiagramSettings.getBackgroundColor() );
      for ( int i = 0; i < Ols.MAX_CHANNELS; i++ )
      {
        setChannelColor( i, aDiagramSettings.getChannelColor( i ) );
      }
      setChannelHeight( aDiagramSettings.getChannelHeight() );
      setColorTarget( aDiagramSettings.getColorTarget() );
      for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
      {
        setCursorColor( i, aDiagramSettings.getCursorColor( i ) );
      }
      setEdgeSlope( aDiagramSettings.getEdgeSlope() );
      setGridColor( aDiagramSettings.getGridColor() );
      setGroupBackgroundColor( aDiagramSettings.getGroupBackgroundColor() );
      setGroupByteColor( aDiagramSettings.getGroupByteColor() );
      setLabelColor( aDiagramSettings.getLabelColor() );
      setScopeColor( aDiagramSettings.getScopeColor() );
      setScopeHeight( aDiagramSettings.getScopeHeight() );
      for ( int i = 0; i < Ols.MAX_BLOCKS; i++ )
      {
        setShowByte( i, aDiagramSettings.isShowByte( i ) );
        setShowChannels( i, aDiagramSettings.isShowChannels( i ) );
        setShowScope( i, aDiagramSettings.isShowScope( i ) );
      }
      setShowCursorTiming( aDiagramSettings.isShowCursorTiming() );
      setSignalAlignment( aDiagramSettings.getSignalAlignment() );
      setSignalColor( aDiagramSettings.getSignalColor() );
      setSignalHeight( aDiagramSettings.getSignalHeight() );
      setTextColor( aDiagramSettings.getTextColor() );
      setTimeColor( aDiagramSettings.getTimeColor() );
      setTriggerColor( aDiagramSettings.getTriggerColor() );
    }
  }

  /**
   * Creates a new MutableDiagramSettings instance.
   * 
   * @param aSettings
   *          the diagram settings to use as default, may be <code>null</code>
   *          in which case this constructor falls back to the default settings.
   * @throws IllegalArgumentException
   *           in case the given settings was <code>null</code> or its name was
   *           not {@link #NAME}.
   */
  public MutableDiagramSettings( final UserSettings aSettings )
  {
    if ( aSettings == null )
    {
      throw new IllegalArgumentException();
    }
    if ( !NAME.equals( aSettings.getName() ) )
    {
      throw new IllegalArgumentException( "User settings name incorrect!" );
    }

    this.name = aSettings.getName();
    this.properties = new Properties();
    for ( Map.Entry<String, Object> entry : aSettings )
    {
      this.properties.put( entry.getKey(), entry.getValue() );
    }
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String get( final String aName, final String aDefaultValue )
  {
    String result = this.properties.getProperty( aName, aDefaultValue );
    return result;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getBackgroundColor()
   */
  @Override
  public final Color getBackgroundColor()
  {
    return getColor( "backgroundColor", getDefaultBackgroundColor() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean getBoolean( final String aName, final boolean aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }
    return Boolean.parseBoolean( value );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getChannelColor(int)
   */
  @Override
  public final Color getChannelColor( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx >= Ols.MAX_CHANNELS ) )
    {
      throw new IllegalArgumentException( "Invalid channel index!" );
    }
    String name = "channelColor." + aChannelIdx;
    return getColor( name, getDefaultChannelColor( aChannelIdx ) );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getChannelHeight()
   */
  @Override
  public final int getChannelHeight()
  {
    return getInt( "channelHeight", DEFAULT_CHANNEL_HEIGHT );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getColorScheme()
   */
  @Override
  public final ColorScheme getColorScheme()
  {
    return getEnumValue( "colorScheme", DEFAULT_COLOR_SCHEME );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getColorTarget()
   */
  @Override
  public final ColorTarget getColorTarget()
  {
    return getEnumValue( "colorTarget", DEFAULT_COLOR_TARGET );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getCursorColor(int)
   */
  @Override
  public final Color getCursorColor( final int aCursorIdx )
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx >= Ols.MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    String name = "cursorColor." + aCursorIdx;
    return getColor( name, getDefaultCursorColor( aCursorIdx ) );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getEdgeSlope()
   */
  @Override
  public final EdgeSlope getEdgeSlope()
  {
    return getEnumValue( "edgeSlope", DEFAULT_EDGE_SLOPE );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getGridColor()
   */
  @Override
  public final Color getGridColor()
  {
    return getColor( "gridColor", getDefaultGridColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getGroupBackgroundColor()
   */
  @Override
  public final Color getGroupBackgroundColor()
  {
    return getColor( "groupBackgroundColor", getDefaultGroupBackgroundColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getGroupByteColor()
   */
  @Override
  public final Color getGroupByteColor()
  {
    return getColor( "groupByteColor", getDefaultGroupByteColor() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getInt( final String aName, final int aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }
    return Integer.parseInt( value );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getLabelColor()
   */
  @Override
  public final Color getLabelColor()
  {
    return getColor( "labelColor", getDefaultLabelColor() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public long getLong( final String aName, final long aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }
    return Long.parseLong( value );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return this.name;
  }

  /**
   * Returns the properties representation of these user settings.
   * 
   * @return the properties, never <code>null</code>.
   */
  public Properties getProperties()
  {
    return this.properties;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getScopeColor()
   */
  @Override
  public final Color getScopeColor()
  {
    return getColor( "scopeColor", getDefaultScopeColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getScopeHeight()
   */
  @Override
  public final int getScopeHeight()
  {
    return getInt( "scopeHeight", DEFAULT_SCOPE_HEIGHT );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getSignalAlignment()
   */
  @Override
  public final SignalAlignment getSignalAlignment()
  {
    return getEnumValue( "signalAlignment", DEFAULT_SIGNAL_ALIGNMENT );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getSignalColor()
   */
  @Override
  public Color getSignalColor()
  {
    return getColor( "signalColor", getDefaultSignalColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getSignalHeight()
   */
  @Override
  public final int getSignalHeight()
  {
    return getInt( "signalHeight", DEFAULT_SIGNAL_HEIGHT );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getTextColor()
   */
  @Override
  public final Color getTextColor()
  {
    return getColor( "textColor", getDefaultTextColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getTimeColor()
   */
  @Override
  public final Color getTimeColor()
  {
    return getColor( "timeColor", getDefaultTimeColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getTriggerColor()
   */
  @Override
  public final Color getTriggerColor()
  {
    return getColor( "triggerColor", getDefaultTriggerColor() );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowByte(int)
   */
  public final boolean isShowByte( final int aGroup )
  {
    final int groupSetting = getGroupSetting( aGroup );
    return ( ( groupSetting & DiagramSettings.DISPLAY_BYTE ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowChannels(int)
   */
  public final boolean isShowChannels( final int aGroup )
  {
    final int groupSetting = getGroupSetting( aGroup );
    return ( ( groupSetting & DiagramSettings.DISPLAY_CHANNELS ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowCursorTiming()
   */
  @Override
  public boolean isShowCursorTiming()
  {
    return getBoolean( "showCursorTiming", DEFAULT_SHOW_CURSOR_TIMING );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowScope(int)
   */
  public final boolean isShowScope( final int aGroup )
  {
    final int groupSetting = getGroupSetting( aGroup );
    return ( ( groupSetting & DiagramSettings.DISPLAY_SCOPE ) > 0 );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<Entry<String, Object>> iterator()
  {
    return new UserSettingsIterator( this.properties );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void put( final String aName, final String aValue )
  {
    this.properties.put( aName, aValue );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putAll( final Map<?, ?> aSettings )
  {
    this.properties.putAll( aSettings );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putBoolean( final String aName, final boolean aValue )
  {
    this.properties.put( aName, Boolean.toString( aValue ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putInt( final String aName, final int aValue )
  {
    this.properties.put( aName, Integer.toString( aValue ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putLong( final String aName, final long aValue )
  {
    this.properties.put( aName, Long.toString( aValue ) );
  }

  /**
   * @param aBackgroundColor
   *          the backgroundColor to set
   */
  public final void setBackgroundColor( final Color aBackgroundColor )
  {
    if ( aBackgroundColor == null )
    {
      throw new IllegalArgumentException( "Background color cannot be null!" );
    }
    putColor( "backgroundColor", aBackgroundColor );
  }

  /**
   * @param aChannelColor
   *          the channelColors to set
   */
  public final void setChannelColor( final int aChannelIdx, final Color aChannelColor )
  {
    if ( aChannelColor == null )
    {
      throw new IllegalArgumentException( "Channel color cannot be null!" );
    }
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx >= Ols.MAX_CHANNELS ) )
    {
      throw new IllegalArgumentException( "Invalid channel index!" );
    }
    String name = "channelColor." + aChannelIdx;
    putColor( name, aChannelColor );
  }

  /**
   * @param aChannelHeight
   *          the channelHeight to set
   */
  public final void setChannelHeight( final int aChannelHeight )
  {
    if ( aChannelHeight <= 0 )
    {
      throw new IllegalArgumentException( "Channel height cannot be zero or negative!" );
    }
    putInt( "channelHeight", aChannelHeight );
  }

  /**
   * @param aColorScheme
   */
  public final void setColorScheme( final ColorScheme aColorScheme )
  {
    if ( aColorScheme == null )
    {
      throw new IllegalArgumentException( "Color scheme cannot be null!" );
    }
    putEnumValue( "colorScheme", aColorScheme );

    if ( ColorScheme.CUSTOM != aColorScheme )
    {
      setDefaultColorScheme();
    }
  }

  /**
   * Sets the color target.
   * 
   * @param aColorTarget
   *          the color target to set, cannot be <code>null</code>.
   */
  public final void setColorTarget( final ColorTarget aColorTarget )
  {
    if ( aColorTarget == null )
    {
      throw new IllegalArgumentException( "Color target cannot be null!" );
    }
    putEnumValue( "colorTarget", aColorTarget );
  }

  /**
   * @param aCursorColors
   *          the cursorColors to set
   */
  public final void setCursorColor( final int aCursorIdx, final Color aCursorColor )
  {
    if ( aCursorColor == null )
    {
      throw new IllegalArgumentException( "Cursor color cannot be null!" );
    }
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx >= Ols.MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    String name = "cursorColor." + aCursorIdx;
    putColor( name, aCursorColor );
  }

  /**
   * Sets the signal edge slope.
   * 
   * @param aEdgeSlope
   *          the edge slope to set, cannot be <code>null</code>.
   */
  public final void setEdgeSlope( final EdgeSlope aEdgeSlope )
  {
    if ( aEdgeSlope == null )
    {
      throw new IllegalArgumentException( "Edge slope cannot be null!" );
    }
    putEnumValue( "edgeSlope", aEdgeSlope );
  }

  /**
   * @param aGridColor
   *          the gridColor to set
   */
  public final void setGridColor( final Color aGridColor )
  {
    if ( aGridColor == null )
    {
      throw new IllegalArgumentException( "Grid color cannot be null!" );
    }
    putColor( "gridColor", aGridColor );
  }

  /**
   * @param aGroupBackgroundColor
   *          the groupBackgroundColor to set
   */
  public final void setGroupBackgroundColor( final Color aGroupBackgroundColor )
  {
    if ( aGroupBackgroundColor == null )
    {
      throw new IllegalArgumentException( "Groupbyte background color cannot be null!" );
    }
    putColor( "groupBackgroundColor", aGroupBackgroundColor );
  }

  /**
   * @param aGroupByteColor
   *          the groupByteColor to set
   */
  public final void setGroupByteColor( final Color aGroupByteColor )
  {
    if ( aGroupByteColor == null )
    {
      throw new IllegalArgumentException( "Groupbyte color cannot be null!" );
    }
    putColor( "groupByteColor", aGroupByteColor );
  }

  /**
   * @param aLabelColor
   *          the labelColor to set
   */
  public final void setLabelColor( final Color aLabelColor )
  {
    if ( aLabelColor == null )
    {
      throw new IllegalArgumentException( "Label color cannot be null!" );
    }
    putColor( "labelColor", aLabelColor );
  }

  /**
   * @param aScopeColor
   *          the scopeColor to set
   */
  public final void setScopeColor( final Color aScopeColor )
  {
    if ( aScopeColor == null )
    {
      throw new IllegalArgumentException( "Scope color cannot be null!" );
    }
    putColor( "scopeColor", aScopeColor );
  }

  /**
   * @param aScopeHeight
   *          the scopeHeight to set
   */
  public final void setScopeHeight( final int aScopeHeight )
  {
    if ( aScopeHeight <= 0 )
    {
      throw new IllegalArgumentException( "Scope height cannot be zero or negative!" );
    }
    putInt( "scopeHeight", aScopeHeight );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#setShowByte(int,
   *      boolean)
   */
  public final void setShowByte( final int aGroup, final boolean aShow )
  {
    int groupSetting = getGroupSetting( aGroup );
    if ( aShow )
    {
      groupSetting |= DiagramSettings.DISPLAY_BYTE;
    }
    else
    {
      groupSetting &= ~DiagramSettings.DISPLAY_BYTE;
    }
    setGroupSetting( aGroup, groupSetting );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#setShowChannels(int,
   *      boolean)
   */
  public final void setShowChannels( final int aGroup, final boolean aShow )
  {
    int groupSetting = getGroupSetting( aGroup );
    if ( aShow )
    {
      groupSetting |= DiagramSettings.DISPLAY_CHANNELS;
    }
    else
    {
      groupSetting &= ~DiagramSettings.DISPLAY_CHANNELS;
    }
    setGroupSetting( aGroup, groupSetting );
  }

  /**
   * @param aShowCursorTiming
   *          <code>true</code> if cursor timings should be shown,
   *          <code>false</code> otherwise.
   */
  public final void setShowCursorTiming( final boolean aShowCursorTiming )
  {
    putBoolean( "showCursorTiming", aShowCursorTiming );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#setShowScope(int,
   *      boolean)
   */
  public final void setShowScope( final int aGroup, final boolean aShow )
  {
    int groupSetting = getGroupSetting( aGroup );
    if ( aShow )
    {
      groupSetting |= DiagramSettings.DISPLAY_SCOPE;
    }
    else
    {
      groupSetting &= ~DiagramSettings.DISPLAY_SCOPE;
    }
    setGroupSetting( aGroup, groupSetting );
  }

  /**
   * Sets the signal alignment.
   * 
   * @param aSignalAlignment
   *          a signal alignment, never <code>null</code>.
   */
  public final void setSignalAlignment( final SignalAlignment aSignalAlignment )
  {
    if ( aSignalAlignment == null )
    {
      throw new IllegalArgumentException( "Signal alignment cannot be null!" );
    }
    putEnumValue( "signalAlignment", aSignalAlignment );
  }

  /**
   * @param aSignalColor
   */
  public final void setSignalColor( final Color aSignalColor )
  {
    if ( aSignalColor == null )
    {
      throw new IllegalArgumentException( "Signal color cannot be null!" );
    }
    putColor( "signalColor", aSignalColor );
  }

  /**
   * @param aSignalHeight
   *          the signalHeight to set
   */
  public final void setSignalHeight( final int aSignalHeight )
  {
    if ( aSignalHeight <= 0 )
    {
      throw new IllegalArgumentException( "Signal height cannot be zero or negative!" );
    }
    putInt( "signalHeight", aSignalHeight );
  }

  /**
   * @param aTextColor
   *          the textColor to set
   */
  public final void setTextColor( final Color aTextColor )
  {
    if ( aTextColor == null )
    {
      throw new IllegalArgumentException( "Text color cannot be null!" );
    }
    putColor( "textColor", aTextColor );
  }

  /**
   * @param aTimeColor
   *          the timeColor to set
   */
  public final void setTimeColor( final Color aTimeColor )
  {
    if ( aTimeColor == null )
    {
      throw new IllegalArgumentException( "Time color cannot be null!" );
    }
    putColor( "timeColor", aTimeColor );
  }

  /**
   * @param aTriggerColor
   *          the triggerColor to set
   */
  public final void setTriggerColor( final Color aTriggerColor )
  {
    if ( aTriggerColor == null )
    {
      throw new IllegalArgumentException( "Trigger color cannot be null!" );
    }
    putColor( "triggerColor", aTriggerColor );
  }

  /**
   * Removes the value identified by the given name.
   * 
   * @param aName
   *          the name of the value to remove, cannot be <code>null</code>.
   */
  protected void delete( final String aName )
  {
    this.properties.remove( aName );
  }

  /**
   * Convenience method to return the color of a certain property.
   * 
   * @param aName
   * @param aDefaultValue
   * @return
   */
  protected Color getColor( final String aName, final Color aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }

    return ColorUtils.parseColor( value );
  }

  /**
   * Convenience method to add a enum constant to the properties.
   * 
   * @param <T>
   *          the enum type to put into this settings object;
   * @param aName
   *          the name of which the enum constant should be retrieved;
   * @param aDefault
   *          the default value to return in case the enum constant is not set.
   * @return the enum value.
   */
  @SuppressWarnings( "unchecked" )
  protected <T extends Enum<T>> T getEnumValue( final String aName, final T aDefault )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefault;
    }

    return ( T )Enum.valueOf( aDefault.getClass(), value );
  }

  /**
   * Convenience method to return the color of a certain property.
   * 
   * @param aName
   * @param aValue
   */
  protected void putColor( final String aName, final Color aValue )
  {
    if ( aValue == null )
    {
      throw new IllegalArgumentException( "Color cannot be null! Use delete() method to delete a key-value pair!" );
    }
    final String hexString = ColorUtils.toHexString( aValue );
    this.properties.put( aName, hexString );
  }

  /**
   * Convenience method to add a enum constant to the properties.
   * 
   * @param <T>
   *          the enum type to put into this settings object;
   * @param aName
   *          the name under which the enum constant should be stored;
   * @param aValue
   *          the enum constant to store.
   */
  protected <T extends Enum<T>> void putEnumValue( final String aName, final T aValue )
  {
    if ( aValue == null )
    {
      throw new IllegalArgumentException( "Enum cannot be null! Use delete() method to delete a key-value pair!" );
    }
    this.properties.put( aName, aValue.name() );
  }

  /**
   * @return
   */
  private Color getDefaultBackgroundColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return Color.WHITE;
    }
    return new Color( 0x10, 0x10, 0x10 );
  }

  /**
   * @param aChannelIdx
   * @return
   */
  private Color getDefaultChannelColor( final int aChannelIdx )
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return getDefaultSignalColor();
    }
    return makePaletteColor( aChannelIdx, 8 );
  }

  /**
   * @param aCursorIdx
   * @return
   */
  private Color getDefaultCursorColor( final int aCursorIdx )
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return makePaletteColor( aCursorIdx, Ols.MAX_CURSORS );
    }
    return getDefaultSignalColor();
  }

  /**
   * @return
   */
  private Color getDefaultGridColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      new Color( 0xc9, 0xc9, 0xc9 );
    }
    return new Color( 0x30, 0x30, 0x30 );
  }

  /**
   * @return
   */
  private Color getDefaultGroupBackgroundColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return getDefaultGridColor().darker();
    }
    return getDefaultGridColor().brighter();
  }

  /**
   * @return
   */
  private Color getDefaultGroupByteColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return getDefaultSignalColor();
    }
    return getDefaultSignalColor();
  }

  /**
   * @return
   */
  private Color getDefaultLabelColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return new Color( 0x82, 0x87, 0x8f );
    }
    return new Color( 0x82, 0x87, 0x8f );
  }

  /**
   * @return
   */
  private Color getDefaultScopeColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return getDefaultSignalColor();
    }
    return getDefaultSignalColor();
  }

  /**
   * @return
   */
  private Color getDefaultSignalColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return new Color( 0x30, 0x4b, 0x75 );
    }
    return new Color( 0xc9, 0xc9, 0xc9 );
  }

  /**
   * @return
   */
  private Color getDefaultTextColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return new Color( 0x25, 0x25, 0x25 );
    }
    return Color.WHITE;
  }

  /**
   * @return
   */
  private Color getDefaultTimeColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return new Color( 0x25, 0x25, 0x25 );
    }
    return new Color( 0x82, 0x87, 0x8f );
  }

  /**
   * @return
   */
  private Color getDefaultTriggerColor()
  {
    if ( ColorScheme.LIGHT.equals( DEFAULT_COLOR_SCHEME ) )
    {
      return new Color( 0x82, 0x87, 0x8f );
    }
    return new Color( 0x82, 0x87, 0x8f );
  }

  /**
   * @param aGroup
   * @return
   */
  private int getGroupSetting( final int aGroupIdx )
  {
    if ( ( aGroupIdx < 0 ) || ( aGroupIdx >= Ols.MAX_BLOCKS ) )
    {
      throw new IllegalArgumentException( "Invalid group index!" );
    }
    final String name = "groupSetting." + aGroupIdx;
    return getInt( name, DEFAULT_GROUP_SETTING );
  }

  /**
   * @param aI
   * @param aFreq1
   * @param aFreq2
   * @param aFreq3
   * @param aPhase1
   * @param aPhase2
   * @param aPhase3
   * @return
   */
  private Color makeColorGradient( final int aI, final double aFreq1, final double aFreq2, final double aFreq3,
      final double aPhase1, final double aPhase2, final double aPhase3 )
  {
    final int width = 127;
    final int center = 128;
    final int red = ( int )( ( Math.sin( ( aFreq1 * aI ) + aPhase1 ) * width ) + center );
    final int grn = ( int )( ( Math.sin( ( aFreq2 * aI ) + aPhase2 ) * width ) + center );
    final int blu = ( int )( ( Math.sin( ( aFreq3 * aI ) + aPhase3 ) * width ) + center );
    return new Color( red, grn, blu );
  }

  /**
   * @return
   */
  private Color makePaletteColor( final int aIndex, final int aSteps )
  {
    final double freq = ( 2 * Math.PI ) / aSteps;
    return makeColorGradient( aIndex, freq, freq, freq, 2.0, 4.0, 6.0 );
  }

  /**
   * Returns to the default (chosen) color scheme by resetting all available
   * colors to <code>null</code>.
   */
  private void setDefaultColorScheme()
  {
    delete( "backgroundColor" );
    delete( "gridColor" );
    delete( "groupBackgroundColor" );
    delete( "groupByteColor" );
    delete( "labelColor" );
    delete( "scopeColor" );
    delete( "signalColor" );
    delete( "textColor" );
    delete( "timeColor" );
    delete( "triggerColor" );

    for ( int i = 0; i < Ols.MAX_CHANNELS; i++ )
    {
      String name = "channelColor." + i;
      delete( name );
    }
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      String name = "cursorColor." + i;
      delete( name );
    }
  }

  /**
   * @param aGroup
   * @param aGroupSetting
   */
  private void setGroupSetting( final int aGroupIdx, final int aGroupSetting )
  {
    if ( ( aGroupIdx < 0 ) || ( aGroupIdx >= Ols.MAX_BLOCKS ) )
    {
      throw new IllegalArgumentException( "Invalid group index!" );
    }
    final String name = "groupSetting." + aGroupIdx;
    putInt( name, aGroupSetting );
  }
}

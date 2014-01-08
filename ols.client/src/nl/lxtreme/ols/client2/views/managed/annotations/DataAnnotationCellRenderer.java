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
package nl.lxtreme.ols.client2.views.managed.annotations;


import static nl.lxtreme.ols.common.annotation.DataAnnotation.*;

import java.awt.*;
import java.awt.event.*;
import java.math.*;
import java.util.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.common.annotation.*;


/**
 * Provides a table cell renderer for {@link DataAnnotation}s.
 */
class DataAnnotationCellRenderer extends DefaultTableCellRenderer
{
  private static final long serialVersionUID = 1L;

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getTableCellRendererComponent( final JTable aTable, final Object aValue, final boolean aIsSelected,
      final boolean aHasFocus, final int aRow, final int aColumn )
  {
    String text = "";
    String tooltip = "";

    final DataAnnotation annotation = ( DataAnnotation )aValue;

    if ( annotation != null )
    {
      text = getText( annotation );
      tooltip = getDescription( annotation );
    }

    JLabel result = ( JLabel )super.getTableCellRendererComponent( aTable, text, aIsSelected, aHasFocus, aRow, aColumn );
    if ( !"".equals( tooltip ) )
    {
      result.setToolTipText( tooltip );
    }

    // Make sure the row height is sufficient to fill all text...
    int prefHeight = getPreferredSize().height;
    if ( aTable.getRowHeight( aRow ) < prefHeight )
    {
      aTable.setRowHeight( aRow, prefHeight );
    }

    return result;
  }

  /**
   * Returns a text description for the given annotation, useful for tooltips.
   * 
   * @param aAnnotation
   *          the annotation to get a description for, cannot be
   *          <code>null</code>;
   * @param aIncludeTimingData
   *          <code>true</code> to include timing data (start & stop of
   *          annotation), <code>false</code> to omit timing data.
   * @return an annotation description, never <code>null</code>.
   */
  private String getDescription( Annotation aAnnotation )
  {
    Object data = aAnnotation.getData();

    Map<String, Object> props = new HashMap<String, Object>();
    String description = null;
    boolean symbol = false;
    boolean error = false;
    boolean event = false;

    if ( aAnnotation instanceof DataAnnotation )
    {
      final DataAnnotation dataAnnotation = ( DataAnnotation )aAnnotation;
      props.putAll( dataAnnotation.getProperties() );

      description = ( String )props.remove( KEY_DESCRIPTION );
      Object type = props.remove( KEY_TYPE );
      props.remove( KEY_COLOR );

      error = TYPE_ERROR.equals( type );
      event = TYPE_EVENT.equals( type );
      symbol = TYPE_SYMBOL.equals( type );
    }

    StringBuilder sb = new StringBuilder(
        "<html><head><style>th{text-align:right;} td{padding:0; margin:0;}</style></head><body><table class='desc' border='0'>" );

    if ( description != null )
    {
      sb.append( "<tr><td colspan='2'>" ).append( description ).append( "</td></tr>" );
    }

    if ( symbol )
    {
      sb.append( "<tr><th>Symbol</th><td>" );

      if ( data instanceof Number )
      {
        sb.append( "<table border='0'>" );
        if ( data instanceof Integer )
        {
          int value = ( ( Integer )data ).intValue();
          if ( ( value >= 0 ) && isPrintableChar( ( char )value ) )
          {
            sb.append( "<tr><th valign='top'>char</th><td>'" ).append( ( char )value ).append( "'</td></tr>" );
          }
        }

        sb.append( "<tr><th>dec</th><td>" ).append( String.format( "%d", data ) ).append( "</td></tr>" );
        sb.append( "<tr><th>hex</th><td>" ).append( String.format( "%x", data ) ).append( "</td></tr>" );
        sb.append( "<tr><th>oct</th><td>" ).append( String.format( "%o", data ) ).append( "</td></tr>" );

        String binary;
        if ( data instanceof BigInteger )
        {
          binary = ( ( BigInteger )data ).toString( 2 );
        }
        else
        {
          binary = Long.toBinaryString( ( ( Number )data ).longValue() );
        }

        sb.append( "<tr><th>bin</th><td>" ).append( binary ).append( "</td></tr>" );
        sb.append( "</table>" );
      }
      else
      {
        sb.append( data );
      }
      sb.append( "</td></tr>" );
    }

    if ( event )
    {
      sb.append( "<tr><th>Event</th><td>" ).append( data ).append( "</td></tr>" );
    }

    if ( error )
    {
      sb.append( "<tr><th>Error</th><td>" ).append( data ).append( "</td></tr>" );
    }

    for ( Map.Entry<String, Object> entry : props.entrySet() )
    {
      sb.append( "<tr><th>" ).append( entry.getKey() ).append( "</th><td>" );
      sb.append( entry.getValue() ).append( "</td></tr>" );
    }

    return sb.append( "</table></body></html>" ).toString();
  }

  /**
   * Returns a text representation for the given annotation.
   * 
   * @param aAnnotation
   *          the annotation to get a text representation for, cannot be
   *          <code>null</code>.
   * @return a text representation, never <code>null</code>.
   */
  private String getText( DataAnnotation aAnnotation )
  {
    String result = null;

    Object data = aAnnotation.getData();

    Map<String, Object> props = aAnnotation.getProperties();
    Object desc = props.get( KEY_DESCRIPTION );
    Object type = props.get( KEY_TYPE );

    if ( TYPE_SYMBOL.equals( type ) && ( data instanceof Number ) )
    {
      if ( data instanceof Integer )
      {
        int value = ( ( Integer )data ).intValue();
        if ( ( value >= 0 ) && isPrintableChar( ( char )value ) )
        {
          result = String.format( "%1$c", data );
        }
      }
      else
      {
        result = String.format( "0x%1$x", data );
      }
    }

    if ( ( TYPE_EVENT.equals( type ) || TYPE_ERROR.equals( type ) ) && ( desc != null ) )
    {
      result = String.valueOf( desc );
    }

    if ( result == null )
    {
      result = String.format( "(%1$s)", String.valueOf( data ) );
    }

    return result;
  }

  /**
   * Determines if the given character is a printable character or not.
   * 
   * @param aChar
   *          the character to test.
   * @return <code>true</code> if the given character is a printable one,
   *         <code>false</code> otherwise.
   */
  private boolean isPrintableChar( final char aChar )
  {
    if ( Character.isISOControl( aChar ) || ( aChar == KeyEvent.CHAR_UNDEFINED ) )
    {
      return false;
    }
    Character.UnicodeBlock block = Character.UnicodeBlock.of( aChar );
    return ( block != null ) && ( block != Character.UnicodeBlock.SPECIALS );
  }
}

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
package nl.lxtreme.ols.client.signaldisplay.util;


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


/**
 * Provides a clickable link, which acts like a kind of hyperlink.
 */
public class ClickableLink extends JLabel
{
  // INNER TYPES

  /**
   * Will be called by {@link ClickableLink} when it is clicked.
   */
  public static interface LinkListener
  {
    /**
     * Called when a link is activated by being clicked.
     * 
     * @param aLinkId
     *          the identifier of the clicked link, cannot be <code>null</code>.
     */
    void linkActivated( Object aLinkId );
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Object linkId;

  private MouseListener mouseListener;

  private volatile LinkListener linkListener;

  // CONSTRUCTORS

  /**
   * Creates a new ClickableLink instance.
   * 
   * @param aText
   *          the text of this link, can be <code>null</code>;
   * @param aLinkId
   *          the identifier object to pass to the {@link LinkListener}, if this
   *          link is clicked.
   */
  public ClickableLink( final String aText, final Object aLinkId )
  {
    super( aText );

    this.linkId = aLinkId;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    super.addNotify();

    addMouseListener( this.mouseListener = new MouseAdapter()
    {
      @Override
      public void mouseClicked( final MouseEvent aEvent )
      {
        if ( isEnabled() && !aEvent.isConsumed() )
        {
          invokeLinkListener();
        }
      }

      @Override
      public void mouseEntered( final MouseEvent aEvent )
      {
        if ( isEnabled() )
        {
          aEvent.getComponent().setCursor( Cursor.getPredefinedCursor( Cursor.HAND_CURSOR ) );
        }
      }

      @Override
      public void mouseExited( final MouseEvent aEvent )
      {
        if ( isEnabled() )
        {
          aEvent.getComponent().setCursor( Cursor.getDefaultCursor() );
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeNotify()
  {
    removeMouseListener( this.mouseListener );
    this.mouseListener = null;

    super.removeNotify();
  }

  /**
   * Sets (or removes) the link listener.
   * 
   * @param aListener
   *          the listener to set, can be <code>null</code> to stop listening.
   */
  public void setLinkListener( final LinkListener aListener )
  {
    this.linkListener = aListener;
  }

  /**
   * Invokes the (optional) link listener that the link has been clicked (or
   * activated).
   */
  final void invokeLinkListener()
  {
    if ( this.linkListener != null )
    {
      this.linkListener.linkActivated( this.linkId );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( final Graphics aCanvas )
  {
    Rectangle rect = getVisibleRect();

    Font f = getFont();
    FontMetrics fm = aCanvas.getFontMetrics( f );

    String text = getText();
    int tw = SwingUtilities.computeStringWidth( fm, text );

    int x1 = rect.x;
    int x2 = ( x1 + tw ) - 1;
    int y1 = rect.y + fm.getAscent();
    int y2 = ( y1 + fm.getDescent() ) - 1;

    Color fontColor;
    if ( isEnabled() )
    {
      fontColor = getForeground();
    }
    else
    {
      fontColor = UIManager.getColor( "Button.disabledText" );
    }

    aCanvas.setColor( fontColor );
    aCanvas.drawString( text, x1, y1 );

    if ( isEnabled() )
    {
      aCanvas.drawLine( x1, y2, x2, y2 );
    }
  }
}

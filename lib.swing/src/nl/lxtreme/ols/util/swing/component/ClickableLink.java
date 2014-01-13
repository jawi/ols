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
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;
import java.awt.event.*;
import java.beans.*;

import javax.swing.*;


/**
 * Provides a clickable link, which acts like a kind of hyperlink.
 */
public class ClickableLink extends JLabel
{
  // INNER TYPES

  /**
   * Default implementation of {@link LinkTextModel}.
   */
  public static class DefaultLinkTextModel implements LinkTextModel
  {
    private final String text;

    /**
     * Creates a new {@link DefaultLinkTextModel} instance.
     */
    public DefaultLinkTextModel( String aText )
    {
      this.text = aText;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getText()
    {
      return this.text;
    }
  }

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

  /**
   * Used to create dynamic link texts.
   */
  public static interface LinkTextModel
  {
    /**
     * Returns the actual text to display in this link.
     * 
     * @return the link text, never <code>null</code>.
     */
    String getText();
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final LinkTextModel model;
  private final Object linkId;

  private MouseListener mouseListener;

  private volatile LinkListener linkListener;
  private volatile boolean hovered;

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
  public ClickableLink( LinkTextModel aModel, Object aLinkId )
  {
    super();

    this.model = aModel;
    this.linkId = aLinkId;
  }

  /**
   * Creates a new ClickableLink instance.
   * 
   * @param aText
   *          the text of this link, can be <code>null</code>;
   * @param aLinkId
   *          the identifier object to pass to the {@link LinkListener}, if this
   *          link is clicked.
   */
  public ClickableLink( String aText, Object aLinkId )
  {
    this( new DefaultLinkTextModel( aText ), aLinkId );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    super.addNotify();

    setOpaque( true );

    addPropertyChangeListener( new PropertyChangeListener()
    {
      @Override
      public void propertyChange( PropertyChangeEvent aEvt )
      {
        if ( "Frame.active".equals( aEvt.getPropertyName() ) )
        {
          revalidate();
          repaint( 150L );
        }
      }
    } );

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
          hovered = true;

          setCursor( Cursor.getPredefinedCursor( Cursor.HAND_CURSOR ) );

          repaint();
        }
      }

      @Override
      public void mouseExited( final MouseEvent aEvent )
      {
        if ( isEnabled() )
        {
          hovered = false;

          setCursor( Cursor.getDefaultCursor() );

          repaint();
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText()
  {
    if ( this.model == null )
    {
      return "";
    }
    return this.model.getText();
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
   * {@inheritDoc}
   */
  @Override
  public void setText( String aText )
  {
    if ( aText != null && !"".equals( aText.trim() ) )
    {
      throw new UnsupportedOperationException( "Use LinkTextModel instead!" );
    }
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

    Rectangle clip = aCanvas.getClipBounds();

    aCanvas.clearRect( clip.x, clip.y, clip.width, clip.height );

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

    if ( isEnabled() && this.hovered )
    {
      aCanvas.drawLine( x1, y2, x2, y2 );
    }
  }
}

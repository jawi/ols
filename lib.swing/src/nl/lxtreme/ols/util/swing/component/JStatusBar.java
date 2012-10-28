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
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.component.icon.*;


/**
 * 
 */
public class JStatusBar extends JPanel
{
  // INNER TYPES

  static final class SeparatorPanel extends JPanel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final Color leftColor;
    private final Color rightColor;

    // CONSTRUCTORS

    /**
     * 
     */
    public SeparatorPanel( final Color aLeftColor, final Color aRightColor )
    {
      this.leftColor = aLeftColor;
      this.rightColor = aRightColor;
      setOpaque( false );

      setSize( new Dimension( 4, getHeight() ) );
    }

    // METHODS

    /**
     * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
     */
    @Override
    protected void paintComponent( final Graphics aCanvas )
    {
      aCanvas.setColor( this.leftColor );
      aCanvas.drawLine( 0, 0, 0, getHeight() );
      aCanvas.setColor( this.rightColor );
      aCanvas.drawLine( 1, 0, 1, getHeight() );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  protected static final int HEIGHT = 23;
  protected static final Insets INSETS = new Insets( 0, 4, 0, 2 );

  private final JPanel contentPanel;
  private JComponent mainLeftComponent;

  // CONSTRUCTORS

  /**
   * Creates a new {@link JStatusBar}.
   */
  public JStatusBar()
  {
    super();

    setPreferredSize( new Dimension( getWidth(), HEIGHT ) );

    setLayout( new BorderLayout() );

    final JPanel rightPanel = new JPanel( new BorderLayout() );
    rightPanel.setOpaque( false );

    if ( !isMacOSX() )
    {
      final JLabel resizeIconLabel = new JLabel( new TriangleSquareWindowsCornerIcon() );
      resizeIconLabel.setOpaque( false );
      rightPanel.add( resizeIconLabel, BorderLayout.SOUTH );
    }
    else
    {
      final JLabel comp = new JLabel( " " );
      comp.setPreferredSize( new Dimension( 12, 12 ) );
      rightPanel.add( comp, BorderLayout.SOUTH );
    }

    add( rightPanel, BorderLayout.EAST );

    this.contentPanel = new JPanel( new GridBagLayout() );
    this.contentPanel.setOpaque( false );
    add( this.contentPanel, BorderLayout.CENTER );
  }

  // METHODS

  /**
   * Adds a component to this status bar.
   * 
   * @param aComponent
   *          the component to add to the right side of this status bar.
   */
  public void addRightComponent( final JComponent aComponent )
  {
    int i = this.contentPanel.getComponentCount();

    final GridBagConstraints gbc = new GridBagConstraints( i, 0, 1, 1, 0.0, 1.0, GridBagConstraints.CENTER,
        GridBagConstraints.NONE, INSETS, 0, 0 );

    this.contentPanel.add( new SeparatorPanel( Color.GRAY, Color.WHITE ), gbc );

    gbc.anchor = GridBagConstraints.CENTER;
    gbc.gridx++;

    this.contentPanel.add( aComponent, gbc );

    revalidate();
  }

  /**
   * Sets the main component of this status bar.
   * 
   * @param aComponent
   *          the component to set as main component on the left side of this
   *          status bar.
   */
  public void setMainLeftComponent( final JComponent aComponent )
  {
    final GridBagConstraints gbc = new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.WEST,
        GridBagConstraints.HORIZONTAL, INSETS, 0, 0 );

    if ( this.mainLeftComponent != null )
    {
      this.contentPanel.remove( this.mainLeftComponent );
    }

    this.mainLeftComponent = aComponent;
    this.contentPanel.add( this.mainLeftComponent, gbc );

    revalidate();
  }

  /**
   * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
   */
  @Override
  protected void paintComponent( final Graphics aCanvas )
  {
    final Color color1 = getBackground();
    final Color color2 = color1.darker();

    // Paint a gradient from top to bottom
    GradientPaint gp = new GradientPaint( 0, 0, color1, getWidth(), getHeight(), color2 );

    ( ( Graphics2D )aCanvas ).setPaint( gp );
    aCanvas.fillRect( 0, 0, getWidth(), getHeight() );

    super.paintComponent( aCanvas );
  }

  /**
   * Returns whether the current host's operating system is Mac OS X.
   * 
   * @return <code>true</code> if running on Mac OS X, <code>false</code>
   *         otherwise.
   */
  private boolean isMacOSX()
  {
    final String osName = System.getProperty( "os.name" );
    return ( "Mac OS X".equalsIgnoreCase( osName ) );
  }

}

/* EOF */

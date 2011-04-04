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


import javax.swing.*;


/**
 * 
 */
public class JTextStatusBar extends JStatusBar
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final JLabel statusText;
  private final JProgressBar progressBar;

  // CONSTRUCTORS

  /**
   * 
   */
  public JTextStatusBar()
  {
    super();

    this.statusText = new JLabel( " " );
    setMainLeftComponent( this.statusText );

    this.progressBar = new JProgressBar( SwingConstants.HORIZONTAL, 0, 100 );
    addRightComponent( this.progressBar );
  }

  // METHODS

  /**
   * @param aPercentage
   */
  public void setProgress( final int aPercentage )
  {
    showProgressBar( true );
    this.progressBar.setValue( aPercentage );
  }

  /**
   * @param aText
   */
  public void setText( final String aText )
  {
    this.statusText.setText( aText );
  }

  /**
   * @param aVisible
   */
  public void showProgressBar( final boolean aVisible )
  {
    this.progressBar.setVisible( aVisible );
  }
}

/* EOF */

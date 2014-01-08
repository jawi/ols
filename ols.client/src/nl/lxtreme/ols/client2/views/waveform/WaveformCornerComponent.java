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
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.client2.views.UIMgr.*;

import java.awt.*;

import javax.swing.*;


/**
 * Simple component for use in the corner of {@link JScrollPane}s.
 */
class WaveformCornerComponent extends JComponent
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( Graphics aGraphics )
  {
    ( ( Graphics2D )aGraphics ).setBackground( getColor( SIGNALVIEW_BACKGROUND_COLOR, Color.WHITE ) );

    Rectangle clip = aGraphics.getClipBounds();
    aGraphics.clearRect( clip.x, clip.y, clip.width, clip.height );
  }
}

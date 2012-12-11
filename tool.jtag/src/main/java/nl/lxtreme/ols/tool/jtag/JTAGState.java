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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.jtag;


/**
 * Denotes the various states of JTAG.
 */
public enum JTAGState
{
  TEST_LOGIC_RESET( "Test logic reset" ), // State 0
  RUN_TEST_IDLE( "Run test idle" ), // State 1
  SELECT_DR( "Select DR scan" ), // State 2
  CAPTURE_DR( "Capture DR" ), // State 3
  SHIFT_DR( "Shift DR" ), // State 4
  EXIT1_DR( "Exit 1 DR" ), // State 5
  PAUSE_DR( "Pause DR" ), // State 6
  EXIT2_DR( "Exit 2 DR" ), // State 7
  UPDATE_DR( "Update DR" ), // State 8
  SELECT_IR( "Select IR scan" ), // State 9
  CAPTURE_IR( "Capture IR" ), // State 10
  SHIFT_IR( "Shift IR" ), // State 11
  EXIT1_IR( "Exit 1 IR" ), // State 12
  PAUSE_IR( "Pause IR" ), // State 13
  EXIT2_IR( "Exit 2 IR" ), // State 14
  UPDATE_IR( "Update IR" ); // State 15

  private final String displayText;

  /**
   * Creates a new JTAGState instance.
   * 
   * @param aDisplayText
   *          the display text of this JTAGState, cannot be <code>null</code>.
   */
  private JTAGState( final String aDisplayText )
  {
    this.displayText = aDisplayText;
  }

  /**
   * Returns the display text of this state.
   * 
   * @return the display text, never <code>null</code>.
   */
  public String getDisplayText()
  {
    return this.displayText;
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() 
  {
	return getDisplayText();
  }
}

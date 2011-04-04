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
import java.lang.reflect.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.basic.*;


/**
 * Provides a lazy-loaded combobox.
 */
public class JLazyComboBox extends JComboBox
{
  // INNER TYPES

  /**
   * Provides a data provider for lazy loaded comboboxes.
   */
  public static interface ItemProvider
  {
    /**
     * Returns the items.
     * 
     * @return an array of items, cannot be <code>null</code>.
     */
    Object[] getItems();
  }

  /**
   * A popup menu listener that populates the combobox model with items right
   * before it is shown.
   */
  static final class ComboboxPopupListener implements PopupMenuListener
  {
    // CONSTANTS

    private static final Method popupHeightForRowCountMethod;

    static
    {
      // For sake of performance (and readability) do this only once for each
      // time this class is loaded...
      try
      {
        popupHeightForRowCountMethod = BasicComboPopup.class.getDeclaredMethod( "getPopupHeightForRowCount",
            Integer.TYPE );
        popupHeightForRowCountMethod.setAccessible( true );
      }
      catch ( SecurityException exception )
      {
        throw new RuntimeException(
            "Security exception while trying to access BasicComboPopup#getPopupHeightForRowCount!", exception );
      }
      catch ( NoSuchMethodException exception )
      {
        throw new RuntimeException( "No such method: BasicComboPopup#getPopupHeightForRowCount()?!", exception );
      }
    }

    // VARIABLES

    private final ItemProvider itemProvider;

    // CONSTRUCTORS

    /**
     * @param aItemProvider
     */
    public ComboboxPopupListener( final ItemProvider aItemProvider )
    {
      this.itemProvider = aItemProvider;
    }

    // METHODS

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuCanceled(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuCanceled( final PopupMenuEvent aEvent )
    {
      // NO-op
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuWillBecomeInvisible( final PopupMenuEvent aEvent )
    {
      // NO-op
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuWillBecomeVisible( final PopupMenuEvent aEvent )
    {
      final JComboBox combobox = ( JComboBox )aEvent.getSource();
      final Dimension originalPreferredSize = combobox.getPreferredSize();

      // In case the combobox model is empty; we lazy add the ports we currently
      // have to it...
      if ( combobox.getItemCount() <= 0 )
      {
        // By default a mutable combobox model is set;
        final MutableComboBoxModel model = ( MutableComboBoxModel )combobox.getModel();

        final Object[] items = this.itemProvider.getItems();
        for ( Object item : items )
        {
          model.addElement( item );
        }

        correctSize( combobox, originalPreferredSize, items.length );
      }
    }

    /**
     * Corrects the dimensions of both the popup of the given combobox as well
     * as the combobox itself to ensure it remains the same after the items were
     * added. Otherwise, the size is changed and causes weird visual behaviour.
     * 
     * @param aComboBox
     *          the combobox to resize;
     * @param aPreferredSize
     *          the original preferred size of the combobox (before the items
     *          were added), cannot be <code>null</code>;
     * @param aAddedItemCount
     *          the number of added items, >= 0.
     */
    private void correctSize( final JComboBox aComboBox, final Dimension aPreferredSize, final int aAddedItemCount )
    {
      aComboBox.setPreferredSize( aPreferredSize );

      if ( aAddedItemCount > 0 )
      {
        // Idea for this taken from:
        // <http://forums.java.net/jive/message.jspa?messageID=61267>
        final Object comp = aComboBox.getUI().getAccessibleChild( aComboBox, 0 );
        if ( !( comp instanceof BasicComboPopup ) )
        {
          return;
        }

        final BasicComboPopup popup = ( BasicComboPopup )comp;
        final JScrollPane scrollPane = ( JScrollPane )popup.getComponent( 0 );

        final int newWidth = Math.max( scrollPane.getPreferredSize().width, aPreferredSize.width );
        final int newHeight = getPopupHeightForRowCount( popup,
            Math.min( aComboBox.getMaximumRowCount(), aAddedItemCount ) );

        final Dimension size = new Dimension( newWidth, newHeight );
        scrollPane.setPreferredSize( size );
        scrollPane.setMaximumSize( size );
      }
    }

    /**
     * Invokes the getPopupHeightForRowCount method on the given popup.
     * 
     * @param aPopup
     *          the combobox popup to invoke the method on;
     * @param aItemCount
     *          the item count to calculate the height for.
     * @return a popup height, >= 0.
     */
    private int getPopupHeightForRowCount( final BasicComboPopup aPopup, final int aItemCount )
    {
      try
      {
        final Integer result = ( Integer )popupHeightForRowCountMethod.invoke( aPopup, Integer.valueOf( aItemCount ) );
        return result.intValue();
      }
      catch ( IllegalAccessException exception )
      {
        throw new RuntimeException( "BasicComboPopup#getPopupHeightForRowCount not accessible?!", exception );
      }
      catch ( InvocationTargetException exception )
      {
        throw new RuntimeException( "BasicComboPopup#getPopupHeightForRowCount throws exception?!",
            exception.getCause() );
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = -4065089150844005742L;

  // CONSTRUCTORS

  /**
   * Creates a new JLazyComboBox instance.
   * 
   * @param aItemProvider
   *          the item provider to use for populating this combobox, cannot be
   *          <code>null</code>.
   */
  public JLazyComboBox( final ItemProvider aItemProvider )
  {
    super( new DefaultComboBoxModel() );

    addPopupMenuListener( new ComboboxPopupListener( aItemProvider ) );
  }
}

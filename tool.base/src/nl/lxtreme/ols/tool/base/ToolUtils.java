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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.base;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;

import nl.lxtreme.ols.tool.base.ExportAware.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction;


/**
 * 
 */
public class ToolUtils
{
  // INNER TYPES

  /**
   * Provides an action that can be restored to a particular (initial) state.
   */
  public static interface RestorableAction extends Action
  {
    /**
     * Restores the state of the action to its initial state.
     */
    void restore();
  }

  /**
   * Provides an export action, that is capable of exporting the analysis
   * results to either CSV or HTML.
   */
  static final class ExportAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final ExportAware<?> toolDialog;

    // CONSTRUCTORS

    /**
     * Creates new ExportAction instance.
     */
    public ExportAction( final ExportAware<?> aToolDialog )
    {
      super( "Export" );

      this.toolDialog = aToolDialog;

      putValue( SHORT_DESCRIPTION, "Exports the analysis results to file" );
      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_E ) );

      setEnabled( false );
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final Window owner = SwingComponentUtils.getOwningWindow( aEvent );
      final File selectedFile = SwingComponentUtils.showFileSaveDialog( owner, StdFileFilter.CSV, StdFileFilter.HTML );

      if ( selectedFile != null )
      {
        ExportFormat format = ExportFormat.CSV;

        final String filenameExt = HostUtils.getFileExtension( selectedFile );
        if ( "htm".equalsIgnoreCase( filenameExt ) || "html".equalsIgnoreCase( filenameExt ) )
        {
          format = ExportFormat.HTML;
        }

        try
        {
          this.toolDialog.exportToFile( selectedFile, format );
        }
        catch ( IOException exception )
        {
          // TODO
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEnabled()
    {
      return super.isEnabled() && ( this.toolDialog.getLastResult() != null );
    }
  }

  /**
   * Provides an "run" action that is actually starting the (asynchronous) tool
   * worker.
   */
  static final class RunAnalysisAction extends AbstractAction implements RestorableAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final ToolDialog toolDialog;

    // CONSTRUCTORS

    /**
     * Creates a new RunAnalysisAction instance.
     */
    public RunAnalysisAction( final ToolDialog aToolDialog )
    {
      super( "Analyze" );
      restore();

      this.toolDialog = aToolDialog;
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final String name = ( String )getValue( NAME );

      if ( "Abort".equals( name ) )
      {
        cancelToolTask();

        putValue( NAME, "Analyze" );
      }
      else
      {
        if ( startToolTask() )
        {
          // Update the state of this action to denote it can also be used as an
          // abort button...
          putValue( NAME, "Abort" );
          putValue( SHORT_DESCRIPTION, "Aborts current analysis..." );
          putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_ESCAPE ) );
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    public void restore()
    {
      putValue( NAME, "Analyze" );
      putValue( SHORT_DESCRIPTION, "Run analysis" );
      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_ENTER ) );
    }

    /**
     * Cancels the current running tool worker.
     */
    protected boolean cancelToolTask()
    {
      try
      {
        this.toolDialog.cancelTool();
        return true;
      }
      catch ( IllegalStateException exception )
      {
        return false;
      }
    }

    /**
     * @return
     */
    protected boolean startToolTask()
    {
      try
      {
        return this.toolDialog.invokeTool();
      }
      catch ( IllegalStateException exception )
      {
        return false;
      }
    }
  }

  /**
   * Convenience method to create a close button that closes this dialog.
   * 
   * @return a button with a {@link CloseAction} instance, never
   *         <code>null</code>.
   */
  public static JButton createCloseButton()
  {
    return StandardActionFactory.createCloseButton();
  }

  /**
   * Factory method for creating an "export" button that -upon execution- calls
   * the methods {@link #storeToCsvFile(File, Object)} or
   * {@link #storeToHtmlFile(File, Object)}.
   * 
   * @return an "export" button, with keyboard shortcuts enabled, never
   *         <code>null</code>.
   */
  public static JButton createExportButton( final ExportAware<?> aToolDialog )
  {
    final ExportAction action = new ExportAction( aToolDialog );
    final JButton exportButton = new JButton( action );
    SwingComponentUtils.registerKeystroke( exportButton, action, "EXPORT" );
    return exportButton;
  }

  /**
   * Factory method for creating a "run analysis" button that -upon execution-
   * creates and starts the tool worker.
   * 
   * @return a "run analysis" button, with keyboard shortcuts enabled, never
   *         <code>null</code>.
   */
  public static JButton createRunAnalysisButton( final ToolDialog aToolDialog )
  {
    final RunAnalysisAction action = new RunAnalysisAction( aToolDialog );
    final JButton runButton = new JButton( action );
    SwingComponentUtils.registerKeystroke( runButton, action, "RUN-ANALYSIS" );
    return runButton;
  }

  /**
   * @param aComboBox
   * @param aIndex
   */
  public static void setComboBoxIndex( final JComboBox aComboBox, final int aIndex )
  {
    int idx = aIndex;
    if ( idx < 0 )
    {
      idx = aComboBox.getSelectedIndex();
    }
    else if ( idx > aComboBox.getItemCount() )
    {
      idx = idx % aComboBox.getItemCount();
    }
    aComboBox.setSelectedIndex( idx );
  }

  /**
   * Convenience method to show an error message.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>;
   * @param aMessage
   *          the error message to show, cannot be <code>null</code>.
   */
  public static void showErrorMessage( final Window aParent, final String aMessage )
  {
    JOptionPane.showMessageDialog( aParent, aMessage, "Error ...", JOptionPane.ERROR_MESSAGE );
  }

  /**
   * Convenience method to show a (information) message.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>;
   * @param aMessage
   *          the message to show, cannot be <code>null</code>.
   */
  public static void showMessage( final Window aParent, final String aMessage )
  {
    JOptionPane.showMessageDialog( aParent, aMessage );
  }

  /**
   * Convenience method to show a warning message.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>;
   * @param aMessage
   *          the warning message to show, cannot be <code>null</code>.
   */
  public static void showWarningMessage( final Window aParent, final String aMessage )
  {
    JOptionPane.showMessageDialog( aParent, aMessage, "Warning ...", JOptionPane.WARNING_MESSAGE );
  }

}

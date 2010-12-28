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
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;
import java.awt.event.*;
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;


/**
 * @author jawi
 */
public class JErrorDialog extends JDialog implements Closeable
{
  // INNER TYPES

  /**
   * @author jawi
   */
  public static class IncidentInfo implements Serializable
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    /**
     * Short string that will be used as a error header
     */
    private String header;
    /**
     * Basic message that describes incident
     */
    private String basicErrorMessage;
    /**
     * Message that will fully describe the incident with all the available
     * details
     */
    private String detailedErrorMessage;
    /**
     * Optional Throwable that will be used
     */
    private Throwable errorException;

    // CONSTRUCTORS

    /**
     * @param aHeader
     * @param aBasicErrorMessage
     * @param aDetailedErrorMessage
     */
    public IncidentInfo( final String aHeader, final String aBasicErrorMessage, final String aDetailedErrorMessage )
    {
      this( aHeader, aBasicErrorMessage, aDetailedErrorMessage, null );
    }

    /**
     * Main constructor that adds all the information to IncidentInfo
     * 
     * @param aHeader
     * @param aBasicErrorMessage
     * @param aDetailedErrorMesage
     * @param aErrorException
     */
    public IncidentInfo( final String aHeader, final String aBasicErrorMessage, final String aDetailedErrorMesage,
        final Throwable aErrorException )
    {
      this.header = aHeader;
      if ( aBasicErrorMessage != null )
      {
        this.basicErrorMessage = aBasicErrorMessage;
      }
      else
      {
        if ( aErrorException != null )
        {
          this.basicErrorMessage = aErrorException.getLocalizedMessage();
        }
        else
        {
          this.basicErrorMessage = "";
        }
      }
      this.detailedErrorMessage = aDetailedErrorMesage;
      this.errorException = aErrorException;
    }

    /**
     * @param aHeader
     * @param aErrorException
     */
    public IncidentInfo( final String aHeader, final Throwable aErrorException )
    {
      this( aHeader, null, null, aErrorException );
    }

    /**
     * Get the basic error description
     * 
     * @return basic error description
     */
    public String getBasicErrorMessage()
    {
      return this.basicErrorMessage;
    }

    /**
     * Get the detailed error description
     * 
     * @return detailed description
     */
    public String getDetailedErrorMessage()
    {
      return this.detailedErrorMessage;
    }

    /**
     * Get an exception that contains some additional information about the
     * error if provided.
     * 
     * @return exception or null if no exception provided
     */
    public Throwable getErrorException()
    {
      return this.errorException;
    }

    /**
     * Get the current header string
     * 
     * @return header string
     */
    public String getHeader()
    {
      return this.header;
    }

    /**
     * Set the current basic error description
     * 
     * @param basicErrorMessage
     */
    public void setBasicErrorMessage( final String basicErrorMessage )
    {
      this.basicErrorMessage = basicErrorMessage;
    }

    /**
     * Set the detailed description for this error
     * 
     * @param detailedErrorMessage
     */
    public void setDetailedErrorMessage( final String detailedErrorMessage )
    {
      this.detailedErrorMessage = detailedErrorMessage;
    }

    /**
     * Set the exception that may contain additional information about the
     * error.
     * 
     * @param errorException
     */
    public void setErrorException( final Throwable errorException )
    {
      this.errorException = errorException;
    }

    /**
     * Set the current header string
     * 
     * @param header
     */
    public void setHeader( final String header )
    {
      this.header = header;
    }
  }

  /**
   * Listener for Details click events. Alternates whether the details section
   * is visible or not.
   * 
   * @author Richard Bair
   */
  final class DetailsClickEvent implements ActionListener
  {
    /*
     * (non-Javadoc)
     * @see
     * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed( final ActionEvent e )
    {
      setDetailsVisible( !JErrorDialog.this.detailsScrollPane.isVisible() );
    }
  }

  /**
   * Listener for Ok button click events
   */
  final class OkClickEvent implements ActionListener
  {
    /*
     * (non-Javadoc)
     * @see
     * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed( final ActionEvent e )
    {
      // close the window
      setVisible( false );
    }
  }

  private static final long serialVersionUID = 1L;

  /**
   * Text representing expanding the details section of the dialog
   */
  private static final String DETAILS_EXPAND_TEXT = "Details >>";
  /**
   * Text representing contracting the details section of the dialog
   */
  private static final String DETAILS_CONTRACT_TEXT = "Details <<";
  /**
   * Text for the Ok button.
   */
  private static final String OK_BUTTON_TEXT = "Ok";
  /**
   * Icon for the error dialog (stop sign, etc)
   */
  private static final Icon icon = UIManager.getIcon( "OptionPane.warningIcon" );
  /**
   * Error message label
   */
  private JLabel errorMessage;
  /**
   * details text area
   */
  private JTextArea details;
  /**
   * detail button
   */
  private JButton detailButton;
  /**
   * details scroll pane
   */
  private JScrollPane detailsScrollPane;

  private IncidentInfo incidentInfo;

  /**
   * Create a new ErrorDialog with the given Frame as the owner
   * 
   * @param owner
   *          Owner of this error dialog.
   */
  public JErrorDialog( final Window aOwner )
  {
    super( aOwner, "", ModalityType.APPLICATION_MODAL );
    initDialog();
  }

  /**
   * Show the error dialog.
   * 
   * @param aOwner
   *          Owner of this error dialog.
   * @param aInfo
   *          <code>IncidentInfo</code> that incorporates all the information
   *          about the error
   */
  public static void showDialog( final Window aOwner, final IncidentInfo aInfo )
  {
    final JErrorDialog dlg = new JErrorDialog( aOwner );
    dlg.setTitle( aInfo.getHeader() );
    dlg.setErrorMessage( aInfo.getBasicErrorMessage() );

    String details = aInfo.getDetailedErrorMessage();
    if ( details == null )
    {
      if ( aInfo.getErrorException() != null )
      {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter( sw );
        aInfo.getErrorException().printStackTrace( pw );
        details = sw.toString();
      }
      else
      {
        details = "";
      }
    }
    dlg.setDetails( details );
    dlg.setIncidentInfo( aInfo );
    dlg.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
    dlg.pack();
    dlg.setLocationRelativeTo( aOwner );
    dlg.setVisible( true );
  }

  /**
   * Show the error dialog.
   * 
   * @param owner
   *          Owner of this error dialog
   * @param title
   *          Title of the error dialog
   * @param errorMessage
   *          Message for the error dialog
   * @param details
   *          Details to be shown in the detail section of the dialog. This can
   *          be null if you do not want to display the details section of the
   *          dialog.
   */
  public static void showDialog( final Window aOwner, final String aTitle, final String aErrorMessage,
      final String aDetails )
  {
    showDialog( aOwner, new IncidentInfo( aTitle, aErrorMessage, aDetails ) );
  }

  /**
   * Constructs and shows the error dialog for the given exception. The
   * exceptions message will be the errorMessage, and the stacktrace will be the
   * details.
   * 
   * @param aOwner
   *          Owner of this error dialog.
   * @param aTitle
   *          Title of the error dialog
   * @param aError
   *          Exception that contains information about the error cause and
   *          stack trace
   */
  public static void showDialog( final Window aOwner, final String aTitle, final Throwable aError )
  {
    showDialog( aOwner, new IncidentInfo( aTitle, null, null, aError ) );
  }

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  @Override
  public void close()
  {
    setVisible( false );
  }

  /**
   * Set the details section of the error dialog. If the details are either null
   * or an empty string, then hide the details button and hide the detail scroll
   * pane. Otherwise, just set the details section.
   * 
   * @param aDetails
   *          Details to be shown in the detail section of the dialog. This can
   *          be null if you do not want to display the details section of the
   *          dialog.
   */
  final void setDetails( final String aDetails )
  {
    setDetailsVisible( false );
    if ( ( aDetails == null ) || aDetails.trim().isEmpty() )
    {
      this.detailButton.setVisible( false );
    }
    else
    {
      this.details.setText( aDetails );
      this.detailButton.setVisible( true );
    }
  }

  /**
   * Set the details section to be either visible or invisible. Set the text of
   * the Details button accordingly.
   * 
   * @param aVisible
   *          if true details section will be visible
   */
  final void setDetailsVisible( final boolean aVisible )
  {
    if ( aVisible )
    {
      this.details.setCaretPosition( 0 );
      this.detailsScrollPane.setVisible( true );
      this.detailButton.setText( DETAILS_CONTRACT_TEXT );
    }
    else
    {
      this.detailsScrollPane.setVisible( false );
      this.detailButton.setText( DETAILS_EXPAND_TEXT );
    }
    pack();
  }

  /**
   * Set the error message for the dialog box
   * 
   * @param aErrorMessage
   *          Message for the error dialog
   */
  final void setErrorMessage( final String aErrorMessage )
  {
    this.errorMessage.setText( aErrorMessage );
  }

  /**
   * Get curent dialog's IncidentInfo
   * 
   * @return <code>IncidentInfo</code> assigned to this dialog
   */
  protected IncidentInfo getIncidentInfo()
  {
    return this.incidentInfo;
  }

  /**
   * Sets the IncidentInfo for this dialog
   * 
   * @param info
   *          IncidentInfo that incorporates all the details about the error
   */
  protected void setIncidentInfo( final IncidentInfo info )
  {
    this.incidentInfo = info;
  }

  /**
   * initialize the gui.
   */
  private void initDialog()
  {
    final Container contentPane = this.getContentPane();
    contentPane.setLayout( new GridBagLayout() );

    final GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = GridBagConstraints.CENTER;
    gbc.fill = GridBagConstraints.NONE;
    gbc.gridheight = 1;
    gbc.insets = new Insets( 22, 12, 11, 17 );
    contentPane.add( new JLabel( icon ), gbc );

    this.errorMessage = new JLabel();
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.BOTH;
    gbc.gridheight = 1;
    gbc.gridwidth = 2;
    gbc.gridx = 1;
    gbc.weightx = 1.0;
    gbc.insets = new Insets( 12, 0, 0, 11 );
    contentPane.add( this.errorMessage, gbc );

    gbc.fill = GridBagConstraints.NONE;
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.gridwidth = 1;
    gbc.weightx = 1.0;
    gbc.weighty = 0.0;
    gbc.anchor = GridBagConstraints.EAST;
    gbc.insets = new Insets( 12, 0, 11, 5 );
    JButton okButton = new JButton( OK_BUTTON_TEXT );
    contentPane.add( okButton, gbc );

    this.detailButton = new JButton( DETAILS_EXPAND_TEXT );
    gbc.gridx = 3;
    gbc.weightx = 0.0;
    gbc.insets = new Insets( 12, 0, 11, 11 );
    contentPane.add( this.detailButton, gbc );

    this.details = new JTextArea( 7, 60 );
    this.detailsScrollPane = new JScrollPane( this.details );
    this.detailsScrollPane.setVerticalScrollBarPolicy( ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS );
    this.details.setEditable( false );
    gbc.fill = GridBagConstraints.BOTH;
    gbc.gridwidth = 4;
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.weighty = 1.0;
    gbc.insets = new Insets( 6, 11, 11, 11 );
    contentPane.add( this.detailsScrollPane, gbc );
    /*
     * Here i'm going to add invisible empty container to the bottom of the
     * content pane to fix minimal width of the dialog. It's quite a hack, but i
     * have not found anything better.
     */
    Dimension spPredictedSize = this.detailsScrollPane.getPreferredSize();
    Dimension newPanelSize = new Dimension( spPredictedSize.width + 15, 0 );
    Container widthHolder = new Container();
    widthHolder.setMinimumSize( newPanelSize );
    widthHolder.setPreferredSize( newPanelSize );
    widthHolder.setMaximumSize( newPanelSize );
    gbc.gridy = 3;
    gbc.insets = new Insets( 0, 11, 11, 0 );
    contentPane.add( widthHolder, gbc );

    // make the buttons the same size
    int buttonLength = this.detailButton.getPreferredSize().width;
    int buttonHeight = this.detailButton.getPreferredSize().height;
    Dimension buttonSize = new Dimension( buttonLength, buttonHeight );
    okButton.setPreferredSize( buttonSize );
    this.detailButton.setPreferredSize( buttonSize );
    // set the event handling
    okButton.addActionListener( new OkClickEvent() );

    this.detailButton.addActionListener( new DetailsClickEvent() );
  }
}

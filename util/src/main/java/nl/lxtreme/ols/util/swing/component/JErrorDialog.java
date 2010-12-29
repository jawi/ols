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
import java.lang.Thread.*;
import java.net.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;


/**
 * Provides an error dialog for displaying exceptions in a more friendly way.
 * <p>
 * This code is largely based on the JXErrorDialog code from the SwingX project.
 * Some of that code is reshuffled and/or modified to fit into the architecture
 * of OLS.
 * </p>
 */
public class JErrorDialog extends JDialog implements Closeable
{
  // INNER TYPES

  /**
   * Provides a container with information about the incident.
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
   *
   */
  static final class IncidentMailReporter
  {
    // VARIABLES

    private final String mailAddress;

    // CONSTRUCTORS

    /**
     * 
     */
    public IncidentMailReporter( final String aMailAddress )
    {
      this.mailAddress = aMailAddress;
    }

    // METHODS

    /**
     * @param aIncident
     */
    public void reportIncident( final IncidentInfo aIncident, final String aHostDetails ) throws IOException
    {
      final String uriStr = String.format( "mailto:%s?subject=%s&body=%s", //
          this.mailAddress, //
          urlEncode( "Crash Reporter" ), //
          urlEncode( getMessageBody( aIncident, aHostDetails ) ) );

      try
      {
        final URI mailURI = new URI( uriStr );
        Desktop.getDesktop().browse( mailURI );
      }
      catch ( URISyntaxException exception )
      {
        throw new IOException( "Unsupported URI: mailto!", exception );
      }
    }

    /**
     * @param aIncident
     * @return
     */
    private String getMessageBody( final IncidentInfo aIncident, final String aHostDetails )
    {
      final boolean debug = Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) );

      StringBuilder body = new StringBuilder();

      if ( aHostDetails != null )
      {
        body.append( "Host information: " ).append( aHostDetails );
      }
      else
      {
        body.append( "No host information found/provided!" );
      }
      body.append( '\n' );

      if ( debug )
      {
        body.append( "Debugging mode is ENABLED!\n" );
      }

      final Throwable error = aIncident.getErrorException();
      if ( error != null )
      {
        final StringWriter sw = new StringWriter();
        final PrintWriter pw = new PrintWriter( sw );
        error.printStackTrace( pw );
        body.append( "\nStack trace: " ).append( "\n-----\n" ).append( sw.toString() ).append( "\n-----\n" );
      }
      body.append( '\n' );

      return body.toString();
    }

    /**
     * @param aString
     * @return
     */
    private String urlEncode( final String aString )
    {
      try
      {
        return URLEncoder.encode( aString, "UTF-8" ).replace( "+", "%20" );
      }
      catch ( UnsupportedEncodingException exception )
      {
        throw new RuntimeException( exception );
      }
    }
  }

  /**
   * Provides a exception handler for the JVM exception hook.
   */
  static final class SwingUncaughtExceptionHandler implements UncaughtExceptionHandler
  {
    // METHODS

    /**
     * @see java.lang.Thread.UncaughtExceptionHandler#uncaughtException(java.lang.Thread,
     *      java.lang.Throwable)
     */
    @Override
    public void uncaughtException( final Thread aThread, final Throwable aException )
    {
      final Window owner = SwingComponentUtils.getCurrentWindow();
      final IncidentInfo incident = new IncidentInfo( "Uncaught exception...", //
          "<html><b>Something unexpected happened!</b><br><br>"
              + "Click on \"more details\" for more information about the possible cause.<br><br>"
              + "If the problem persists, please report it as bug.</html>", "", aException );
      JErrorDialog.showDialog( owner, incident );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /**
   * Text representing extracting the details section of this dialog.
   */
  private static final String MORE_DETAILS = "More details";
  /**
   * Text representing contracting the details section of this dialog.
   */
  private static final String LESS_DETAILS = "Less details";
  /**
   * Text representing the reporting button of this dialog.
   */
  private static final String REPORT = "Report";
  /**
   * Icon for the error dialog (stop sign, etc)
   */
  private static final Icon ICON = UIManager.getIcon( "OptionPane.warningIcon" );

  /** To ensure there's at most one error dialog on screen at all times. */
  private static volatile JErrorDialog instance = null;
  private static String reportIncidentAddress;
  private static String hostDetails;

  // VARIABLES

  private JLabel errorMessage;
  private JTextArea details;
  private JButton detailButton;
  private JScrollPane detailsScrollPane;
  private JButton reportButton;

  private final IncidentInfo incidentInfo;

  // CONSTRUCTORS

  /**
   * Create a new JErrorDialog with the given window as the owner.
   * 
   * @param aOwner
   *          Owner of this error dialog.
   * @param aInfo
   */
  protected JErrorDialog( final Window aOwner, final IncidentInfo aInfo )
  {
    super( aOwner, "", ModalityType.APPLICATION_MODAL );

    this.incidentInfo = aInfo;

    initDialog();

    String details = aInfo.getDetailedErrorMessage();
    if ( ( details == null ) || details.trim().isEmpty() )
    {
      final Throwable error = aInfo.getErrorException();
      if ( error != null )
      {
        final StringWriter sw = new StringWriter();
        final PrintWriter pw = new PrintWriter( sw );
        error.printStackTrace( pw );
        details = sw.toString();
      }
      else
      {
        details = "";
      }
    }

    setTitle( aInfo.getHeader() );
    setErrorMessage( aInfo.getBasicErrorMessage() );
    setDetails( details );

    setLocationRelativeTo( aOwner );
  }

  // METHODS

  /**
   * Installs a Swing-capable default exception handler.
   * <p>
   * Calling this method will cause <em>all</em> uncaught exceptions, for which
   * <b>no</b> exception handling is done, to be displayed in this error dialog.
   * </p>
   */
  public static void installSwingExceptionHandler()
  {
    Thread.setDefaultUncaughtExceptionHandler( new SwingUncaughtExceptionHandler() );
  }

  /**
   * Sets the email address used for reporting incidents.
   * 
   * @param aReportIncidentAddress
   *          the report incident email address to set, may be <code>null</code>
   *          to disable reporting functionality.
   */
  public static void setHostInformation( final String aOSName, final String aOSVersion, final String aProcessor )
  {
    hostDetails = String.format( "%s, %s (%s)", aOSName, aOSVersion, aProcessor );
  }

  /**
   * Sets the email address used for reporting incidents.
   * 
   * @param aReportIncidentAddress
   *          the report incident email address to set, may be <code>null</code>
   *          to disable reporting functionality.
   */
  public static void setReportIncidentAddress( final String aReportIncidentAddress )
  {
    reportIncidentAddress = aReportIncidentAddress;
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
  public static synchronized void showDialog( final Window aOwner, final IncidentInfo aInfo )
  {
    if ( instance == null )
    {
      instance = new JErrorDialog( aOwner, aInfo );
      instance.setVisible( true );
      // Wait until we're closed...
      instance.dispose();
      instance = null;
    }
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
   * Reports the incident through mail.
   */
  final void reportIncident() throws IOException
  {
    final IncidentMailReporter reporter = new IncidentMailReporter( reportIncidentAddress );
    reporter.reportIncident( getIncidentInfo(), hostDetails );
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
      this.detailButton.setText( LESS_DETAILS );
    }
    else
    {
      this.detailsScrollPane.setVisible( false );
      this.detailButton.setText( MORE_DETAILS );
    }
    pack();
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
   * Creates the buttons pane.
   * 
   * @return a button pane, never <code>null</code>.
   */
  private JComponent createButtonPane()
  {
    final boolean reportingEnabled = ( reportIncidentAddress != null ) && !reportIncidentAddress.trim().isEmpty();

    this.reportButton = new JButton( REPORT );
    this.reportButton.setVisible( reportingEnabled );
    this.reportButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        try
        {
          reportIncident();
        }
        catch ( IOException exception )
        {
          final Window parent = SwingComponentUtils.getOwningWindow( aEvent );
          JOptionPane.showMessageDialog( parent, exception.getMessage(), "Reporting failed!", JOptionPane.ERROR_MESSAGE );
        }
      }
    } );

    final JButton cancel = StandardActionFactory.createCloseButton();

    this.detailButton = new JButton( MORE_DETAILS );
    this.detailButton.addActionListener( new ActionListener()
    {
      /**
       * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
       */
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        setDetailsVisible( !JErrorDialog.this.detailsScrollPane.isVisible() );
      }
    } );

    return SwingComponentUtils.createButtonPane( new JButton[] { this.reportButton, this.detailButton, cancel } );
  }

  /**
   * @return
   */
  private JPanel createDetailsPane()
  {
    this.errorMessage = new JLabel();

    this.details = new JTextArea( 8, 50 );
    this.details.setFont( Font.decode( Font.MONOSPACED ) );
    this.details.setEditable( false );

    this.detailsScrollPane = new JScrollPane( this.details );
    this.detailsScrollPane.setVerticalScrollBarPolicy( ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS );

    final JLabel icon = new JLabel( ICON );
    icon.setBorder( BorderFactory.createEmptyBorder( 16, 0, 12, 24 ) );

    final JPanel topPanel = new JPanel( new BorderLayout() );
    topPanel.setBorder( BorderFactory.createEmptyBorder( 0, 12, 12, 12 ) );
    topPanel.add( icon, BorderLayout.WEST );
    topPanel.add( this.errorMessage, BorderLayout.CENTER );

    final JPanel detailPanel = new JPanel( new BorderLayout() );
    detailPanel.setBorder( BorderFactory.createEmptyBorder( 12, 6, 12, 6 ) );
    detailPanel.add( topPanel, BorderLayout.NORTH );
    detailPanel.add( this.detailsScrollPane, BorderLayout.CENTER );

    return detailPanel;
  }

  /**
   * Initialize this dialog.
   */
  private void initDialog()
  {
    final JPanel contentPane = createDetailsPane();
    final JComponent buttonPane = createButtonPane();

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttonPane );

    setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
    pack();

    /*
     * final Container contentPane = getContentPane(); contentPane.setLayout(
     * new GridBagLayout() ); final GridBagConstraints gbc = new
     * GridBagConstraints(); gbc.anchor = GridBagConstraints.CENTER; gbc.fill =
     * GridBagConstraints.NONE; gbc.gridheight = 1; gbc.insets = new Insets( 22,
     * 12, 11, 17 ); contentPane.add( new JLabel( ICON ), gbc ); gbc.anchor =
     * GridBagConstraints.WEST; gbc.fill = GridBagConstraints.BOTH;
     * gbc.gridheight = 1; gbc.gridwidth = 2; gbc.gridx = 1; gbc.weightx = 1.0;
     * gbc.insets = new Insets( 12, 0, 0, 11 ); contentPane.add(
     * this.errorMessage, gbc ); gbc.fill = GridBagConstraints.NONE; gbc.gridx =
     * 1; gbc.gridy = 1; gbc.gridwidth = 1; gbc.weightx = 1.0; gbc.weighty =
     * 0.0; gbc.anchor = GridBagConstraints.EAST; gbc.insets = new Insets( 12,
     * 0, 11, 5 ); contentPane.add( okButton, gbc ); gbc.gridx = 3; gbc.weightx
     * = 0.0; gbc.insets = new Insets( 12, 0, 11, 11 ); contentPane.add(
     * this.detailButton, gbc ); gbc.fill = GridBagConstraints.BOTH;
     * gbc.gridwidth = 4; gbc.gridx = 0; gbc.gridy = 2; gbc.weighty = 1.0;
     * gbc.insets = new Insets( 6, 11, 11, 11 ); contentPane.add(
     * this.detailsScrollPane, gbc ); / * Here i'm going to add invisible empty
     * container to the bottom of the content pane to fix minimal width of the
     * dialog. It's quite a hack, but i have not found anything better. /
     * Dimension spPredictedSize = this.detailsScrollPane.getPreferredSize();
     * Dimension newPanelSize = new Dimension( spPredictedSize.width + 15, 0 );
     * Container widthHolder = new Container(); widthHolder.setMinimumSize(
     * newPanelSize ); widthHolder.setPreferredSize( newPanelSize );
     * widthHolder.setMaximumSize( newPanelSize ); gbc.gridy = 3; gbc.insets =
     * new Insets( 0, 11, 11, 0 ); contentPane.add( widthHolder, gbc );
     */
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
  private void setDetails( final String aDetails )
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
   * Set the error message for the dialog box
   * 
   * @param aErrorMessage
   *          Message for the error dialog
   */
  private void setErrorMessage( final String aErrorMessage )
  {
    this.errorMessage.setText( aErrorMessage );
  }
}

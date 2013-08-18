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


import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.Font;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URI;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import nl.lxtreme.ols.util.HostUtils;
import nl.lxtreme.ols.util.swing.StandardActionFactory;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.SwingComponentUtils;


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
    private final String header;
    /**
     * Basic message that describes incident
     */
    private final String basicErrorMessage;
    /**
     * Message that will fully describe the incident with all the available
     * details
     */
    private final String detailedErrorMessage;
    /**
     * Optional Throwable that will be used
     */
    private final Throwable errorException;

    // CONSTRUCTORS

    /**
     * Creates a new IncidentInfo instance.
     * 
     * @param aHeader
     *          the header of this incident;
     * @param aBasicErrorMessage
     *          the short/human readable version of this incident;
     * @param aDetailedErrorMessage
     *          the detailed version of this incident (stack trace).
     */
    public IncidentInfo( final String aHeader, final String aBasicErrorMessage, final String aDetailedErrorMessage )
    {
      this( aHeader, aBasicErrorMessage, aDetailedErrorMessage, null );
    }

    /**
     * Main constructor that adds all the information to IncidentInfo
     * 
     * @param aHeader
     *          the header of this incident;
     * @param aBasicErrorMessage
     *          the short/human readable version of this incident;
     * @param aDetailedErrorMessage
     *          the detailed version of this incident (stack trace);
     * @param aErrorException
     *          the exception to show in this incident.
     */
    public IncidentInfo( final String aHeader, final String aBasicErrorMessage, final String aDetailedErrorMessage,
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
      this.detailedErrorMessage = aDetailedErrorMessage;
      this.errorException = aErrorException;
    }

    /**
     * Creates a new IncidentInfo instance.
     * 
     * @param aHeader
     *          the header of this incident;
     * @param aErrorException
     *          the exception to show in this incident.
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
  }

  /**
   *
   */
  static final class IncidentMailReporter
  {
    // CONSTANTS

    /**
     * For some reason URIs on Windows machines cannot be longer than 1700
     * characters...
     */
    private static final int MAGIC_WINDOWS_URI_LIMIT = 1700;

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
     * Reports a given incident by mail.
     * 
     * @param aIncident
     *          the incident to report through mail, cannot be <code>null</code>
     *          .
     */
    public void reportIncident( final IncidentInfo aIncident )
    {
      String uriStr = String.format( "%s?subject=%s&body=%s", //
          this.mailAddress, //
          "Crash Reporter", //
          formatIncident( aIncident ) );

      try
      {
        final URI mailURI;

        if ( HostUtils.getHostInfo().isWindows() && ( uriStr.length() > MAGIC_WINDOWS_URI_LIMIT ) )
        {
          mailURI = new URI( "mailto", uriStr.substring( 0, MAGIC_WINDOWS_URI_LIMIT ), null );
          uriStr = null;
        }
        else
        {
          mailURI = new URI( "mailto", uriStr, null );
          uriStr = null;
        }

        if ( Desktop.isDesktopSupported() )
        {
          Desktop.getDesktop().mail( mailURI );
        }
      }
      catch ( Exception exception )
      {
        File report = writeIncidentReport( aIncident );
        throw new RuntimeException( "Cannot open mail client!\nInstead, the report is written to disk. Please mail the contents of:\n"
                + report.getPath() + "\nto:\n" + this.mailAddress + ", or post it on <https://github.com/jawi/ols/issues>." );
      }
    }

    /**
     * Formats the given incident information into a proper text document.
     * 
     * @param aIncident
     *          the incident information to format to text, cannot be
     *          <code>null</code>.
     * @return a text-based version of the given incident information.
     */
    private String formatIncident( final IncidentInfo aIncident )
    {
      final String osName = System.getProperty( "os.name", "unknown" );
      final String osVersion = System.getProperty( "os.version", "unknown" );
      final String processor = System.getProperty( "os.arch", "unknown" );
      final String hostDetails = String.format( "%s, %s (%s)", osName, osVersion, processor );

      final String javaVendor = System.getProperty( "java.vendor", "unknown" );
      final String javaVersion = System.getProperty( "java.version", "unknown" );
      final String javaDetails = String.format( "%s v%s", javaVendor, javaVersion );

      final boolean debug = Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) );
      final String version = System.getProperty( "nl.lxtreme.ols.client.version", "<unknown>" );

      StringBuilder body = new StringBuilder();

      body.append( "Client version  : " ).append( version ).append( '\n' );
      body.append( "Java information: " ).append( javaDetails ).append( '\n' );
      body.append( "Host information: " ).append( hostDetails ).append( '\n' );

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
     * Writes the given incident information down to a plain file in the user's
     * home directory.
     * 
     * @param aIncident
     *          the incident information to write to a file, cannot be
     *          <code>null</code>.
     * @return the written file, never <code>null</code>.
     */
    private File writeIncidentReport( final IncidentInfo aIncident )
    {
      String reportName = String.format( "ols-report-%x.txt", Long.valueOf( System.nanoTime() ) );
      String homeDir = System.getProperty( "user.home" );

      File report = new File( homeDir, reportName );
      FileOutputStream fos = null;
      try
      {
        fos = new FileOutputStream( report );
        fos.write( formatIncident( aIncident ).getBytes() );
        fos.flush();
      }
      catch ( IOException exception )
      {
        throw new RuntimeException( "Tried to write an incident report to disk, but even that failed!", exception );
      }
      finally
      {
        if ( fos != null )
        {
          try
          {
            fos.close();
          }
          catch ( IOException ignored )
          {
            // Ignore; nothing we can do...
          }
        }
      }

      return report;
    }
  }

  /**
   * Provides a exception handler for the JVM exception hook.
   */
  static final class SwingUncaughtExceptionHandler implements UncaughtExceptionHandler
  {
    // METHODS

    /**
     * Called by the EDT in case an exception occurs on it. See
     * <http://stackoverflow.com/questions/4448523/how-can-i-catch-event-
     * dispatch-thread-edt-exceptions/4448569#4448569> for more
     * information/rationale.
     * 
     * @param aException
     *          the exception thrown.
     */
    public void handle( final Throwable aException )
    {
      uncaughtException( Thread.currentThread(), aException );
    }

    /**
     * @see java.lang.Thread.UncaughtExceptionHandler#uncaughtException(java.lang.Thread,
     *      java.lang.Throwable)
     */
    @Override
    public void uncaughtException( final Thread aThread, final Throwable aException )
    {
      // Make sure we dump the exception on the console...
      aException.printStackTrace( System.err );

      final Window owner = SwingComponentUtils.getCurrentWindow();
      final IncidentInfo incident = new IncidentInfo( "Uncaught exception...", //
          "<html><b>Something unexpected happened!</b><br><br>"
              + "Click on \"more details\" for more information about the possible cause.<br><br>"
              + "If the problem persists, please report it as bug.</html>", "", aException );

      final Runnable task = new Runnable()
      {
        public void run()
        {
          JErrorDialog.showDialog( owner, incident );
        }
      };

      if ( SwingUtilities.isEventDispatchThread() )
      {
        task.run();
      }
      else
      {
        SwingUtilities.invokeLater( task );
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** System property to read for the incident email address. */
  public static final String PROPERTY_REPORT_INCIDENT_EMAIL_ADDRESS = "nl.lxtreme.ols.report_incident_email_addr";

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
   * Text representing the quit button of this dialog.
   */
  private static final String QUIT = "Quit";
  /**
   * Icon for the error dialog (stop sign, etc)
   */
  private static final Icon ICON = UIManager.getIcon( "OptionPane.warningIcon" );

  // VARIABLES

  private JLabel errorMessage;
  private JTextArea details;
  private JButton detailButton;
  private JScrollPane detailsScrollPane;
  private JButton reportButton;
  private IncidentInfo incidentInfo;

  // CONSTRUCTORS

  /**
   * Create a new JErrorDialog with the given window as the owner.
   * 
   * @param aOwner
   *          the Owner of this error dialog;
   * @param aInfo
   *          the incident information to show, cannot be <code>null</code>.
   */
  protected JErrorDialog( final Window aOwner, final IncidentInfo aInfo )
  {
    super( aOwner, "", ModalityType.APPLICATION_MODAL );

    initDialog();

    setIncident( aInfo );

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
    JErrorDialog errorDialog = null;
    // First try to find out whether we're already showing an error dialog on
    // screen. If so, we need to update that one instead of displaying a new
    // one...
    for ( Window window : Window.getWindows() )
    {
      if ( window instanceof JErrorDialog )
      {
        // Hmm, already showing an error dialog... Let's reuse that one...
        errorDialog = ( JErrorDialog )window;
        break;
      }
    }

    if ( errorDialog == null )
    {
      errorDialog = new JErrorDialog( aOwner, aInfo );
      errorDialog.setVisible( true );
    }
    else
    {
      errorDialog.setIncident( aInfo );
      if ( !errorDialog.isVisible() )
      {
        errorDialog.setVisible( true );
      }
      errorDialog.requestFocus();
    }
  }

  /**
   * Show the error dialog.
   * 
   * @param aOwner
   *          the owner of this error dialog
   * @param aTitle
   *          the title of the error dialog
   * @param aErrorMessage
   *          the error message for the error dialog
   * @param aDetails
   *          the details to be shown in the detail section of the dialog. This
   *          can be <code>null</code> if you do not want to display the details
   *          section of the dialog.
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
   * Uninstalls the default exception handler.
   */
  public static void uninstallSwingExceptionHandler()
  {
    Thread.setDefaultUncaughtExceptionHandler( null );
  }

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * Reports the incident through mail.
   */
  final void reportIncident()
  {
    final IncidentMailReporter reporter = new IncidentMailReporter( getReportIncidentAddress() );
    reporter.reportIncident( getIncidentInfo() );
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
   * Returns the email address to report incidents to.
   * 
   * @return the report incident email address, can be <code>null</code> if this
   *         address is not defined.
   */
  private String getReportIncidentAddress()
  {
    final Object property = System.getProperty( PROPERTY_REPORT_INCIDENT_EMAIL_ADDRESS, "" );
    final String result = String.valueOf( property ).trim();
    return result.isEmpty() ? null : result;
  }

  /**
   * Initialize this dialog.
   */
  private void initDialog()
  {
    final JPanel contentPane = createDetailsPane();
    final boolean reportingEnabled = getReportIncidentAddress() != null;

    this.reportButton = new JButton( REPORT );
    this.reportButton.setVisible( reportingEnabled );
    this.reportButton.setEnabled( Desktop.isDesktopSupported() );
    this.reportButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        try
        {
          reportIncident();
        }
        catch ( RuntimeException exception )
        {
          final Window parent1 = SwingComponentUtils.getOwningWindow( aEvent );
          JOptionPane.showMessageDialog( parent1, exception.getMessage(), "Reporting failed!",
              JOptionPane.ERROR_MESSAGE );
        }
      }
    } );

    final JButton cancel = StandardActionFactory.createCloseButton();

    this.detailButton = new JButton( MORE_DETAILS );
    this.detailButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        setDetailsVisible( !JErrorDialog.this.detailsScrollPane.isVisible() );
      }
    } );

    final JButton quit = new JButton( QUIT );
    quit.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        // Abort the VM; do *not* try to use any OSGi hooks or whatever to go
        // down gracefully; we're in a situation that might be very hairy to get
        // out of...
        System.exit( 1 );
      }
    } );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( quit, this.reportButton, this.detailButton,
        cancel );

    SwingComponentUtils.setupWindowContentPane( this, contentPane, buttonPane, cancel );

    pack();
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

  /**
   * Sets the incident information.
   * 
   * @param aIncident
   *          the incident information to show, cannot be <code>null</code>.
   */
  private void setIncident( final IncidentInfo aIncident )
  {
    this.incidentInfo = aIncident;

    String details = aIncident.getDetailedErrorMessage();
    if ( ( details == null ) || details.trim().isEmpty() )
    {
      final Throwable error = aIncident.getErrorException();
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

    setTitle( aIncident.getHeader() );
    setErrorMessage( aIncident.getBasicErrorMessage() );
    setDetails( details );
  }
}

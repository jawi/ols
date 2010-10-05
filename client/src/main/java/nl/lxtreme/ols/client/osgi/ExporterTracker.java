/**
 * 
 */
package nl.lxtreme.ols.client.osgi;


import javax.swing.*;

import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * @author jawi
 */
public class ExporterTracker extends ServiceTracker
{
  // VARIABLES

  private final ClientController controller;

  // CONSTRUCTORS

  /**
   * @param aContext
   *          the bundle context to use;
   * @param aController
   *          the client controller to use.
   */
  public ExporterTracker( final BundleContext aContext, final ClientController aController )
  {
    super( aContext, Exporter.class.getName(), null );
    this.controller = aController;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final Exporter exporter = ( Exporter )this.context.getService( aReference );

    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        ExporterTracker.this.controller.addExporter( exporter );
      }
    } );

    return exporter;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    final Exporter exporter = ( Exporter )aService;

    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        ExporterTracker.this.controller.removeExporter( exporter );
      }
    } );

    // TODO Auto-generated method stub
    super.removedService( aReference, aService );
  }
}

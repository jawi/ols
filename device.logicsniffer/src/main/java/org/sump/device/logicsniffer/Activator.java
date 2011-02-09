/**
 * 
 */
package org.sump.device.logicsniffer;


import java.util.*;

import org.osgi.framework.*;
import org.osgi.service.cm.*;


/**
 * @author jawi
 */
public class Activator implements BundleActivator
{
  // VARIABLES

  private ServiceRegistration serviceRegistration;

  // METHODS

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    Dictionary<String, String> props = new Hashtable<String, String>();
    props.put( Constants.SERVICE_PID, DeviceProfileFactory.SERVICE_PID );

    this.serviceRegistration = aContext.registerService( ManagedServiceFactory.class.getName(),
        new DeviceProfileFactory(), props );

  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    if ( this.serviceRegistration != null )
    {
      this.serviceRegistration.unregister();
      this.serviceRegistration = null;
    }
  }
}

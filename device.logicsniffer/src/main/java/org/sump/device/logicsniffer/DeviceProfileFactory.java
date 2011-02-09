/**
 * 
 */
package org.sump.device.logicsniffer;


import java.util.*;

import org.osgi.service.cm.*;


/**
 * @author jawi
 */
public class DeviceProfileFactory implements ManagedServiceFactory
{
  // CONSTANTS

  public static final String SERVICE_PID = "ols.profile";

  // VARIABLES

  private final Map<String, Object> profiles;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfileFactory instance.
   */
  public DeviceProfileFactory()
  {
    this.profiles = new HashMap<String, Object>();
  }

  // METHODS

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#deleted(java.lang.String)
   */
  @Override
  public void deleted( final String aPid )
  {
    synchronized ( this.profiles )
    {
      this.profiles.remove( aPid );
    }
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#getName()
   */
  @Override
  public String getName()
  {
    return "LogicSniffer device profile factory";
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#updated(java.lang.String,
   *      java.util.Dictionary)
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final String aPid, final Dictionary aProperties ) throws ConfigurationException
  {
    synchronized ( this.profiles )
    {
      if ( this.profiles.containsKey( aPid ) )
      {
        Object profile = this.profiles.get( aProperties );
        // TODO update profile object with given properties...
      }
      else
      {
        // TODO create device profile object...
        this.profiles.put( aPid, new Object() );
      }
    }
  }

}

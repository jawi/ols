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
package nl.lxtreme.ols.device.logicsniffer.profile;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.device.logicsniffer.*;

import org.osgi.service.cm.*;


/**
 * Provides a device profile manager,
 */
public class DeviceProfileManager implements ManagedServiceFactory, DeviceProfileProvider
{
  // CONSTANTS

  public static final String SERVICE_PID = "ols.profile";

  /** The default profile type to use. See {@link #getProfile(String)}. */
  private static final String DEFAULT_PROFILE_TYPE = "OLS";

  // VARIABLES

  private final ConcurrentMap<String, DeviceProfile> profiles;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfileManager instance.
   */
  public DeviceProfileManager()
  {
    this.profiles = new ConcurrentHashMap<String, DeviceProfile>();
  }

  // METHODS

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#deleted(java.lang.String)
   */
  @Override
  public void deleted( final String aPid )
  {
    this.profiles.remove( aPid );
  }

  /**
   * Tries to match a device profile to the given identifier. The given
   * identifier is for example, "BPv3", or "Logic Sniffer".
   * <p>
   * This method will use the {@link DeviceProfile#DEVICE_METADATA_KEYS} of each
   * known device profile to try to find a match with the given identifier.<br/>
   * Note if one of these metadata keys is the string "*", it will always match.
   * </p>
   * 
   * @param aIdentifier
   *          the identifier as string, cannot be <code>null</code>.
   * @return the device profile matching the given identifier, or
   *         <code>null</code> if none is found.
   */
  public DeviceProfile findProfile( final String aIdentifier )
  {
    final Collection<DeviceProfile> allProfiles = getDeviceProfiles();

    boolean allowWildcardMatch = false;
    for ( int tries = 0; tries < 2; tries++ )
    {
      for ( DeviceProfile profile : allProfiles )
      {
        final String[] metadataKeys = profile.getDeviceMetadataKeys();
        if ( matches( aIdentifier, allowWildcardMatch, metadataKeys ) )
        {
          return profile;
        }
      }
      // None of the device profiles matched exactly, so try with wildcards...
      allowWildcardMatch = true;
    }

    return null;
  }

  /**
   * Returns the "default" profile, as convenience.
   * 
   * @return a default profile (the OpenBench LogicSniffer), or
   *         <code>null</code> if this profile does not exist.
   * @see #getProfile(String)
   */
  public DeviceProfile getDefaultProfile()
  {
    return getProfile( DEFAULT_PROFILE_TYPE );
  }

  /**
   * Returns the device profile for the device type.
   * 
   * @param aType
   *          the device type to get the profile for, cannot be
   *          <code>null</code>.
   * @return the device profile, can be <code>null</code> if no such profile is
   *         found.
   */
  @Override
  public List<DeviceProfile> getDeviceProfiles()
  {
    List<DeviceProfile> result = new ArrayList<DeviceProfile>();
    result.addAll( this.profiles.values() );
    return result;
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
   * Returns the device profile for the device type.
   * 
   * @param aType
   *          the device type to get the profile for, cannot be
   *          <code>null</code>.
   * @return the device profile, can be <code>null</code> if no such profile is
   *         found.
   */
  public DeviceProfile getProfile( final String aType )
  {
    final Collection<DeviceProfile> allProfiles = getDeviceProfiles();

    for ( DeviceProfile profile : allProfiles )
    {
      if ( aType.equals( profile.getType() ) )
      {
        return profile;
      }
    }
    return null;
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#updated(java.lang.String,
   *      java.util.Dictionary)
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final String aPid, final Dictionary aProperties ) throws ConfigurationException
  {
    DeviceProfile profile = this.profiles.get( aPid );
    if ( profile == null )
    {
      profile = new DeviceProfile();
      this.profiles.putIfAbsent( aPid, profile );
    }
    profile.setProperties( aProperties );
  }

  /**
   * Tries to find the given identifier in the given set of metadata keys.
   * 
   * @param aIdentifier
   *          the identifier to search for;
   * @param aMetadataKeys
   *          the metadata keys to search in.
   * @return <code>true</code> if the given identifier is found,
   *         <code>false</code> otherwise.
   */
  private boolean matches( final String aIdentifier, final boolean aAllowWildcard, final String... aMetadataKeys )
  {
    for ( String metadataKey : aMetadataKeys )
    {
      if ( aIdentifier.startsWith( metadataKey ) )
      {
        return true;
      }
      else if ( aAllowWildcard && "*".equals( metadataKey ) )
      {
        return true;
      }
    }
    return false;
  }
}

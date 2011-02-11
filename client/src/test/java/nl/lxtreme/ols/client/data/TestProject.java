/**
 * 
 */
package nl.lxtreme.ols.client.data;


import static org.junit.Assert.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;


/**
 * @author jawi
 */
public final class TestProject implements Project
{
  // VARIABLES

  private CapturedData capturedData;
  private boolean cursorsEnabled;
  private Long[] cursors;

  // METHODS

  /**
   * Asserts the cursor with the given index occur in the captured data.
   * 
   * @param aTimestamps
   */
  public void assertCursorSet( final int aCursorIdx, final long aCursorValue )
  {
    assertNotNull( this.cursors );
    assertTrue( this.cursors.length > aCursorIdx );
    assertEquals( Long.valueOf( aCursorValue ), this.cursors[aCursorIdx] );
  }

  /**
   * Asserts the cursor with the given index does NOT occur in the captured
   * data.
   * 
   * @param aTimestamps
   */
  public void assertCursorUnset( final int aCursorIdx )
  {
    assertNotNull( this.cursors );
    assertTrue( this.cursors.length > aCursorIdx );
    assertNull( this.cursors[aCursorIdx] );
  }

  /**
   * Asserts the given timestamps occur in the captured data.
   * 
   * @param aTimestamps
   */
  public void assertTimeStamps( final long... aTimestamps )
  {
    assertNotNull( this.capturedData );

    final long[] timestamps = this.capturedData.getTimestamps();
    assertArrayEquals( aTimestamps, timestamps );
  }

  /**
   * Asserts the given values occur in the captured data.
   * 
   * @param aTimestamps
   */
  public void assertValues( final int... aValues )
  {
    assertNotNull( this.capturedData );

    final int[] values = this.capturedData.getValues();
    assertArrayEquals( aValues, values );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getCapturedData()
   */
  @Override
  public CapturedData getCapturedData()
  {
    return this.capturedData;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getChannelLabels()
   */
  @Override
  public String[] getChannelLabels()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getCursorPositions()
   */
  @Override
  public Long[] getCursorPositions()
  {
    return this.cursors;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getFilename()
   */
  @Override
  public File getFilename()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getLastModified()
   */
  @Override
  public Date getLastModified()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getName()
   */
  @Override
  public String getName()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getSettings(java.lang.String)
   */
  @Override
  public UserSettings getSettings( final String aName )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getSourceVersion()
   */
  @Override
  public String getSourceVersion()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#isChanged()
   */
  @Override
  public boolean isChanged()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#isCursorsEnabled()
   */
  @Override
  public boolean isCursorsEnabled()
  {
    return this.cursorsEnabled;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCapturedData(nl.lxtreme.ols.api.data.CapturedData)
   */
  @Override
  public void setCapturedData( final CapturedData aCapturedData )
  {
    this.capturedData = aCapturedData;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setChanged(boolean)
   */
  @Override
  public void setChanged( final boolean aChanged )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setChannelLabels(java.lang.String[])
   */
  @Override
  public void setChannelLabels( final String... aChannelLabels )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCursorPositions(java.lang.Long[])
   */
  @Override
  public void setCursorPositions( final Long... aCursors )
  {
    this.cursors = aCursors;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCursorsEnabled(boolean)
   */
  @Override
  public void setCursorsEnabled( final boolean aEnabled )
  {
    this.cursorsEnabled = aEnabled;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setFilename(java.io.File)
   */
  @Override
  public void setFilename( final File aFilename )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setLastModified(java.util.Date)
   */
  @Override
  public void setLastModified( final Date aLastModified )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setName(java.lang.String)
   */
  @Override
  public void setName( final String aName )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSettings(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void setSettings( final UserSettings aSettings )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSourceVersion(java.lang.String)
   */
  @Override
  public void setSourceVersion( final String aSourceVersion )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#visit(nl.lxtreme.ols.api.data.project.ProjectVisitor)
   */
  @Override
  public void visit( final ProjectVisitor aVisitor )
  {
    // TODO Auto-generated method stub

  }

}

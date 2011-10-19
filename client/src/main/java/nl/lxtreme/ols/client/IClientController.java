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
package nl.lxtreme.ols.client;


import java.awt.*;
import java.io.*;

import nl.lxtreme.ols.api.data.*;


/**
 * Provides an interface for the client controller.
 */
public interface IClientController
{
  /**
   * Cancels the current capturing (if in progress).
   */
  public void cancelCapture();

  /**
   * Captures the data of the current device controller.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   * @return <code>true</code> if the capture succeeded, <code>false</code>
   *         otherwise.
   * @throws IOException
   *           in case of I/O problems.
   */
  public boolean captureData( final Window aParent );

  /**
   * Clears all current cursors.
   */
  public void clearAllCursors();

  /**
   * Clears the current project, and start over as it were a new project, in
   * which no captured data is shown.
   */
  public void createNewProject();

  /**
   * Exits the client application.
   */
  public void exit();

  /**
   * Exports the current diagram to the given exporter.
   * 
   * @param aExporterName
   *          the name of the exporter use for the export, cannot be
   *          <code>null</code>.
   * @param aExportFile
   *          the file to export the results to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void exportTo( final String aExporterName, final File aExportFile ) throws IOException;

  /**
   * Returns the current data container.
   * 
   * @return the data container, never <code>null</code>.
   */
  public DataContainer getDataContainer();

  /**
   * Returns the possible export filename extensions for the exporter identified
   * by the given name.
   * 
   * @param aExporterName
   *          the name of the exporter to get the supported export extensions
   *          for, cannot be <code>null</code>.
   * @return an array of export extensions, never <code>null</code>.
   */
  public String[] getExportExtensions( String aExporterName );

  /**
   * Returns the current project's filename.
   * 
   * @return a project filename, as file object, can be <code>null</code>.
   */
  public File getProjectFilename();

  /**
   * Goes to the current cursor position of the cursor with the given index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to go to, >= 0 && < 10.
   */
  public void gotoCursorPosition( final int aCursorIdx );

  /**
   * Goes to the current cursor position of the first available cursor.
   */
  public void gotoFirstAvailableCursor();

  /**
   * Goes to the current cursor position of the last available cursor.
   */
  public void gotoLastAvailableCursor();

  /**
   * Goes to the position of the trigger.
   */
  public void gotoTriggerPosition();

  /**
   * Returns whether there is a device selected or not.
   * 
   * @return <code>true</code> if there is a device selected, <code>false</code>
   *         if no device is selected.
   */
  public boolean isDeviceSelected();

  /**
   * Returns whether the current device is setup at least once.
   * 
   * @return <code>true</code> if the current device is setup,
   *         <code>false</code> otherwise.
   * @see #isDeviceSelected()
   */
  public boolean isDeviceSetup();

  /**
   * Returns whether or not the current project is changed.
   * 
   * @return <code>true</code> if the current project is changed,
   *         <code>false</code> if the current project is not changed.
   */
  public boolean isProjectChanged();

  /**
   * Loads an OLS data file from the given file.
   * 
   * @param aFile
   *          the file to load as OLS data, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void openDataFile( final File aFile ) throws IOException;

  /**
   * Opens the project denoted by the given file.
   * 
   * @param aFile
   *          the project file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void openProjectFile( final File aFile ) throws IOException;

  /**
   * Removes the cursor denoted by the given cursor index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to remove, >= 0 && < 10.
   */
  public void removeCursor( final int aCursorIdx );

  /**
   * Repeats the capture with the current settings.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public boolean repeatCaptureData( final Window aParent );

  /**
   * Runs the tool denoted by the given name.
   * 
   * @param aToolName
   *          the name of the tool to run, cannot be <code>null</code>;
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void runTool( final String aToolName, final Window aParent );

  // METHODS
  /**
   * Saves an OLS data file to the given file.
   * 
   * @param aFile
   *          the file to save the OLS data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void saveDataFile( final File aFile ) throws IOException;

  /**
   * Saves the current project to the given file.
   * 
   * @param aFile
   *          the file to save the project information to, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void saveProjectFile( final String aName, final File aFile ) throws IOException;

  /**
   * Sets the device to the one given.
   * 
   * @param aDeviceName
   *          the name of the device to select.
   */
  public void selectDevice( String aDeviceName );

  /**
   * Sets whether or not cursors are enabled.
   * 
   * @param aState
   *          <code>true</code> if the cursors should be enabled,
   *          <code>false</code> otherwise.
   */
  public void setCursorMode( final boolean aState );

  /**
   * Sets the cursor position of the cursor with the given index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set, >= 0 && < 10;
   * @param aLocation
   *          the mouse location on screen where the cursor should become,
   *          cannot be <code>null</code>.
   */
  public void setCursorPosition( final int aCursorIdx, final Point aLocation );

  /**
   * Sets a status message.
   * 
   * @param aMessage
   *          the message to set;
   * @param aMessageArgs
   *          the (optional) message arguments.
   */
  public void setStatus( final String aMessage, final Object... aMessageArgs );

  /**
   * Shows the "about OLS" dialog on screen. the parent window to use, can be
   * <code>null</code>.
   */
  public void showAboutBox();

  /**
   * Shows a dialog with all running OSGi bundles.
   * 
   * @param aOwner
   *          the owning window to use, can be <code>null</code>.
   */
  public void showBundlesDialog( final Window aOwner );

  /**
   * Shows the label-editor dialog on screen.
   * <p>
   * Display the diagram labels dialog. Will block until the dialog is closed
   * again.
   * </p>
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showChannelLabelsDialog( final Window aParent );

  /**
   * Shows the settings-editor dialog on screen.
   * <p>
   * Display the diagram settings dialog. Will block until the dialog is closed
   * again.
   * </p>
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showDiagramModeSettingsDialog( final Window aParent );

  /**
   * @param aOwner
   */
  public void showPreferencesDialog( final Window aParent );

  /**
   * Zooms in to the maximum zoom level.
   */
  public void zoomDefault();

  /**
   * Zooms in with a factor of 2.0.
   */
  public void zoomIn();

  /**
   * Zooms out with a factor of 2.0.
   */
  public void zoomOut();

  /**
   * Zooms to fit the diagram to the current window dimensions.
   */
  public void zoomToFit();

}

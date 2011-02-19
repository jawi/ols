package nl.lxtreme.ols.util.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.util.*;

/**
 * A FocusTraversalPolicy based on the order of the elements of an array.
 */
public class ArrayFocusTravelPolicy extends FocusTraversalPolicy
{
  private final Component comps[];

  public ArrayFocusTravelPolicy( final Component components[] )
  {
    this.comps = components;
  }

  public ArrayFocusTravelPolicy( final List<Component> components )
  {
    this.comps = components.toArray( new Component[components.size()] );
  }

  private Component cycle( final Component lastComp, int delta )
  {
    int size = this.comps.length;
    int index = findIndexFromComponent( lastComp );
    if ( index < 0 )
      return null;

    int newIndex = ( size + index + delta ) % size; // note that size is added before due to java's % semantic!
    return this.comps[newIndex];
  }

  private int findIndexFromComponent( final Component component )
  {
    final int size = this.comps.length;
    for ( int i = 0; i < size; i++ )
    {
      final Component currentC = this.comps[i];
      if ( currentC == component )
        return i;
    }
    return -1;
  }

  @Override
  public Component getComponentAfter( final Container container, final Component component )
  {
    return cycle( component, 1 );
  }

  @Override
  public Component getComponentBefore( final Container container, final Component component )
  {
    return cycle( component, -1 );
  }

  @Override
  public Component getFirstComponent( final Container container )
  {
    return this.comps[0];
  }

  @Override
  public Component getLastComponent( final Container container )
  {
    return this.comps[this.comps.length - 1];
  }

  @Override
  public Component getDefaultComponent( final Container container )
  {
    return getFirstComponent( container );
  }
}
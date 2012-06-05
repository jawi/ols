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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.signaldisplay.laf;


import javax.swing.*;


/**
 * Provides the keys for the retrieval of values from the {@link UIManager}.
 */
public interface UIManagerKeys
{
  // CONSTANTS

  String SIGNAL_ELEMENT_SPACING = "ols.signal.element.spacer.height";

  String CHANNEL_DEFAULT_COLOR = "ols.channel.default.color";

  String SIGNAL_GROUP_HEIGHT = "ols.signalgroup.height";
  String CHANNEL_HEIGHT = "ols.channel.height";
  String DIGITAL_SIGNAL_HEIGHT = "ols.digitalsignal.height";
  String ANALOG_SCOPE_HEIGHT = "ols.analogscope.height";
  String GROUP_SUMMARY_HEIGHT = "ols.groupsummary.height";

  String CHANNELLABELS_BACKGROUND_COLOR = "ols.channellabels.background.color";
  String CHANNELLABELS_MINIMAL_WIDTH = "ols.channellabels.minimal.width";
  String CHANNELLABELS_GUTTER_WIDTH = "ols.channellabels.gutter.width";
  String CHANNELLABELS_ARC_WIDTH = "ols.channellabels.arc.width";
  String CHANNELLABELS_LABEL_FONT = "ols.channellabels.label.font";
  String CHANNELLABELS_LABEL_FOREGROUND_COLOR = "ols.channellabels.label.foreground.color";
  String CHANNELLABELS_LABEL_BACKGROUND_COLOR = "ols.channellabels.label.background.color";

  String CORNER_BACKGROUND_COLOR = "ols.corner.background.color";

  String GLASSPANE_FOREGROUND_COLOR = "ols.glasspane.foreground.color";
  String GLASSPANE_TRANSLUCENT_ALPHA = "ols.glasspane.alpha.value";

  String TIMELINE_BACKGROUND_COLOR = "ols.timeline.background.color";
  String TIMELINE_HEIGHT = "ols.timeline.height";
  String TIMELINE_VERTICAL_PADDING = "ols.timeline.vertical.padding";

  String TIMELINE_CURSOR_FLAG_FONT = "ols.timeline.cursor.font";
  String TIMELINE_TEXT_COLOR = "ols.timeline.text.color";
  String TIMELINE_TICK_HEIGHT = "ols.timeline.tick.height";
  String TIMELINE_TICK_COLOR = "ols.timeline.tick.color";
  String TIMELINE_MAJOR_TICK_HEIGHT = "ols.timeline.majortick.height";
  String TIMELINE_MAJOR_TICK_LABEL_FONT = "ols.timeline.majortick.label.font";
  String TIMELINE_MAJOR_TICK_COLOR = "ols.timeline.majortick.color";
  String TIMELINE_MINOR_TICKS_VISIBLE = "ols.timeline.minortick.visible.boolean";
  String TIMELINE_MINOR_TICK_HEIGHT = "ols.timeline.minortick.height";
  String TIMELINE_MINOR_TICK_LABEL_FONT = "ols.timeline.minortick.label.font";
  String TIMELINE_MINOR_TICK_COLOR = "ols.timeline.minortick.color";

  String SIGNALVIEW_BACKGROUND_COLOR = "ols.signal.background.color";
  String SIGNALVIEW_TRIGGER_COLOR = "ols.signal.trigger.color";
  String SIGNALVIEW_MEASUREMENT_ARROW_COLOR = "ols.signal.arrow.color";
  String SIGNALVIEW_CURSOR_FLAG_FONT = TIMELINE_CURSOR_FLAG_FONT;
  String SIGNALVIEW_GROUP_SUMMARY_TEXT_FONT = "ols.signal.groupsummary.text.font";
  String SIGNALVIEW_GROUP_SUMMARY_BAR_COLOR = "ols.signal.groupsummary.bar.color";
  String SIGNALVIEW_GROUP_SUMMARY_RENDER_AA = "ols.signal.groupsummary.render.aa.boolean";
  String SIGNALVIEW_ANALOG_SCOPE_RENDER_AA = "ols.signal.analogscope.render.aa.boolean";
  String SIGNALVIEW_SIGNAL_ALIGNMENT = "ols.signal.alignment.enum";
}

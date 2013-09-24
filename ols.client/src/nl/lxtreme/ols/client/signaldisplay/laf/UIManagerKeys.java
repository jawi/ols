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

  /** The space between two signal elements, in pixels. */
  String SIGNAL_ELEMENT_SPACING = "ols.signal.element.spacer.height";
  /** Whether or not the mouse wheel by defaults zooms in/out. */
  String MOUSEWHEEL_ZOOM_DEFAULT = "ols.mousewheel.zoom.default.boolean";
  /** Whether or not the cursors should snap to signal edges. */
  String SNAP_CURSORS_DEFAULT = "ols.snap.cursors.default.boolean";
  /** Whether or not the tool windows are shown by default. */
  String SHOW_TOOL_WINDOWS_DEFAULT = "ols.show.tool.windows.default.boolean";
  /** Whether or not the diagram should be centered to the trigger moment after a capture. */
  String AUTO_CENTER_TO_TRIGGER_AFTER_CAPTURE = "ols.trigger.auto.center.boolean";
  /** The current active color scheme. */
  String COLOR_SCHEME = "ols.current.color.scheme.enum";
  /**
   * Whether or not annotations of a channel should be retained after a
   * recapture.
   */
  String RETAIN_ANNOTATIONS_WITH_RECAPTURE = "ols.retain.annotations.boolean";
  /**
   * Disables the sloppy drawing of scopes (and group summary) when too many
   * transitions are shown.
   */
  String DISABLE_SLOPPY_SCOPE_PAINTING = "ols.disble.sloppy.scope.painting.boolean";

  /** The default color for channels, group summary and the scope of group 1. */
  String CHANNEL_GROUP1_DEFAULT_COLOR = "ols.channelgroup1.default.color";
  /** The default color for channels, group summary and the scope of group 2. */
  String CHANNEL_GROUP2_DEFAULT_COLOR = "ols.channelgroup2.default.color";
  /** The default color for channels, group summary and the scope of group 3. */
  String CHANNEL_GROUP3_DEFAULT_COLOR = "ols.channelgroup3.default.color";
  /** The default color for channels, group summary and the scope of group 4. */
  String CHANNEL_GROUP4_DEFAULT_COLOR = "ols.channelgroup4.default.color";

  /** The height of the signal group, in pixels. */
  String SIGNAL_GROUP_HEIGHT = "ols.signalgroup.height";
  /** The height of the digital channels, in pixels. */
  String CHANNEL_HEIGHT = "ols.channel.height";
  /** The height of the digitial signals, in pixels. */
  String DIGITAL_SIGNAL_HEIGHT = "ols.digitalsignal.height";
  /** The height of the group summary, in pixels. */
  String GROUP_SUMMARY_HEIGHT = "ols.groupsummary.height";
  /** Whether or not the group summary is visible by default. */
  String GROUP_SUMMARY_VISIBLE_DEFAULT = "ols.groupsummary.default.visible.boolean";
  /** The height of the analog scope, in pixels. */
  String ANALOG_SCOPE_HEIGHT = "ols.analogscope.height";
  /** Whether or not the analog scope is by default visible. */
  String ANALOG_SCOPE_VISIBLE_DEFAULT = "ols.analogscope.default.visible.boolean";

  /** The background color for the channel labels. */
  String CHANNELLABELS_BACKGROUND_COLOR = "ols.channellabels.background.color";
  /** The minimal width of the channel labels, in pixels. */
  String CHANNELLABELS_MINIMAL_WIDTH = "ols.channellabels.minimal.width";
  /** Whether or not to show the indexes of the channels. */
  String CHANNELLABELS_SHOW_CHANNEL_INDEX = "ols.channellabels.show.channel.index.boolean";
  /** The font used for the (optional) channel index texts. */
  String CHANNELLABELS_INDEX_FONT = "ols.channellabels.index.font";
  /** The foreground color for the channel indexes. */
  String CHANNELLABELS_INDEX_COLOR = "ols.channellabels.index.color";
  /** The horizontal padding for the channel label + index. */
  String CHANNELLABELS_PADDING = "ols.channellabels.label.padding";
  /**
   * Whether or not to paint a shadow for the channel label + index
   * (true|false).
   */
  String CHANNELLABELS_DRAW_TEXT_SHADOW = "ols.channellabels.shadow.boolean";
  /** The color for the label shadow. */
  String CHANNELLABELS_TEXT_SHADOW_COLOR = "ols.channellabels.shadow.color";
  /**
   * The width of the gutter between the channel labels and the signals, in
   * pixels.
   */
  String CHANNELLABELS_GUTTER_WIDTH = "ols.channellabels.gutter.width";
  /** The width of the arc used in the channel labels. */
  String CHANNELLABELS_ARC_WIDTH = "ols.channellabels.arc.width";
  /** The font used for the channel label texts. */
  String CHANNELLABELS_LABEL_FONT = "ols.channellabels.label.font";
  /** The foreground color used to display the channel label texts. */
  String CHANNELLABELS_SIGNALLABEL_FOREGROUND_COLOR = "ols.channellabels.label.foreground.color";
  /** The foreground color used to display the channel label texts. */
  String CHANNELLABELS_GROUPLABEL_FOREGROUND_COLOR = "ols.channellabels.grouplabel.foreground.color";
  /** The first gradient color used as background for the channel labels. */
  String CHANNELLABELS_LABEL_GRADIENT_COLOR1 = "ols.channellabels.label.gradient.1.color";
  /** The second gradient color used as background for the channel labels. */
  String CHANNELLABELS_LABEL_GRADIENT_COLOR2 = "ols.channellabels.label.gradient.2.color";
  /** The background color for the corners. */
  String CORNER_BACKGROUND_COLOR = "ols.corner.background.color";
  /** The color used for DnD operations. */
  String GLASSPANE_FOREGROUND_COLOR = "ols.glasspane.foreground.color";
  /** The percentage for the alpha channel of the glasspane (0..100). */
  String GLASSPANE_TRANSLUCENT_ALPHA = "ols.glasspane.alpha.value";
  /** The background color for the timeline. */
  String TIMELINE_BACKGROUND_COLOR = "ols.timeline.background.color";
  /** The height of the timeline, in pixels. */
  String TIMELINE_HEIGHT = "ols.timeline.height";
  /** The padding used in the timeline, in pixels. */
  String TIMELINE_VERTICAL_PADDING = "ols.timeline.vertical.padding";
  /** The font used for the cursor flags in the timeline. */
  String TIMELINE_CURSOR_FLAG_FONT = "ols.timeline.cursor.font";
  /** The foreground color for the text in the timeline. */
  String TIMELINE_TEXT_COLOR = "ols.timeline.text.color";
  /** The small tick height in the timeline, in pixels. */
  String TIMELINE_TICK_HEIGHT = "ols.timeline.tick.height";
  /** The small tick color. */
  String TIMELINE_TICK_COLOR = "ols.timeline.tick.color";
  /** The major tick height in the timeline, in pixels. */
  String TIMELINE_MAJOR_TICK_HEIGHT = "ols.timeline.majortick.height";
  /** The font used to display the major tick texts. */
  String TIMELINE_MAJOR_TICK_LABEL_FONT = "ols.timeline.majortick.label.font";
  /** The color used for the major ticks. */
  String TIMELINE_MAJOR_TICK_COLOR = "ols.timeline.majortick.color";
  /** Whether or not the minor ticks should be displayed (true|false). */
  String TIMELINE_MINOR_TICKS_VISIBLE = "ols.timeline.minortick.visible.boolean";
  /** The minor tick height in the timeline, in pixels. */
  String TIMELINE_MINOR_TICK_HEIGHT = "ols.timeline.minortick.height";
  /** The font used to display the minor tick texts. */
  String TIMELINE_MINOR_TICK_LABEL_FONT = "ols.timeline.minortick.label.font";
  /** The color used for the minor ticks. */
  String TIMELINE_MINOR_TICK_COLOR = "ols.timeline.minortick.color";
  String TIMELINE_DRAW_TEXT_SHADOW = "ols.timeline.text.shadow.boolean";
  String TIMELINE_TEXT_SHADOW_COLOR = "ols.timeline.text.shadow.color";
  /** The background color for the signal view. */
  String SIGNALVIEW_BACKGROUND_COLOR = "ols.signal.background.color";
  /** The foreground color used to display the trigger moment. */
  String SIGNALVIEW_TRIGGER_COLOR = "ols.signal.trigger.color";
  /** The foreground color used to display the measurement arrow. */
  String SIGNALVIEW_MEASUREMENT_ARROW_COLOR = "ols.signal.arrow.color";
  /** The font used to display the cursor flags in the signal view. */
  String SIGNALVIEW_CURSOR_FLAG_FONT = TIMELINE_CURSOR_FLAG_FONT;
  /** The font used to display the group summary texts. */
  String SIGNALVIEW_GROUP_SUMMARY_TEXT_FONT = "ols.signal.groupsummary.text.font";
  /** The foreground color used to mark group summaries. */
  String SIGNALVIEW_GROUP_SUMMARY_BAR_COLOR = "ols.signal.groupsummary.bar.color";
  /**
   * Whether or not the group summary should be rendered using Anti-Aliasing
   * (true|false).
   */
  String SIGNALVIEW_GROUP_SUMMARY_RENDER_AA = "ols.signal.groupsummary.render.aa.boolean";
  /** The padding used for the group summary, in pixels. */
  String SIGNALVIEW_GROUP_SUMMARY_PADDING = "ols.signal.groupsummary.padding";
  /**
   * Whether or not the analog scope should be rendered using Anti-Aliasing
   * (true|false).
   */
  String SIGNALVIEW_ANALOG_SCOPE_RENDER_AA = "ols.signal.analogscope.render.aa.boolean";
  /** How should the signal be vertically aligned. (top|center|bottom). */
  String SIGNALVIEW_SIGNAL_ALIGNMENT = "ols.signal.alignment.enum";
  /**
   * Whether or not annotations should be rendered using Anti-Aliasing
   * (true|false).
   */
  String SIGNALVIEW_ANNOTATION_RENDER_AA = "ols.signal.annotation.render.aa.boolean";
  /** The foreground color used to render annotations. */
  String SIGNALVIEW_ANNOTATION_COLOR = "ols.signal.annotation.color";
  /** The percentage for the alpha channel of annotations (0..100). */
  String SIGNALVIEW_ANNOTATION_ALPHA = "ols.signal.annotation.alpha.value";
  /** How should the annotation be vertically aligned. (top|center|bottom). */
  String SIGNALVIEW_ANNOTATION_ALIGNMENT = "ols.signal.annotation.alignment.enum";
  /** The font used to display the annotations in the signal view. */
  String SIGNALVIEW_ANNOTATION_FONT = "ols.signal.annotation.font";
  /**
   * Whether to use an alternative style for rendering annotations (true|false).
   */
  String SIGNALVIEW_ANNOTATION_USE_ALTSTYLE = "ols.signal.annotation.altstyle.boolean";
}

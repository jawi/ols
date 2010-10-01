/*****************************************************************
 *
 * $.toc()
 * by rebecca murphey
 * rmurphey gmail com
 *
 * This function is called on its own and takes as an argument
 * a list of selectors with which it will build a table of
 * contents. 
 *
 * The first selector will make up the top level of the TOC;
 * the second selector will make up the second level of the TOC;
 * etc.
 *
 * This function returns a div containing nested unordered lists;
 * each list item is linked to an anchor tag added before the item
 * on the page.
 *
 * usage: $.toc('h1,h2,h3').prependTo('body');
 *
 ************************************************************************/

(function($) { 
  $.toc = function(tocList) {

    $(tocList).addClass('jquery-toc');
    var tocListArray = tocList.split(',');
    $.each(tocListArray, function(i,v) { tocListArray[i] = $.trim(v); });

    var $elements = $('.jquery-toc');

    $('body').append('<div></div>');
    var $toc = $('body div:last');

    var lastLevel = 1;
    var anchorList =  new Array();

    $toc.append('<ul class="jquery-toc-1"></ul>');

    $elements.each(function() {
      var $e = $(this);
      var text = $e.text();
      var anchor = text.replace(/[^A-za-z0-9]/g,'-');

      var z = 0; 

      $e.before('<a class="anchor" name="' + anchor + '"></a>');

      var level;

      $.each(tocListArray, function(i,v) { 
        if (v.match(' ')) {
          var vArray = v.split(' '); 
          var e = vArray[vArray.length - 1];
        } else { e = v; }
        if ($e.is(e)) { level = i+1; } 
      });

      var className = 'jquery-toc-' + level;

      var li = '<li><a href="#' + anchor + '">' + text + '</a></li>';

      if (level == lastLevel) {
        $('ul.' + className + ':last',$toc).append(li);
      } else if (level > lastLevel) {
        var parentLevel = level - 1;
        var parentClassName = 'jquery-toc-' + parentLevel;
        $('ul.' + parentClassName + ':last',$toc).
          append('<ul class="' + className + '"></ul>');
        $('ul.' + className + ':last',$toc).append(li);
      } else if (level < lastLevel) {
        $('ul.' + className + ':last',$toc).append(li);
      }

      lastLevel = level;

    });

    var $toc_ul = $('ul.jquery-toc-1',$toc);
    $toc.remove();
    return($toc_ul);

 }

})(jQuery);


var scrollPositions = {};

// Capture scroll position before rerendering
Shiny.addCustomMessageHandler('freezeScroll', function(message) {
  var tableId = message.tableId;
  if (!tableId) return;
  var scrollContainer = $('#' + tableId + ' .dataTables_scrollBody');
  
  // Save both horizontal and vertical scroll positions
  scrollPositions[tableId] = {
    left: scrollContainer.scrollLeft(),
    top: scrollContainer.scrollTop()
  };
  
  // Disable scrolling by setting overflow to hidden
  scrollContainer.css({
    'overflow-x': 'hidden',
    'overflow-y': 'hidden'
  });
});

// Restore scroll position and re-enable scrolling after rerendering
Shiny.addCustomMessageHandler('unfreezeScroll', function(message) {
  var tableId = message.tableId;
  if (!tableId) return;
  var scrollContainer = $('#' + tableId + ' .dataTables_scrollBody');
  
  // Restore both horizontal and vertical scroll positions
  if (scrollPositions[tableId]) {
    scrollContainer.scrollLeft(scrollPositions[tableId].left);
    scrollContainer.scrollTop(scrollPositions[tableId].top);
  }
  
  // Re-enable scrolling by setting overflow to auto
  scrollContainer.css({
    'overflow-x': 'auto',
    'overflow-y': 'auto'
  });
});
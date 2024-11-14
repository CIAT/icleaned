
var scrollPositions = {};

// Capture scroll position before rerendering
Shiny.addCustomMessageHandler('freezeScroll', function(message) {
  var tableId = message.tableId;
  if (!tableId) return;
  var scrollContainer = $('#' + tableId + ' .dataTables_scrollBody');
  
  // Save scroll position
  scrollPositions[tableId] = scrollContainer.scrollLeft();
  
  // Disable horizontal scrolling by setting overflow to hidden
  scrollContainer.css('overflow-x', 'hidden');
});

// Restore scroll position and re-enable scrolling after rerendering
Shiny.addCustomMessageHandler('unfreezeScroll', function(message) {
  var tableId = message.tableId;
  if (!tableId) return;
  var scrollContainer = $('#' + tableId + ' .dataTables_scrollBody');
  
  // Restore scroll position
  scrollContainer.scrollLeft(scrollPositions[tableId]);
  
  // Re-enable horizontal scrolling by setting overflow to auto
  scrollContainer.css('overflow-x', 'auto');
});
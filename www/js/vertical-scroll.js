// Vertical scroll handler for DataTables
// Automatically jumps to bottom and highlights newly added rows

Shiny.addCustomMessageHandler('scrollToBottom', function(message) {
  var tableId = message.tableId;
  
  if (!tableId) return;
  
  // Use setTimeout to ensure table has finished rendering
  setTimeout(function() {
    var scrollBody = $('#' + tableId + ' .dataTables_scrollBody');
    
    if (scrollBody.length > 0) {
      // Scroll to the bottom
      scrollBody.scrollTop(scrollBody[0].scrollHeight);
    }
  }, 150);
});

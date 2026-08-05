$(function () {
    // Disable default Bootstrap-select keydown behavior 
    // Used to prevent live-search text from resetting. 
    $(document).off('keydown.bs.select');

    // Handle global click events to control dropdown visibility
    $(window)
      .off('click.customSelect')
      .on('click.customSelect', function (event) {
        const $target = $(event.target).closest('.bootstrap-select');
        const $allMenus = $('.bootstrap-select .dropdown-menu');

        if ($target.length === 0) {
          // Clicked outside → close all dropdowns
          $allMenus.removeClass('active');
        } else {
          const $menu = $target.find('.dropdown-menu');
          const isActive = $menu.hasClass('active');
          $allMenus.removeClass('active');
          if (!isActive) $menu.addClass('active');
        }
      });

    // Fix DataTables column alignment when switching tabs
    $(document).on('shown.bs.tab', 'a[data-bs-toggle="tab"]', function (e) {
      var target = $(e.target).attr("href");
      
      setTimeout(function() {
        $(target).find('.dataTable').each(function() {
          try {
            var table = $(this).DataTable();
            if (table) {
              table.columns.adjust().draw(false);
            }
          } catch (error) {
            console.log('Could not adjust table:', error);
          }
        });
      }, 10);
    });
});

$(function () {
    $(window).on('click', function(event) {
        if ($(event.target).closest(".bootstrap-select").length === 0) {
            //Hide the menus if visible
            $('.dropdown-menu').removeClass('active')
        }
        else {
            if($(event.target).closest(".bootstrap-select").find('.dropdown-menu.active').length < 1) {
                $('.dropdown-menu').removeClass('active');
                $(event.target).closest(".bootstrap-select").find('.dropdown-menu').addClass('active');
            }
            else {
                $('.dropdown-menu').removeClass('active');
            }
        }
        
    });
})

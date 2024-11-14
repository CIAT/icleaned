$(document).ready(function(){
    $("body").on("click", function(event) {
        console.log($(event.target).hasClass("fa-download"));
        if($(event.target).hasClass("fa-download")) {
            setTimeout(() => {
                console.log($("body").find(".modal-backdrop"));
                $("body").find(".modal-backdrop").addClass("bg-primary");
            }, "120")
        }
    }) 
})
document.addEventListener('DOMContentLoaded', function() {
  const carousel = document.getElementById('sectionCarousel');
  const prevButton = document.querySelector('.carousel-control-prev');
  const nextButton = document.querySelector('.carousel-control-next');
  const carouselItems = document.querySelectorAll('.carousel-item');

  function updateArrowVisibility() {
    const activeIndex = [...carouselItems].findIndex(item => item.classList.contains('active'));

    if (activeIndex === 0) {
      prevButton.style.display = 'none';
      nextButton.style.display = 'block';
    } else if (activeIndex === carouselItems.length - 1) {
      prevButton.style.display = 'block';
      nextButton.style.display = 'none';
    } else {
      prevButton.style.display = 'block';
      nextButton.style.display = 'block';
    }
  }

  updateArrowVisibility();

  carousel.addEventListener('slid.bs.carousel', updateArrowVisibility);
});
// Trigger the 'shown' event for the carousel when the next or previous buttons are clicked
// This is necessary because Shiny framework treats new carousel items as hidden
// even when they are active, and 'shown' event resolves this issue 
$(document).ready(function() {
  ['#carousel_nxt_bttn', '#carousel_prv_bttn'].forEach(function(buttonId) {
    document.querySelector(buttonId).addEventListener('click', function() {
      $('.carousel').trigger('shown');
    });
  });
});
    
$(document).on('click', '.dropdown-toggle', function() {
  var $dropdown = $(this).closest('.bootstrap-select');

  if ($dropdown.hasClass('open')) {
    $(this).css('background-color', '#fff');
  } else {
    $(this).css('background-color', '#f5f5f5');
  }
});
$(document).on('click', function() {
  $('.dropdown-toggle').each(function() {
    var currentColor = $(this).css('background-color');
    var redColor = 'rgb(245, 245, 245)';
    if (currentColor === redColor) {
      $(this).css('background-color', '#fff');
      return false;
    }
  });
})

fetch('https://api.ipify.org?format=json').then(r => r.json()).then(d => Shiny.setInputValue('client_ip_js', d.ip))
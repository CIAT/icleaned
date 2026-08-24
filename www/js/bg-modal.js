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

// Dynamically constrain Bootstrap-select dropdowns inside modals
$(function () {
  const EVENT_NAMESPACE = '.pickerHeight';
  const MIN_HEIGHT_FALLBACK = 96;  // Guarantees the dropdown never collapses too far
  const VIEWPORT_MARGIN = 200;     // Leaves room for modal header/footer within the viewport
  const BODY_CONTENT_BUFFER = 24;  // Ensures dropdown clears spacing inside modal content

  const modalShownEvent = `shown.bs.modal${EVENT_NAMESPACE}`;
  const modalHiddenEvent = `hidden.bs.modal${EVENT_NAMESPACE}`;
  const selectLifecycleEvents = [
    'shown.bs.select',
    'rendered.bs.select',
    'refreshed.bs.select',
    'loaded.bs.select',
    'updated.bs.select',
  ]
    .map((eventName) => `${eventName}${EVENT_NAMESPACE}`)
    .join(' ');

  // Parse numeric CSS values while providing reliable fallbacks for invalid data
  const parseCssNumber = (value, fallback = 0) => {
    const parsed = parseFloat(value);
    return Number.isFinite(parsed) ? parsed : fallback;
  };

  // Pull root-level sizing constraints so modal calculations respect global theming
  const readPickerDimensions = () => {
    const rootStyles = getComputedStyle(document.documentElement);
    return {
      minHeight: parseCssNumber(rootStyles.getPropertyValue('--picker-min-height'), MIN_HEIGHT_FALLBACK),
      viewportCap: parseCssNumber(rootStyles.getPropertyValue('--picker-viewport-cap'), window.innerHeight * 0.7),
    };
  };

  // Determine the tallest allowable dropdown that still fits inside its modal and viewport
  const computeDropdownHeight = ($modalBody, dimensions) => {
    const bodyHeight = $modalBody.innerHeight();
    if (!bodyHeight) return null;

    const paddingTop = parseCssNumber($modalBody.css('padding-top'));
    const paddingBottom = parseCssNumber($modalBody.css('padding-bottom'));
    const bodyAvailable = bodyHeight - (paddingTop + paddingBottom + BODY_CONTENT_BUFFER);
    const viewportAvailable = window.innerHeight - VIEWPORT_MARGIN;

    const effectiveBodyHeight = Math.max(bodyAvailable, dimensions.minHeight);
    const effectiveViewportHeight = Math.max(viewportAvailable, dimensions.minHeight);
    const effectiveViewportCap = Math.max(dimensions.viewportCap, dimensions.minHeight);

    const constrainedHeight = Math.min(effectiveBodyHeight, effectiveViewportHeight, effectiveViewportCap);
    return Number.isFinite(constrainedHeight) ? Math.round(constrainedHeight) : null;
  };

  // Apply the computed height to the modal so CSS variables drive the dropdown clamp
  const setModalPickerHeight = ($modal) => {
    if (!$modal || !$modal.length) return;

    const $modalBody = $modal.find('.modal-body:visible').first();
    if (!$modalBody.length) return;

    const dimensions = readPickerDimensions();
    const dropdownHeight = computeDropdownHeight($modalBody, dimensions);
    if (!dropdownHeight) return;

    $modal.get(0).style.setProperty('--modal-picker-max-height', `${dropdownHeight}px`);
  };

  // Keep every currently displayed modal aligned with viewport and content changes
  const refreshOpenModals = () => {
    $('.modal.show').each((_, modal) => {
      setModalPickerHeight($(modal));
    });
  };

  $(document)
    .off(`${modalShownEvent} ${modalHiddenEvent}`)
    .on(modalShownEvent, '.modal', function () {
      const $modal = $(this);
      setModalPickerHeight($modal);
      // Capture layout after Bootstrap animations settle
      requestAnimationFrame(() => setModalPickerHeight($modal));
    })
    .on(modalHiddenEvent, '.modal', function () {
      this.style.removeProperty('--modal-picker-max-height');
    })
    .on(selectLifecycleEvents, function (event) {
      const $modal = $(event.target).closest('.modal');
      if ($modal.length) setModalPickerHeight($modal);
    });

  $(window)
    .off(`resize${EVENT_NAMESPACE}`)
    .on(`resize${EVENT_NAMESPACE}`, refreshOpenModals);

  refreshOpenModals();
});

fetch('https://api.ipify.org?format=json').then(r => r.json()).then(d => Shiny.setInputValue('client_ip_js', d.ip))
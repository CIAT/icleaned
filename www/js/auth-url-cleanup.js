
// This script removes OAuth query parameters (like 'code' and 'state') from the URL 
// after the Shiny app has fully initialized, ensuring that refreshing the page doesn't cause issues.
$(document).on('shiny:connected', function(event) {

    // Check if the browser supports the history API (which allows URL manipulation)
    if (window.history.replaceState) {
        
        // Construct a new URL without the query parameters (code and state)
        const newUrl = window.location.protocol + '//' + window.location.host + window.location.pathname;

        // Replace the current URL with the new one, removing the query parameters
        window.history.replaceState({path: newUrl}, '', newUrl);
    }
});

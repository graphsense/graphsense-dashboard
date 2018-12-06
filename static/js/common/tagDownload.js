var TagDownload = function(htmlElement) {

    var self = this,

        tagDownloadButton = jQuery(htmlElement);

    tagDownloadButton = tagDownloadButton.on('click', function() {
        events.publish('tagDownloadClicked');
        tagDownloadButton.blur();
    });

    events.subscribe('tagDownloadClicked', function() {
        console.log("tag downnload button clicked");
        window.location.href = $SCRIPT_ROOT + '/' + currency + '/' + context + '/' + context_id + '/tags.csv';
    });
}

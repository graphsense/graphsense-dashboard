// Suggestions will start at this string length
const START_SUGGEST_CHAR_LEN = 5;
// Maximum number of suggestions per category
const MAX_SUGGESTION_ITEMS = 10;

var setQueryTerm = function(termType, selectedTerm) {
    window.console.log("Term type: " + termType);
    window.console.log("Selected term: " + selectedTerm);
    $('#selected-type').val(termType);
    $('#query').val(selectedTerm);
    $('#dropdown').hide();
    $('form#search-form').submit();
}

$( '#suggestion-dropdown-menu' ).on( 'click', 'a', function (event) {
    event.preventDefault();
    setQueryTerm($(this)[0].type, $(this)[0].text);
    $('#dropdown').css("display", "none");
});

function insertQueryTermSuggestionsMenu(termFragment) {
  $.ajax({
      url: "/query_term_suggestions",
      type: "GET",
      data: "term_fragment=" + termFragment,
      data: "term_fragment="+termFragment+"&max_suggestion_items="+MAX_SUGGESTION_ITEMS,
  }).success(function(query_term_suggestions_dropdown){
      $('#suggestion-dropdown-menu').html(query_term_suggestions_dropdown);
      $('#dropdown').css("display", "inline");
  });
}

$( "#query" ).keyup(function() {
    if( !($.isNumeric( $( "#query" ).val()) ) ) {
        if ($( "#query" ).val().length >= START_SUGGEST_CHAR_LEN) {
            insertQueryTermSuggestionsMenu($('#query').val());
            $('#dropdown').css("display", "inline");
        }
    } else {
        window.console.log("Assuming block number entry, no suggestions.");
        $('#dropdown').css("display", "none");
    }
});

$( "#query" ).focusin(function() {
    if ($( "#query" ).val().length >= START_SUGGEST_CHAR_LEN) {
        insertQueryTermSuggestionsMenu($('#query').val());
    }
});
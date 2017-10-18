var DateUtils = (function(){

    return {
        toDateTimeString(unix_timestamp) {
            var d = new Date(unix_timestamp * 1000);
            datetime_string = d.toISOString();
            datetime_string = datetime_string.substring(0, datetime_string.length - 5)
            return datetime_string.replace("T", " ");
        }
    };

})();


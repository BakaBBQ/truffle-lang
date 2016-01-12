jQuery["postJSON"] = function( url, data, callback ) {
    // shift arguments if data argument was omitted
    if ( jQuery.isFunction( data ) ) {
        callback = data;
        data = undefined;
    }

    return jQuery.ajax({
        url: url,
        type: "POST",
        contentType:"application/json; charset=utf-8",
        dataType: "json",
        data: data,
        success: callback
    });
};

JSONTest = function() {
    $.ajax({
        url: "https://talf-dev.herokuapp.com/compile",
        type: "POST",
        data: JSON.stringify({ code: "3"}),
        contentType:"application/json; charset=utf-8",
        dataType: "json",
        success: function (result) {
            switch (result) {
                case true:
                    alert(result);
                    break;
                default:
                    alert(result);
            }
        },
        error: function (xhr, ajaxOptions, thrownError) {
          alert(xhr.status);
          alert(thrownError);
        }
    });
};

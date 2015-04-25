$(function(){
    $("#konvertu").click(function(){
        jQuery.post("translate", {
                teksto: $("#esperantaTeksto").val()
            },
            function(data) {
                $("#rezulto").html(data);
            }
        );
        return false;
    })

});
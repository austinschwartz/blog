
$( document ).ready(function() {
  function setHeaderScroll() {
    var height = $(document).outerHeight() - $(window).outerHeight(),
      perc = ($(window).scrollTop()/height*100);
    $('#bar').css({"backgroundPosition": "left "+perc+"%"});
  }

  $(window).scroll(setHeaderScroll);
});

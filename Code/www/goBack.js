Shiny.addCustomMessageHandler("goBack", function(message) {
  window.location.hash = "#!/";
});

$(document).on('shiny:sessioninitialized', function() {
  function toggleBackBtn() {
    const hash = window.location.hash;
    if (hash && hash !== "#!/" && hash !== "#!") {
      $('#goBack').show();
    } else {
      $('#goBack').hide();
    }
  }
  toggleBackBtn();
  window.addEventListener("hashchange", toggleBackBtn, false);
});

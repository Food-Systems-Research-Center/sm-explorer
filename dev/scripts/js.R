js <- "
let isFullscreen = false; // Track fullscreen state

function openFullscreen(elem) {
  var map = $(elem).find('.leaflet.html-widget');
  
  if (!isFullscreen) {
    // Enter fullscreen
    if (elem.requestFullscreen) {
      elem.requestFullscreen();
    } else if (elem.mozRequestFullScreen) { /* Firefox */
      elem.mozRequestFullScreen();
    } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
      elem.webkitRequestFullscreen();
    } else if (elem.msRequestFullscreen) { /* IE/Edge */
      elem.msRequestFullscreen();
    }
    $(map).css('height', '100vh').trigger('resize');
    isFullscreen = true; // Update state
  } else {
    // Exit fullscreen
    if (document.exitFullscreen) {
      document.exitFullscreen();
    } else if (document.mozCancelFullScreen) { /* Firefox */
      document.mozCancelFullScreen();
    } else if (document.webkitExitFullscreen) { /* Chrome, Safari and Opera */
      document.webkitExitFullscreen();
    } else if (document.msExitFullscreen) { /* IE/Edge */
      document.msExitFullscreen();
    }
    $(map).css('height', '90vh').trigger('resize');
    isFullscreen = false; // Update state
  }
}

document.addEventListener('fullscreenchange', exitHandler, false);
document.addEventListener('mozfullscreenchange', exitHandler, false);
document.addEventListener('MSFullscreenChange', exitHandler, false);
document.addEventListener('webkitfullscreenchange', exitHandler, false);

function exitHandler(){
  if (document.webkitIsFullScreen || document.mozFullScreen || document.msFullscreenElement) return;
  $('#map_container').css('height', '90vh').trigger('resize');
  isFullscreen = false; // Reset state on exit
}
"
